import numpy as np
from DLfeaturemodel import resnet50
import torch
import torch.nn as nn
from densenet3D import DenseNet3D
from vit3D import ViT
import torch.nn.functional as F

class AttentionFusion(nn.Module):
    def __init__(self, input_dim, hidden_dim):
        super(AttentionFusion, self).__init__()
        # Attention layers
        self.attention = nn.Sequential(
            nn.Linear(input_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, 2),
            nn.Softmax(dim=1)
        )

    def forward(self, feat1, feat2):
        combined_feat = torch.cat((feat1, feat2), dim=1)
        weights = self.attention(combined_feat)
        feat_fused = weights[:, 0:1] * feat1 + weights[:, 1:2] * feat2
        return feat_fused

class CrossAttention(nn.Module):
    """
    交叉注意力模块
    Args:
        query_dim: 查询向量维度
        key_dim: 键向量维度
        value_dim: 值向量维度
        embed_dim: 嵌入维度
        num_heads: 注意力头数
    """
    def __init__(self, query_dim, key_dim, value_dim, embed_dim, num_heads=4):
        super(CrossAttention, self).__init__()
        self.embed_dim = embed_dim
        self.num_heads = num_heads
        self.head_dim = embed_dim // num_heads
        # 线性投影层
        self.query_proj = nn.Linear(query_dim, embed_dim)
        self.key_proj = nn.Linear(key_dim, embed_dim)
        self.value_proj = nn.Linear(value_dim, embed_dim)  
        # 输出层
        self.out_proj = nn.Linear(embed_dim, embed_dim)
        
    def forward(self, query, key, value):
        """
        Args:
            query: [batch_size, query_dim]
            key: [batch_size, key_dim]
            value: [batch_size, value_dim]
        Returns:
            [batch_size, embed_dim]
        """
        batch_size = query.size(0)  
        # 投影到嵌入空间
        Q = self.query_proj(query).view(batch_size, -1, self.num_heads, self.head_dim).permute(0, 2, 1, 3)
        K = self.key_proj(key).view(batch_size, -1, self.num_heads, self.head_dim).permute(0, 2, 1, 3)
        V = self.value_proj(value).view(batch_size, -1, self.num_heads, self.head_dim).permute(0, 2, 1, 3)
        
        # 计算注意力分数
        attn_scores = torch.matmul(Q, K.transpose(-2, -1)) / torch.sqrt(torch.tensor(self.head_dim, dtype=torch.float32))
        attn_weights = F.softmax(attn_scores, dim=-1)    
        # 注意力加权和
        attn_output = torch.matmul(attn_weights, V)
        attn_output = attn_output.permute(0, 2, 1, 3).contiguous().view(batch_size, -1, self.embed_dim)    
        # 输出投影
        output = self.out_proj(attn_output.squeeze(1))
        return output


class ImageFusionModel(nn.Module):
    def __init__(self, img_encoder = 'resnet50', device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')):
        super(ImageFusionModel, self).__init__()
        self.img_encoder = img_encoder
        if img_encoder == 'resnet50':
            self.DWI_ImgEncoder = resnet50().to(device)
            self.T2_ImgEncoder = resnet50().to(device)
            input_dim = 2048
        elif img_encoder == 'vit':
            self.DWI_ImgEncoder = ViT(
                image_size = 224,          # image size
                frames = 12,               # number of frames
                image_patch_size = 16,     # image patch size
                frame_patch_size = 2,      # frame patch size
                num_classes=1024,
                mlp_dim = 2048,                                
                dim = 1024,
                depth = 6,
                heads = 8,
                dropout = 0.1,
                emb_dropout = 0.1
            )
            self.T2_ImgEncoder = ViT(
                image_size = 224,          # image size
                frames = 12,               # number of frames
                image_patch_size = 16,     # image patch size
                frame_patch_size = 2,      # frame patch size
                num_classes=1024,
                mlp_dim = 2048,                
                dim = 1024,
                depth = 6,
                heads = 8,
                dropout = 0.1,
                emb_dropout = 0.1
            )
            input_dim = 1024
        elif img_encoder == 'dense3D':        
            self.DWI_ImgEncoder = DenseNet3D()
            self.T2_ImgEncoder = DenseNet3D()
            input_dim = 1024
        # Attention layers            
        self.ImgAttentionFusion = AttentionFusion(input_dim=input_dim, hidden_dim=256).to(device)
    
    def forward(self, DWI_ImgTensor, T2_ImgTensor):
        if self.img_encoder == 'resnet50':
            _, DWI_features = self.DWI_ImgEncoder(DWI_ImgTensor)
            _, T2_features = self.T2_ImgEncoder(T2_ImgTensor)
        else:
            DWI_features = self.DWI_ImgEncoder(DWI_ImgTensor)
            T2_features = self.T2_ImgEncoder(T2_ImgTensor)            
            # print (f"DWI_features", DWI_features.shape)
        fused_features = self.ImgAttentionFusion(DWI_features, T2_features)
        return fused_features
      

class MLP(nn.Module):
    """
    Standard fully-connected MLP with configurable hidden layers,
    batch norm, activation, and dropout.

    Args:
        input_dim (int):  
        hidden_dims (list of int):  
        output_dim (int): 
        activation (callable or nn.Module):  (e.g., nn.ReLU or nn.ReLU())
        dropout (float): dropout rate
        batchnorm (bool): BatchNorm1d
    """
    
    def __init__(
        self,
        input_dim,
        hidden_dims,
        output_dim,
        activation = nn.ReLU,
        dropout = 0.6,
        batchnorm = True,
    ):
        super().__init__()

        if isinstance(activation, type) and issubclass(activation, nn.Module):
            activation_layer = activation
        elif isinstance(activation, nn.Module):
            activation_layer = lambda: activation
        else:
            raise ValueError("activation must be an nn.Module class or instance")

        layers = []
        in_dim = input_dim
        for h_dim in hidden_dims:
            layers.append(nn.Linear(in_dim, h_dim))

            if batchnorm:
                layers.append(nn.BatchNorm1d(h_dim))

            layers.append(activation_layer())

            if dropout > 0:
                layers.append(nn.Dropout(dropout))

            in_dim = h_dim

        layers.append(nn.Linear(in_dim, output_dim))

        self.net = nn.Sequential(*layers)

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Args:
            x: shape (batch_size, input_dim)
        Returns:
            shape (batch_size, output_dim)
        """
        return self.net(x)


class CrossMRIModel(nn.Module):
    def __init__(self, img_encoder='resnet50', device=torch.device('cuda' if torch.cuda.is_available() else 'cpu')):
        super(CrossMRIModel, self).__init__()
        self.img_encoder = img_encoder
        if img_encoder == 'resnet50':
            input_dim = 2048
        elif img_encoder == 'vit':
            input_dim = 1024
        elif img_encoder == 'dense3D':        
            input_dim = 1024   
        # 图像融合层
        self.ImageFusionLayer = ImageFusionModel(img_encoder=self.img_encoder, device=device)
        self.ImageMLP = MLP(input_dim=input_dim, hidden_dims=[1024], output_dim=512)
        # 交叉注意力模块
        self.cross_attn_omics = CrossAttention(
            query_dim=512, 
            key_dim=20, 
            value_dim=20, 
            embed_dim=256)   
        self.cross_attn_clinical = CrossAttention(
            query_dim=512, 
            key_dim=3, 
            value_dim=3, 
            embed_dim=256)
        
        self.concat_fusion_layer = MLP(input_dim=512+256+256, hidden_dims=[512], output_dim=1) 
        
    def forward(self, DWI_ImgTensor, T2_ImgTensor, rad_features, clinical_features):
        # 图像特征融合
        image_fused_features = self.ImageFusionLayer(
            DWI_ImgTensor=DWI_ImgTensor, 
            T2_ImgTensor=T2_ImgTensor
        )
        # 图像特征投影
        image_X = self.ImageMLP(image_fused_features)  # [batch_size, 512]
        # 交叉注意力融合
        cross_omics = self.cross_attn_omics(
            query=image_X, 
            key=rad_features, 
            value=rad_features
        )  # [batch_size, 256]
        
        cross_clinical = self.cross_attn_clinical(
            query=image_X, 
            key=clinical_features, 
            value=clinical_features
        )  # [batch_size, 256]

        # 特征拼接与融合
        x = torch.cat([image_X, cross_omics, cross_clinical], dim=1)  # [batch_size, 1024]
        output = self.concat_fusion_layer(x)
        return output, x   

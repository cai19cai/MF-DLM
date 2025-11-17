import torch
import torch.nn as nn

class SpatioTemporalBottleneck(nn.Module):
    expansion = 4

    def __init__(self, in_channels, out_channels, stride=1, downsample=None):
        super().__init__()
        # Stage 1: 1×1×1 convolution
        self.conv1 = nn.Conv3d(in_channels, out_channels, kernel_size=1, bias=False)
        self.bn1 = nn.BatchNorm3d(out_channels)
        
        # Stage 2: 3×3×3 convolution with stride
        self.conv2 = nn.Conv3d(
            out_channels, out_channels, 
            kernel_size=3, stride=stride, 
            padding=1, bias=False
        )
        self.bn2 = nn.BatchNorm3d(out_channels)
        
        # Stage 3: 1×1×1 convolution with expansion
        self.conv3 = nn.Conv3d(
            out_channels, out_channels * self.expansion, 
            kernel_size=1, bias=False
        )
        self.bn3 = nn.BatchNorm3d(out_channels * self.expansion)
        
        self.relu = nn.ReLU(inplace=True)
        self.downsample = downsample
        self.stride = stride

    def forward(self, x):
        identity = x

        # Feature transformation path
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)

        out = self.conv2(out)
        out = self.bn2(out)
        out = self.relu(out)

        out = self.conv3(out)
        out = self.bn3(out)

        # Shortcut connection
        if self.downsample is not None:
            identity = self.downsample(x)

        out += identity
        out = self.relu(out)

        return out

class VolumetricResNet(nn.Module):
    def __init__(self,
                 block,
                 layers,
                 temporal_depth=11,
                 spatial_size=224,
                 num_classes=1):
        super().__init__()
        self.in_channels = 64
        
        # Input processing
        self.conv1 = nn.Conv3d(
            3, self.in_channels,
            kernel_size=7, stride=(1, 2, 2),
            padding=(3, 3, 3), bias=False
        )
        self.bn1 = nn.BatchNorm3d(self.in_channels)
        self.relu = nn.ReLU(inplace=True)
        self.maxpool = nn.MaxPool3d(
            kernel_size=(3, 3, 3), 
            stride=2, padding=1
        )
        
        # Feature extraction stages
        self.layer1 = self._build_stage(block, 64, layers[0])
        self.layer2 = self._build_stage(block, 128, layers[1], stride=2)
        self.layer3 = self._build_stage(block, 256, layers[2], stride=2)
        self.layer4 = self._build_stage(block, 512, layers[3], stride=2)
        
        # Output processing
        self.feature_pool = nn.AdaptiveAvgPool3d((1, 1, 1))
        self.flatten = nn.Flatten()
        self.fc = nn.Linear(512 * block.expansion, num_classes, bias=True)
        self.output_norm = nn.BatchNorm1d(1)

        # Initialize weights
        self._initialize_weights()

    def _build_stage(self, block, channels, num_blocks, stride=1):
        downsample = None
        if stride != 1 or self.in_channels != channels * block.expansion:
            downsample = nn.Sequential(
                nn.Conv3d(
                    self.in_channels,
                    channels * block.expansion,
                    kernel_size=1,
                    stride=stride,
                    bias=False),
                nn.BatchNorm3d(channels * block.expansion)
            )

        layers = []
        layers.append(block(self.in_channels, channels, stride, downsample))
        self.in_channels = channels * block.expansion
        for _ in range(1, num_blocks):
            layers.append(block(self.in_channels, channels))

        return nn.Sequential(*layers)

    def _initialize_weights(self):
        for m in self.modules():
            if isinstance(m, nn.Conv3d):
                nn.init.kaiming_normal_(
                    m.weight, 
                    mode='fan_out',
                    nonlinearity='relu'
                )
            elif isinstance(m, nn.BatchNorm3d):
                nn.init.constant_(m.weight, 1)
                nn.init.constant_(m.bias, 0)

    def forward(self, x):
        # Input processing
        x = self.conv1(x)
        x = self.bn1(x)
        x = self.relu(x)
        x = self.maxpool(x)
        
        # Feature extraction
        x = self.layer1(x)
        x = self.layer2(x)
        x = self.layer3(x)
        x = self.layer4(x)
        
        # Output processing
        x = self.feature_pool(x)
        x = self.flatten(x)
        features = x
        x = self.fc(x)
        x = self.output_norm(x)
        
        return x, features

def create_volumetric_resnet(model_depth=50, **kwargs):
    """Factory function for 3D ResNet models"""
    config_dict = {
        50: [3, 4, 6, 3],
        101: [3, 4, 23, 3],
        152: [3, 8, 36, 3]
    }
    
    if model_depth not in config_dict:
        raise ValueError(
            f"Unsupported model depth: {model_depth}. "
            f"Supported depths: {list(config_dict.keys())}"
        )
    
    return VolumetricResNet(SpatioTemporalBottleneck, config_dict[model_depth], **kwargs)

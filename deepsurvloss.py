import torch
import torch.nn as nn

class NegativeLogLikelihood(nn.Module):
    def __init__(self, device):
        super(NegativeLogLikelihood, self).__init__()
        self.device = device

    def forward(self, risk_pred, y, e):
        mask = torch.ones(y.shape[0], y.shape[0])
        mask[(y.reshape(-1, y.shape[0]) -y.reshape(y.shape[0], -1)) > 0] = 0
        mask = mask.to(self.device)
        log_loss = torch.exp(risk_pred) * mask
        log_loss = torch.sum(log_loss, dim=0) / torch.sum(mask, dim=0) + torch.tensor([1e-45]).to(self.device)
        log_loss = torch.log(log_loss).reshape(-1, 1)
        neg_log_loss = -torch.sum((risk_pred-log_loss) * e) / torch.sum(e)
        return neg_log_loss

class NegativeLogLikelihoodWithRegular(nn.Module):
    def __init__(self, device, model, weight_decay):
        super(NegativeLogLikelihoodWithRegular, self).__init__()
        self.device = device
        self.model = model
        self.weight_decay = weight_decay

    def forward(self, risk_pred, y, e):
        mask = torch.ones(y.shape[0], y.shape[0])
        mask[(y.reshape(-1, y.shape[0]) -y.reshape(y.shape[0], -1)) > 0] = 0
        mask = mask.to(self.device)
        log_loss = torch.exp(risk_pred) * mask
        log_loss = torch.sum(log_loss, dim=0) / torch.sum(mask, dim=0) + torch.tensor([1e-45]).to(self.device)
        log_loss = torch.log(log_loss).reshape(-1, 1)
        neg_log_loss = -torch.sum((risk_pred-log_loss) * e) / torch.sum(e)

        reg_loss = 0
        for name,w in self.model.named_parameters():
            if 'weight' in name:
                reg_loss += torch.norm(w, p=2)
            
        return neg_log_loss + self.weight_decay * reg_loss
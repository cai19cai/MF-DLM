
from sklearn import model_selection, preprocessing, metrics, feature_selection
import os
import time
import numpy as np
import pandas as pd
import glob
import torch
from torch.utils import data as torch_data
from torch.nn import functional as torch_functional
from resnet import resnet50
from Dataset import GetLoader2
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score
from utils import mkdir, calculate, _init_fn, set_seed, load_npy_data_, c_index
from torch.optim import SGD
import torch.nn.functional as F


class Trainer:
    def __init__(
            self,
            model,
            device,
            optimizer,
            # scheduler,
            criterion
    ):
        self.model = model
        self.device = device
        self.optimizer = optimizer
        self.criterion = criterion
        # self.scheduler = scheduler

        self.best_valid_score = 0  # np.inf
        self.n_patience = 0
        self.lastmodel = None
        self.best_valid_out_df = None
        self.best_valid_train_out_df = None

    def fit(self, epochs, train_loader, valid_loader, modility, save_path, patience, fold, train=True):
        if train:
            train_surv_list = []
            valid_surv_list = []
            best_c_index = 0 # 使用c_index作为指标
            for n_epoch in range(1, epochs + 1):
                self.info_message("EPOCH: {}", n_epoch)

                train_loss, train_cindex, train_out_df = self.train_epoch(train_loader)
                valid_loss, valid_cindex, valid_out_df = self.valid_epoch(valid_loader)

                self.info_message(
                    "[Epoch Train: {}] loss: {:.4f}, cindex: {:.4f}",
                    n_epoch, train_loss, train_cindex
                )

                self.info_message(
                    "[Epoch Valid: {}] loss: {:.4f}, cindex: {:.4f}",
                    n_epoch, valid_loss, valid_cindex
                )
                train_surv_list.append(train_cindex)
                valid_surv_list.append(valid_cindex)


                if self.best_valid_score < valid_cindex:
                    # if self.best_valid_score > valid_loss:
                    self.save_model(n_epoch, modility, save_path, valid_loss, valid_cindex, fold)
                    self.info_message(
                        "loss decrease from {:.4f} to {:.4f}. Saved model to '{}'",
                        self.best_valid_score, valid_cindex, self.lastmodel
                    )
                    self.best_valid_score = valid_cindex
                    self.best_valid_out_df = valid_out_df
                    self.best_valid_train_out_df = train_out_df
                    self.n_patience = 0

                else:
                    self.n_patience += 1

                if self.n_patience >= patience:
                    self.info_message("\nValid auc didn't improve last {} epochs.", patience)
                    break
        else:
            train_loss, train_cindex = self.train_epoch(train_loader)
            valid_loss, valid_cindex = self.valid_epoch(valid_loader)

        print('fold ' + str(fold) + ' finished!')
        return train_surv_list, valid_surv_list, self.best_valid_out_df, self.best_valid_train_out_df

    def train_epoch(self, train_loader):

        self.model.train()

        t = time.time()
        sum_loss = 0
        events_all = []
        times_all = []
        outputs_all = []
        ids_all = []
        features_all = []

        for step, batch in enumerate(train_loader, 1):
            X = batch[0].to(self.device)
            events = batch[1].to(self.device)
            times = batch[2].to(self.device)
            ids_all.extend(batch[3])
            self.optimizer.zero_grad()
            outputs, features = self.model(X)
            outputs = outputs.squeeze(1)

            loss = self.criterion(outputs.to(self.device), times.to(self.device), events.to(self.device))
            # print(batch[3])
            # print('loss: {:.4f}, o:{}, t: {}, e: {}'.format(loss.detach().item(), outputs, times, events))

            loss.backward()


            sum_loss += loss.detach().item()
            events_all.extend(batch[1].tolist())
            times_all.extend(batch[2].tolist())
            outputs_all.extend(outputs.tolist())
            features_all.extend(features.tolist())

            self.optimizer.step()
            
            message = 'Train Step {}/{}, train_loss: {:.4f}'
            self.info_message(message, step, len(train_loader), sum_loss / step, end="\r")
        outputs_all = np.array(outputs_all)
        ids_all = np.array(ids_all)
        times_all = np.array(times_all)
        events_all = np.array(events_all)
        train_cindex = c_index(-outputs_all, times_all, events_all)
        out_df = pd.DataFrame({'ID': ids_all, 'time': times_all, 'status': events_all, 'output': outputs_all})
        out_features = pd.DataFrame(np.array(features_all), columns=[f'dl{i}' for i in range(1, 2049)])
        out = pd.concat([out_df, out_features], axis=1)

        return sum_loss / len(train_loader), train_cindex, out

    def valid_epoch(self, valid_loader):
        self.model.eval()
        t = time.time()
        sum_loss = 0
        events_all = []
        times_all = []
        outputs_all = []
        ids_all = []
        features_all = []

        for step, batch in enumerate(valid_loader, 1):
            with torch.no_grad():
                X = batch[0].to(self.device)
                events = batch[1].to(self.device)
                times = batch[2].to(self.device)
                ids_all.extend(batch[3])

                outputs, features = self.model(X)
                outputs = outputs.squeeze(1)

                loss = self.criterion(outputs, times, events)


                sum_loss += loss.detach().item()
                events_all.extend(batch[1].tolist())
                times_all.extend(batch[2].tolist())
                outputs_all.extend(outputs.tolist())
                features_all.extend(features.tolist())

            message = 'Valid Step {}/{}, valid_loss: {:.4f}'
            self.info_message(message, step, len(valid_loader), sum_loss / step, end="\r")
        outputs_all = np.array(outputs_all)
        times_all = np.array(times_all)
        events_all = np.array(events_all)
        val_cindex = c_index(-outputs_all, times_all, events_all)
        ids_all = np.array(ids_all)
        
        out_df = pd.DataFrame({'ID': ids_all, 'time': times_all, 'status': events_all, 'output': outputs_all})
        out_features = pd.DataFrame(np.array(features_all), columns=[f'dl{i}' for i in range(1, 2049)])
        out = pd.concat([out_df, out_features], axis=1)
        
        return sum_loss / len(valid_loader), val_cindex, out

    def save_model(self, n_epoch, modility, save_path, loss, cindex, fold):

        os.makedirs(save_path, exist_ok=True)
        self.lastmodel = os.path.join(save_path, f"{modility}-fold{fold}.pth")

        #         self.lastmodel = f"{save_path}-e{n_epoch}-loss{loss:.3f}-auc{auc:.3f}.pth"
        torch.save(
            {
                "model_state_dict": self.model.state_dict(),
                "optimizer_state_dict": self.optimizer.state_dict(),
                "best_valid_score": self.best_valid_score,
                "n_epoch": n_epoch,
            },
            self.lastmodel,
        )

    @staticmethod
    def info_message(message, *args, end="\n"):
        print(message.format(*args), end=end)


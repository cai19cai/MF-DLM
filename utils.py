from skimage import transform, exposure
from sklearn import model_selection, preprocessing, metrics, feature_selection
import os
import numpy as np
import random
import torch
import pandas as pd
from lifelines.utils import concordance_index
def load_npy_data_(data_dir, split):
    datanp = []  # images
    dataevent = []  # event
    datatime = [] # time

    for file in os.listdir(data_dir):
        if os.path.splitext(file)[1] == '.npy':
            data = np.load(os.path.join(data_dir, file), allow_pickle=True)
    
            if (split == '2'):
                data_sug = transform.rotate(data[0][0], 30)
    
                datanp.append(data_sug)
                dataevent.append(data[0][1])
                datatime.append(data[0][2])
                # datanp.append(data_sug2)
                # truenp.append(data[0][1])
            dataevent.append(data[0][1])
            datatime.append(data[0][2])
            datanp.append(data[0][0])



    datanp = np.array(datanp)
    datanp = datanp.transpose(0, 4, 3, 1, 2)  # 三通道
    dataevent = np.array(dataevent)
    datatime = np.array(datatime)
    print(datanp.shape, dataevent.shape, datatime.shape)#n c d h w
    return datanp, dataevent, datatime

def load_npy_data_with_id(data_dir, split):   
    datanp = []  # images
    dataevent = []  # event
    datatime = [] # time
    dataids = [] # id

    for file in os.listdir(data_dir):
        data = np.load(os.path.join(data_dir, file), allow_pickle=True)
        dataids.append(file.replace('.npy', ''))

        if (split == '2'):
            data_sug = transform.rotate(data[0][0], 30)

            datanp.append(data_sug)
            dataevent.append(data[0][1])
            datatime.append(data[0][2])
            # datanp.append(data_sug2)
            # truenp.append(data[0][1])
        dataevent.append(data[0][1])
        datatime.append(data[0][2])
        datanp.append(data[0][0])



    datanp = np.array(datanp)
    datanp = datanp.transpose(0, 4, 3, 1, 2)  # 三通道
    dataevent = np.array(dataevent)
    datatime = np.array(datatime)
    print(datanp.shape, dataevent.shape, datatime.shape)#n c d h w
    return datanp, dataevent, datatime, dataids


def set_seed(seed):
    random.seed(seed)
    os.environ["PYTHONHASHSEED"] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False

def set_seed2(seed):
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    # When running on the CuDNN backend, two further options must be set
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False
    # Set a fixed value for the hash seed
    os.environ['PYTHONHASHSEED'] = str(seed)


def _init_fn(worker_id):
    np.random.seed(int(12) + worker_id)


def calculate(score, label, th):
    score = np.array(score)
    label = np.array(label)
    pred = np.zeros_like(label)
    pred[score >= th] = 1
    pred[score < th] = 0
    TP = len(pred[(pred > 0.5) & (label > 0.5)])
    FN = len(pred[(pred < 0.5) & (label > 0.5)])
    TN = len(pred[(pred < 0.5) & (label < 0.5)])
    FP = len(pred[(pred > 0.5) & (label < 0.5)])
    Precision = TP/(TP+FP+0.0001)
    Recall = TP/(TP+FN+0.0001)
    F1_score = 2*Precision*Recall/(Precision+Recall+0.0001)

    AUC = metrics.roc_auc_score(label, score)
    result = {'AUC': AUC, 'acc': (TP + TN) / (TP + TN + FP + FN), 'prec':Precision ,
             'sen/reca': (TP) / (TP + FN + 0.0001),'spe': (TN) / (TN + FP + 0.0001), 'F1-score': F1_score}
    #     print('acc',(TP+TN),(TP+TN+FP+FN),'spe',(TN),(TN+FP),'sen',(TP),(TP+FN))
    return result
def c_index(risk_pred, y, e):
    ''' Performs calculating c-index

    :param risk_pred: (np.ndarray or torch.Tensor) model prediction
    :param y: (np.ndarray or torch.Tensor) the times of event e
    :param e: (np.ndarray or torch.Tensor) flag that records whether the event occurs
    :return c_index: the c_index is calculated by (risk_pred, y, e)
    '''
    if not isinstance(y, np.ndarray):
        y = y.detach().cpu().numpy()
    if not isinstance(risk_pred, np.ndarray):
        risk_pred = risk_pred.detach().cpu().numpy()
    if not isinstance(e, np.ndarray):
        e = e.detach().cpu().numpy()
    return concordance_index(y, risk_pred, e)

def mkdir(path):
    if not os.path.exists(path):
        os.makedirs(path)

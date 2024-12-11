import torch
import os
import numpy as np
from skimage.transform import resize
import pandas as pd


def mkdir(path):
    if not os.path.exists(path):
        os.makedirs(path)


# 定义GetLoader类，继承Dataset方法，并重写__getitem__()和__len__()方法

class GetLoader2(torch.utils.data.Dataset):
    # 初始化函数，得到数据
    def __init__(self, data_root1, data_event1, data_time1):
        self.data1 = data_root1
        self.event1 = data_event1
        self.time1 = data_time1


    # index是根据batchsize划分数据后得到的索引，最后将data和对应的labels进行一起返回
    def __getitem__(self, index):
        data1 = self.data1[index]
        event1 = self.event1[index]
        time1 = self.time1[index]

        return torch.tensor(data1).float(), torch.tensor(event1).float(),torch.tensor(time1).float() \


    # 该函数返回数据大小长度，目的是DataLoader方便划分
    def __len__(self):
        return len(self.data1)

# 定义GetLoader类，继承Dataset方法，并重写__getitem__()和__len__()方法

class GetLoader3(torch.utils.data.Dataset):
    # 初始化函数，得到数据
    def __init__(self, data_root1, data_event1, data_time1):
        self.data1 = data_root1
        self.event1 = data_event1
        self.time1 = data_time1


    # index是根据batchsize划分数据后得到的索引，最后将data和对应的labels进行一起返回
    def __getitem__(self, index):
        data1 = self.data1[index]
        event1 = self.event1[index]
        time1 = self.time1[index]

        return torch.tensor(data1).float(), torch.tensor(event1).float(),torch.tensor(time1).float() \


    # 该函数返回数据大小长度，目的是DataLoader方便划分
    def __len__(self):
        return len(self.data1)

class GetLoaderWithID(torch.utils.data.Dataset):
    # 初始化函数，得到数据
    def __init__(self, data_root1, data_event1, data_time1, ids1):
        self.data1 = data_root1
        self.event1 = data_event1
        self.time1 = data_time1
        self.ids1 = ids1

    # index是根据batchsize划分数据后得到的索引，最后将data和对应的labels进行一起返回
    def __getitem__(self, index):
        data1 = self.data1[index]
        event1 = self.event1[index]
        time1 = self.time1[index]
        id1 = self.ids1[index]

        return torch.tensor(data1).float(), torch.tensor(event1).float(),torch.tensor(time1).float(), id1


    # 该函数返回数据大小长度，目的是DataLoader方便划分
    def __len__(self):
        return len(self.data1)
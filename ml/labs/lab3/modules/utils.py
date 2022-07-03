import pandas as pd
import numpy as np
from numpy import ndarray


def read(path: str) -> (ndarray, ndarray):
    dataset = pd.read_csv(path)
    dataset['class'] = dataset['class'].map({'P': 1, 'N': -1})
    x = np.array(dataset[['x', 'y']].values.tolist())
    y = np.array(dataset['class'].values.tolist())
    return x, y


def get_values(values: ndarray, ind: ndarray) -> ndarray:
    return np.array([values[i] for i in ind])


def get_train_test(x: ndarray, y: ndarray, parts: int, shuffle: bool = False) -> (ndarray, ndarray, ndarray, ndarray):
    indexes = np.arange(0, len(x))

    if shuffle:
        np.random.shuffle(indexes)

    block_size = np.math.ceil(len(x) / parts)
    x_test, y_test, x_train, y_train = [], [], [], []

    for i in range(parts):
        l = block_size * i
        r = block_size * (i + 1)
        indexes_test = indexes[l:r]
        indexes_train = np.concatenate((indexes[:l], indexes[r:]))
        x_test.append(get_values(x, indexes_test))
        y_test.append(get_values(y, indexes_test))
        x_train.append(get_values(x, indexes_train))
        y_train.append(get_values(y, indexes_train))
    return np.array(x_test), np.array(y_test), np.array(x_train), np.array(y_train)

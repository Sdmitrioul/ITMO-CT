import numpy as np
from numpy import ndarray


def mse(y_predicted: ndarray, y_actual: ndarray) -> float:
    return np.mean(np.square(y_actual - y_predicted))


def mse_gradient(weight: np.ndarray, x_train: np.ndarray, y_train: float) -> float:
    return 2 * (x_train * ((weight @ x_train) - y_train))


def smape(y_predicted: ndarray, y_actual: ndarray) -> float:
    return np.mean(np.abs(y_predicted - y_actual) / ((np.abs(y_actual) + np.abs(y_predicted)) / 2))


def nmrse(y_predicted: ndarray, y_actual: ndarray) -> float:
    return np.sqrt(np.sum(np.power(y_predicted - y_actual, 2)) / len(y_predicted)) / (np.max(y_actual) - np.min(y_actual))

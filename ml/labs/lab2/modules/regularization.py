import random

import numpy as np

from modules.error import mse_gradient, smape, nmrse
from modules.helpers import start_vector


def least_square_method(train_x: np.ndarray, train_y: np.ndarray, *, thau: float = 0.9) -> np.ndarray:
    if thau == 0.:
        return np.linalg.pinv(train_x) @ train_y

    size = train_x[0].size
    inv = np.linalg.inv(train_x.T @ train_x + thau * np.eye(size))

    return inv @ train_x.T @ train_y


def sgd(train_x: np.ndarray, train_y: np.ndarray, train_count, *, max_operation: int = 2000, thau: float = 0.9, log_err: bool = False) -> (np.ndarray, object):
    weight = start_vector(train_x[0].size)
    counter = 1
    log = []
    gradient = 0
    omega = 0.9
    vector_is_equal = False
    Q = smape(train_x @ weight, train_y)
    err = []

    while max_operation >= counter and not vector_is_equal:
        log.append((weight, Q))

        counter += 1
        index = random.randint(0, train_count - 1)
        h = 1. / counter

        phi = smape(np.array([train_x[index] @ weight]), np.array([train_y[index]]))

        gradient = 2 * ((1 - omega) * mse_gradient(weight, train_x[index], train_y[index]) + thau * weight) + omega * gradient
        weight -= h * gradient
        Q = (1 - thau) * Q + thau * phi

        if log_err:
            mul = train_x @ weight
            err.append((smape(mul, train_y), nmrse(mul, train_y)))
    else:
        log.append((weight, Q))

    return weight, log, err

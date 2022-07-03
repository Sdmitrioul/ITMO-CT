import numpy as np


def start_vector(n: int) -> np.ndarray:
    element = 1. / (2 * n)
    return np.random.uniform(-element, element, n)

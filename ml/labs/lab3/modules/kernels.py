from numpy import ndarray, exp, sqrt, sum


def linear(x: ndarray, y: ndarray, _) -> float:
    return x @ y


def polynomial(x: ndarray, y: ndarray, p: float) -> float:
    return (1 + (x @ y)) ** p


def gaussian(x: ndarray, y: ndarray, beta: float) -> float:
    return exp(-beta * sqrt(sum((x - y) ** 2)) ** 2)


kernels = {
    'linear': linear,
    'polynomial': polynomial,
    'gaussian': gaussian,
}

import numpy as np


def calc_parabola(x, y):
    return np.linalg.solve(
        [[x_i * x_i, x_i, 1] for x_i in x],
        y
    )


EPS = 0.01


def calc_range(x, par):
    return list(map(lambda g: par[0] + par[1] * g + par[2] * g * g, x))


def compute(f, x, coef):
    indices = []
    for (i, (x1, _, x3)) in enumerate(zip(x, x[1:], x[2:])):
        if x1 <= f <= x3:
            indices.append(i)
    if len(indices) == 0:
        raise RuntimeError(f"{x} is outside of interpolation range")
    total = 0
    for i in indices:
        total += (coef[i] * [f ** 2, f, 1]).sum()
    return total / len(indices)


def quadratic(x, y, _range):
    xy = list(zip(x, y))
    xy.sort(key=lambda p: p[0])
    x, y = zip(*xy)
    coefficients = []
    for x1, x2, x3, y1, y2, y3 in zip(x, x[1:], x[2:], y, y[1:], y[2:]):
        coefficients.append(calc_parabola((x1, x2, x3), (y1, y2, y3)))
    return list(map(lambda b: compute(b, x, coefficients), _range))

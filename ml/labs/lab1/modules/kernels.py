def uniform_kernel(u: float) -> float:
    return 0.5 if abs(u) <= 1 else 0.0


def triangular_kernel(u: float):
    return max(0., 1 - abs(u))


def epanechnikov_kernel(u: float) -> float:
    return max(0., 0.75 * (1 - u * u))


def quartic_kernel(u: float) -> float:
    return max(0., 15 / 16 * ((1 - u * u) ** 2))


kernels = {
    "uniform": uniform_kernel,
    "triangular": triangular_kernel,
    "epanechnikov": epanechnikov_kernel,
    "quartic": quartic_kernel,
}

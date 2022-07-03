from math import sqrt

import numpy as np


def euclidian_distance(vec1: np.ndarray, vec2: np.ndarray) -> float:
    ans = 0
    for i in range(len(vec1)):
        ans += (vec1[i] - vec2[i]) ** 2
    return sqrt(ans)


def manhattan_distance(vec1: np.ndarray, vec2: np.ndarray) -> float:
    ans = 0
    for i in range(len(vec1)):
        ans += abs(vec1[i] - vec2[i])
    return ans


def chebishev_distance(vec1: np.ndarray, vec2: np.ndarray) -> float:
    ans = float("-inf")
    for i in range(len(vec1)):
        ans = max(ans, abs(vec1[i] - vec2[i]))
    return ans


metrics = {
    "euclidean": euclidian_distance,
    "chebyshev": chebishev_distance,
    "manhattan": manhattan_distance,
}

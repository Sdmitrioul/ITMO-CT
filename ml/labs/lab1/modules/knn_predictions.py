from modules.kernels import kernels
from modules.distance import metrics

from math import sqrt

import numpy as np


def kernel_regression(
        objs,
        index,
        default_val,
        distances,
        neighbours=0,
        window=0.,
        metric_name="euclidean",
        kernel_name="triangular"):
    kernel = kernels[kernel_name]

    dist = np.array(distances[metric_name][index])
    sorted_dist = dist.copy()
    sorted_dist.sort()

    h = window
    if neighbours != 0:
        h = sorted_dist[min(len(sorted_dist), neighbours + 2)]

    sum_pred = np.array([0.] * 3)
    denominator = 0.

    for i in range(len(objs)):
        if i == index:
            continue

        buff = kernel(dist[i] / h)
        denominator += buff
        sum_pred += buff * objs[i][0]

    return sum_pred / denominator if denominator != 0 else default_val


def calculate_predictions(ndata, max_distance, distances):
    predict_fixed_res = {}
    predict_var_res = {}

    length = len(ndata)
    size = sqrt(length)

    default_result = np.array([0.] * 3)
    for i in range(length):
        default_result += ndata[i][0]
    default_result = default_result / length

    for kernel in kernels.keys():
        for dist in metrics.keys():
            max_dis = max(max_distance[dist])
            step = max_dis / size

            for i in range(length):
                fixed = step
                while fixed <= max_dis:
                    index = (kernel, dist, fixed)

                    if i == 0:
                        predict_fixed_res[index] = []

                    predict_fixed_res[index].append(kernel_regression(ndata, i, default_result, distances, window=fixed, metric_name=dist, kernel_name=kernel))
                    fixed += step

                for var in range(1, int(size)):
                    index = (kernel, dist, var)

                    if i == 0:
                        predict_var_res[index] = []

                    predict_var_res[index].append(kernel_regression(ndata, i, default_result, distances, neighbours=var, metric_name=dist, kernel_name=kernel))
    return predict_fixed_res, predict_var_res

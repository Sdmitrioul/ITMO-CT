from bisect import bisect
import numpy as np
from typing import List, Tuple, Union


def get_div(array: List[Union[int, float]], bins: int) -> float:
    return (array[-1] - array[0]) / bins


def get_eps(array: List[Union[int, float]], bins: int) -> float:
    return (array[-1] - array[0]) / bins


def fast_hist(array: List[Union[int, float]],
              bins: int) -> Tuple[List[int], List[float]]:
    """
    Builds bins' labels and bins' value counts for given array
    :param array: array with numeric values
    :param bins:  number of bins in result distribution
    :return: Two lists:
             first contains value counts of each bin,
             second contains list of bins' labels
    """
    array.sort()
    labels = (np.arange(array[0], array[-1], get_div(array, bins))).tolist()
    regions = [0] * bins
    right = 0
    for bin in range(bins):
        next_bin_label = labels[bin + 1] if bin < bins - 1 else labels[bin] + 100
        while right < len(array) and array[right] < next_bin_label:
            right += 1
            regions[bin] += 1

    return regions, labels

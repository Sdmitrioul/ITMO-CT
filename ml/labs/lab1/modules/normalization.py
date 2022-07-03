import pandas as pd
from numpy import ndarray


def min_max_normalization(ds: pd.DataFrame, main: str) -> ndarray:
    objs = ds.values
    min_max = {}
    index = 0
    mr = 0
    for i in ds:
        if i == main:
            mr = index
            continue

        min_el = ds[i].min()
        max_el = ds[i].max()
        min_max[index] = (min_el, max_el)
        index += 1

    for i in range(len(objs)):
        for j in range(len(objs[i])):
            col = j if j < mr else j - 1
            if j != mr:
                objs[i][j] = (objs[i][j] - min_max[col][0]) / (min_max[col][1] - min_max[col][0])

    return objs

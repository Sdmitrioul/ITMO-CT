import numpy as np


def min_max_normalization(objs, m_attr_in, min_max):
    norm_data = []
    index = len(objs[0])
    m_attr = []

    for i in range(len(m_attr_in)):
        w = min_max[1][index] - min_max[0][index]
        m_attr.append(0 if w == 0 else (m_attr_in[i] - min_max[0][index]) / w)

    for obj in objs:
        current = []
        for i in range(obj.size):
            w = min_max[1][i] - min_max[0][i]
            res = 0 if w == 0 else (obj[i] - min_max[0][i]) / w
            current.append(res)
        norm_data.append(np.array(current))

    return np.array(norm_data), np.array(m_attr)

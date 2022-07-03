import numpy as np


def read_linear(filename: str):
    file = open(filename, 'r')
    obj_size = int(file.readline())

    train_count = int(file.readline())

    objs = []
    m_attr = []
    min_max = [[float('inf') for _ in range(obj_size + 1)], [float('-inf') for _ in range(obj_size + 1)]]

    read_objects(file, m_attr, min_max, obj_size, objs, train_count)

    test_count = int(file.readline())

    read_objects(file, m_attr, min_max, obj_size, objs, test_count)

    objs = np.array(objs)
    m_attr = np.array(m_attr)

    file.close()

    return objs, m_attr, min_max, train_count, test_count


def read_objects(file, m_attr, min_max, obj_size, objs, test_count):
    for _ in range(test_count):
        current = list(map(int, file.readline().split()))
        objs.append(np.array(current[:obj_size]))
        m_attr.append(current[obj_size])
        for i in range(obj_size + 1):
            min_max[0][i] = current[i] if min_max[0][i] > current[i] else min_max[0][i]
            min_max[1][i] = current[i] if min_max[1][i] < current[i] else min_max[1][i]

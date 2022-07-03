def row(matrix, i):
    n = len(matrix)
    ans = 0

    for j in range(n):
        ans += matrix[j][i]
    return ans


def col(matrix, i):
    n = len(matrix)
    ans = 0
    for j in range(n):
        ans += matrix[i][j]
    return ans


def all_(matrix):
    ans = 0
    for row_item in matrix:
        for item in row_item:
            ans += item
    return ans


def prec_w(matrix):
    n = len(matrix)
    ans = 0
    all_r = all_(matrix)
    for i in range(n):
        col_ = col(matrix, i)
        if col_ != 0:
            ans += matrix[i][i] * row(matrix, i) / col_
    return ans / all_r


def recall_w(matrix):
    n = len(matrix)
    ans = 0
    all_r = all_(matrix)
    for i in range(n):
        ans += matrix[i][i]
    return ans / all_r


def f_macro(matrix, beta):
    precision = prec_w(matrix)
    recall = recall_w(matrix)
    weight = pow(beta, 2)
    if precision == 0 and recall == 0:
        return 0
    return (1 + weight) * precision * recall / (weight * precision + recall)

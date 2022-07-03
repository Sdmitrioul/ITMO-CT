from numpy import ndarray


def accuracy(confusion_matrix: ndarray) -> float:
    size = len(confusion_matrix)
    tp = confusion_matrix[0][0]
    tn = confusion_matrix[1][1]
    total = sum([sum(confusion_matrix[i]) for i in range(size)])
    return (tp + tn) / total

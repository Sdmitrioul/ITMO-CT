import re
import numpy as np

from numpy import ndarray


def read_text(filename: str) -> str:
    text = open(filename, 'r').read().lower()

    text = re.sub(r'[ё]', 'е', text)
    text = re.sub(r'[,:-]', ' ', text)
    text = re.sub(r'[^а-я\s.]', '.', text)
    text = re.sub(r'\s+', ' ', text)

    return text


def get_char_indexes(text: str) -> (dict, dict):
    chars = {}
    indexes = {}

    text_chars = sorted(list(set(text)))

    for ind, char in enumerate(text_chars):
        chars[char] = ind
        indexes[ind] = char

    return chars, indexes


def get_data_from_text(text: str, length: int = 5) -> (ndarray, ndarray):
    dataX = []
    dataY = []

    text_sentences = text.split('.')

    for sentence in text_sentences:
        sentence += '.'

        for i in range(len(sentence) - length):
            dataX.append(sentence[i:i + length])
            dataY.append(sentence[length + i])

    return dataX, dataY


def get_coding(data_x: ndarray, data_y: ndarray, chars: dict, length: int = 5) -> (ndarray, ndarray):
    count_sentences = len(data_x)

    X = np.zeros((count_sentences, length, len(chars)))
    Y = np.zeros((count_sentences, len(chars)))

    for i, sentence in enumerate(data_x):
        for j, char in enumerate(sentence):
            X[i, j, chars[char]] = 1

        Y[i, chars[data_y[i]]] = 1

    return X, Y

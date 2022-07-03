import pandas as pd


def weight(series: pd.Series) -> pd.Series:
    return series.str.extract(r'(\d+) lbs\. / (\d+(\.\d+|)) kg\.')[1].apply(float)


def height(series: pd.Series) -> pd.Series:
    return series.str.extract(r'((\d+)-(\d+)) / (\d+(\.\d+|))')[3].apply(float)


def salary(series: pd.Series) -> pd.Series:
    return series.str.slice(1).apply(int)

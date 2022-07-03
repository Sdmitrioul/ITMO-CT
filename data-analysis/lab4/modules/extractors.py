import pandas as pd


def weight(series: pd.Series) -> pd.Series:
    return series.str.extract(r'(\d+) lbs\. / (\d+(\.\d+|)) kg\.')[1].apply(float)


def height(series: pd.Series) -> pd.Series:
    return series.str.extract(r'((\d+)-(\d+)) / (\d+(\.\d+|))')[3].apply(float)


def salary(series: pd.Series) -> pd.Series:
    return series.str.slice(1).apply(int)


year_2020 = pd.to_datetime("2020/12/31", yearfirst=True)


def age(series: pd.Series) -> pd.Series:
    return series.apply(lambda x: int((year_2020 - pd.to_datetime(x, format='%m/%d/%y')).days / 365))

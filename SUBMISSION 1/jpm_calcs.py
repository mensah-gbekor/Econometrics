import numpy as np
import pandas as pd
import math

from csv_reader import csv_reader


def avg_s_v(x):
    return round(np.sum(x)/x.size, 4)


def stock_volatility(a):
    std = np.std(a)
    return round(std, 8)


def daily_stock_returns(x):
    n = len(x)
    returns = []
    for i in range(1,n):
        o_price = x[i-1]
        c_price = x[i]
        d_return = (c_price/o_price)-1
        returns.append(d_return)
    return returns


def reg_analysis(x, y):
    import statsmodels.api as sm
    x1 = sm.add_constant(x)

    model = sm.OLS(y, x1)

    results = model.fit()
    print(results.summary())


def calc_reg():
    jpm = pd.read_csv('./JPM.csv', parse_dates=True, index_col='Date')
    sp = pd.read_csv('./S & P 500.csv', parse_dates=True, index_col='Date')
    prices = pd.concat([jpm['Close'], sp['Close']], axis=1)
    prices.columns = ['jpm', 'sp']
    returns = prices.pct_change(1)
    clean_returns = returns.dropna(axis=0)

    x = clean_returns['jpm']
    y = clean_returns['sp']
    reg_analysis(x, y)


def main():
    data = csv_reader("./JPM.csv")
    x = np.array(data['open'])
    y = np.array(data['close'])
    a = np.array(data['adj_close'])
    a_s_v = avg_s_v(a)
    d_r = daily_stock_returns(data['adj_close'])
    s_v = stock_volatility(d_r)

    a = 'Average stock value = {}'
    b = 'Stock Volatility(daily) = {}%'
    c = 'Stock Volatility(annualized) = {}%'
    print(a.format(a_s_v), '\n', b.format(s_v*100), '\n', c.format(round(100*s_v*math.sqrt(252),8)))
    for i in range(0, len(d_r)):
        print('Daily returns {} = {}%'.format(i + 1, round(d_r[i]*100, 4)))
    calc_reg()
    
if __name__ == '__main__':
    main()
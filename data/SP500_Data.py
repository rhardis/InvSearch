#!/usr/bin/env python
# coding: utf-8


import yfinance as yf
import pandas as pd
import numpy as np
import os
import time

datadir = 'C:/Users/richa/Documents/GitHub/InvSearch/data'
sp500 = pd.read_csv(os.path.join(datadir, "S&P500-Symbols.csv"),index_col=0)


def rename_with_symbols(df, symbol):
    for col in df.columns:
        if '_' not in col:
            df.rename(columns={col: "{}_{}".format(symbol,col)}, inplace=True)
    return(None)

def remove_close(df):
    for col in df.columns:
        df.rename(columns={col: col[:-6]}, inplace=True)
    return(None)


def download_data():
    ticklist = ['^GSPC', 'UPRO', 'SSO', 'IAU', 'TLT'] + list(sp500.iloc[:, 0])
    total_df = yf.Ticker(ticklist[0]).history(period="max")

    rename_with_symbols(total_df, ticklist[0])

    end = len(ticklist)
    ticklist = [ticker for ticker in ticklist if ticker not in ['BRK.B', 'BF.B', 'KEYS']]
    for i, ticker in enumerate(ticklist[1:end]):

        base = np.random.uniform(low=0, high=2)
        ran = np.abs(np.random.normal(5, base))
        time.sleep(ran)

        try:
            new_df = yf.Ticker(ticker).history(period="max")
            rename_with_symbols(new_df, ticker)

            dupe_list = new_df.index.duplicated()
            dupe_sum = np.sum(dupe_list)
            if dupe_sum > 0:
                print(ticker)
                print(new_df.tail())

                keeplist = list(range(0, len(new_df) - 2))
                keeplist.append(len(new_df) - 1)
                new_df = new_df.iloc[keeplist, :]

                print(new_df.tail())

            total_df = total_df.join(new_df, how='outer')
        except:
            print("failed {}".format(ticker))
            next

        indlist = list(total_df.index)
        indlist = [str(item)[:10] for item in indlist]

        if indlist.count('2020-05-08') > 1:
            print("Error of multiple May 7 Happened on {} number ticker {}".format(i, ticker))

        if i % 50 == 0:
            print("{}% Gathered".format(i / len(ticklist) * 100))

    total_df = total_df[[col for col in total_df.columns if 'Close' in col]]
    remove_close(total_df)

    # total_df = total_df.iloc[:-1,:]    # Use this cell if running on a day that has not hit close of market yet
    total_df.tail(40)

    total_df.to_csv("sp500_individual_prices.csv")

    returns_df = total_df.copy(deep=True)
    for ticker in returns_df.columns:  # ticklist[:end]:
        today = returns_df["{}".format(ticker)]
        yesterday = returns_df["{}".format(ticker)].shift(1, axis=0)
        returns_df[ticker] = (today - yesterday) / yesterday * 100
    returns_df = returns_df[returns_df.columns]
    returns_df.to_csv("sp500_individual_returns.csv")


# if __name__ == "__main__":
#     download_data()
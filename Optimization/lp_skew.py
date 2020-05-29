import math
import os
import subprocess
import time
import yaml

import gurobipy as gp
import hmmlearn.hmm as hmm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import yfinance as yf

from datetime import datetime
from gurobipy import *
from scipy.stats import skew



# Invoke Gurobi to solve the LP
def solve_lp(methnum, annualized_r, sigma, inflation_factor, regime, min_days, returns_dataframe):
    # Model setup
    gm = gp.Model("Konno")

    # Create the data
    mu, T, n, returns, cnames = get_optimization_data(regime, min_days, returns_dataframe)

    r_expected = (((1 + (annualized_r / 100)) ** (1 / 252)) - 1) * 100  # Set the expected return here.  Asssumes 252 trading days per year
    #print(r_expected)
    rj = mu  # means are denoted by r in Konno's paper
    alpha = 1  # Set value for alpha
    time_inverse = 1 / (T - 1)  # Constant for minimization of objective function

    w = (T - 1) * np.sqrt(2 / math.pi) * sigma * inflation_factor

    rho_1 = r_expected - 1
    rho_2 = r_expected - 2

    min_prop = 0.04
    max_prop = 0.2

    # --------------------------------------------------------------------------
    # IMPLEMENT LP Formulation Here
    # --------------------------------------------------------------------------

    # Add vars
    u = gm.addVars(T)  # For calulating Skewness
    v = gm.addVars(T)  # For calculating Skewness
    x = gm.addVars(n)  # Weights of each asset
    XI = gm.addVars(T)  # For constraining
    H = gm.addVars(T)  # For constraining

    # Set the Objective
    gm.setObjective(time_inverse * (quicksum(u[t] for t in range(T)) + alpha * quicksum(v[t] for t in range(T))), GRB.MINIMIZE)

    y = gm.addVars(n, vtype=GRB.BINARY)
    gm.addConstrs(y[j] >= x[j] for j in range(n))
    gm.addConstrs(min_prop * y[j] <= x[j] for j in range(n))
    gm.addConstrs(max_prop * y[j] >= x[j] for j in range(n))

    # Add constraints
    gm.addConstrs((u[t] + quicksum(returns[t, j] * x[j] for j in range(n))) >= rho_1 for t in range(T))
    gm.addConstrs((v[t] + quicksum(returns[t, j] * x[j] for j in range(n))) >= rho_2 for t in range(T))

    gm.addConstrs(XI[t] - H[t] - quicksum(returns[t, j] * x[j] for j in range(n)) >= r_expected for t in range(T))
    gm.addConstr(quicksum(rj[j] * x[j] for j in range(n)) >= r_expected)

    gm.addConstr(quicksum(XI[t] + H[t] for t in range(T)) <= w)
    gm.addConstr(quicksum(x[j] for j in range(n)) == 1)
    gm.addConstrs(x[j] >= 0 for j in range(n))
    gm.addConstrs(u[t] >= 0 for t in range(T))
    gm.addConstrs(v[t] >= 0 for t in range(T))
    gm.addConstrs(XI[t] >= 0 for t in range(T))
    gm.addConstrs(H[t] >= 0 for t in range(T))

    # Solve the model
    meth = methnum
    gm.setParam('Method', meth)
    gm.setParam('OutputFlag', 0)
    gm.update()

    gm.setParam('LogToConsole', 1)
    gm.setParam('LogFile', "method_{}.log".format(meth))

    gm.optimize()

    weights = []
    for k in x.keys():
        weights.append(x[k].x)
    weights = np.array(weights)
    # print(weights)
    #print(np.mean(returns.dot(weights)))
    #print(calc_geom_mean(returns.dot(weights)))
    print("annualized geom return = {}".format((1 + calc_geom_mean(returns.dot(weights)) / 100) ** 252))
    #print("")
    #print(np.std(returns.dot(weights)))
    print("portfolio skew = {}".format(skew(returns.dot(weights))))
    print("")

    return (gm, weights, skew(returns.dot(weights)), cnames)


# Generate the data
def get_optimization_data(regime, min_days, returns_df):
    filtered_returns = returns_df[returns_df.Regime == regime]
    returns = filtered_returns.iloc[:, :-1]  # remove regimes names column
    enough_returns_level = np.floor(min_days)  # Returns must have a minumum of X*252 days of returns in the given regime
    enough_returns_list = [i for i, _ in enumerate(returns.columns) if returns.iloc[:, i].count() >= enough_returns_level]
    returns = returns.iloc[:, enough_returns_list]
    cnames = returns.columns

    returns = returns.to_numpy()
    returns = returns[~np.isnan(returns).any(axis=1)]  # Drop any rows containing nan
    returns = returns[~np.isinf(returns).any(axis=1)]  # Drop any rows containing infinity
    T = returns.shape[0]
    num_assets = returns.shape[1]
    mu_geom = calc_geom_means(returns)
    # mu = np.mean(returns, axis=0)
    # print(mu_geom)
    print(returns.shape)

    return (mu_geom, T, num_assets, returns, cnames)

def calc_geom_mean(ret):
    scaled_col = 1+(ret/100)
    geom_avg = np.prod(scaled_col)**(1/len(ret))
    rescaled_geom_avg = (geom_avg-1)*100
    return(rescaled_geom_avg)

def calc_geom_means(rets):
    means = []
    for i in range(rets.shape[1]):
        scaled_col = 1+(rets[:,i]/100)
        geom_avg = np.prod(scaled_col)**(1/rets.shape[0])
        rescaled_geom_avg = (geom_avg-1)*100
        means.append(rescaled_geom_avg)
    return(np.array(means))


def best_return_allocation(balance, current_regime, md, rdf, pdf):
    final_output = None
    final_weights = None
    final_skew = -np.inf
    final_annualized_r = -np.inf
    final_sd_inflation = None

    min_allowable_skew = 0.1
    min_sd_inflation = 0.2
    max_sd_inflation = 1.1

    fin_flag = False
    for annualized_r in [60, 50, 40, 30, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10]:

        if fin_flag:
            break

        for inflation_factor in np.arange(min_sd_inflation + 0.6, max_sd_inflation, 0.1):
            #print('Annualized r {} vs Final r {}'.format(annualized_r, final_annualized_r))

            try:
                out, weights, skw, cn = solve_lp(methnum=2, annualized_r=annualized_r, sigma=1, inflation_factor=inflation_factor, regime=current_regime, min_days=md, returns_dataframe=rdf)
                if (annualized_r >= final_annualized_r) & (skw >= min_allowable_skew):
                    final_output = out
                    final_weights = weights
                    final_skew = skw
                    final_annualized_r = annualized_r
                    final_sd_inflation = inflation_factor
                    fin_flag = True
                    break
            except AttributeError:
                #print("Infeasible on anr {}, inflf {}".format(annualized_r, inflation_factor))
                next

    #print("finished")

    purchased = np.where(final_weights > 0.01)[0]
    buylist = list(cn[purchased])

    #print(balance)
    values = balance * final_weights[purchased]

    buy_dict = {}

    for ticker, val in zip(buylist, values):
        price = pdf.loc[:, ticker][-1]
        #print(val)
        shares = round(val / price, 0)
        #print(shares)
        buy_dict[ticker] = shares

    return (buy_dict)

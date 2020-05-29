#!/usr/bin/env python
# coding: utf-8

import numpy as np

class Account:
    def __init__(self, test_name, starting_balance):
        self.name = test_name
        self.assets = {}  # 'ticker': {'share_count':#, 'share_value':#, 'cost_basis':#}
        self.balance = starting_balance
        self.account_value = starting_balance
        self.history = {'date':[], 'account_value':[]}
        
    def update_account_value(self, pdf):
        self.account_value = self.balance
        for key in self.assets.keys():
            #print(key)
            shares_count = self.assets[key]['share_count']
            #print("shares count = {}".format(shares_count))
            shares_price = pdf.loc[:,key][-1]
            #print("shares price = {}".format(shares_price))
            asset_val = shares_count * shares_price   # Share count * current share price
            #print("asset_val = {}".format(asset_val))
            self.account_value += asset_val
            
    def buy(self, ticker, shares, price):
        sale_price = shares*price
        self.balance -= sale_price
        
        if ticker not in self.assets.keys():
            self.assets[ticker] = {'share_count':0, 'cost_basis':0}
        
        prior_cost_basis = self.assets[ticker]['cost_basis']
        prior_share_count = self.assets[ticker]['share_count']
        self.assets[ticker]['cost_basis'] = ((prior_cost_basis*prior_share_count)+sale_price)/(prior_share_count+shares)
        
        self.assets[ticker]['share_count'] += shares

    
    def sell(self, ticker, shares, price):
        if ticker not in self.assets.keys():
            pass
        
        sale_price = shares*price
        self.balance += sale_price
        
        self.assets[ticker]['share_count'] -= shares


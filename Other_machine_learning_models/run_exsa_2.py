# -*- coding: utf-8 -*-
"""
Created on Fri Feb  5 16:21:25 2021

@author: yange
"""

import xgboost as xgb
import numpy as np
import collections
from functools import cmp_to_key
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV
from xgboost.sklearn import XGBRegressor
import pandas as pd
from libsurv.datasets import survival_df
from xgboost import DMatrix
import os
os.chdir("d:\\#workspace\\STUDY\\LUAD") 
import random
random.seed(1234) 
import numpy as np
np.random.seed(1234)

def _abs_sort(x, y):
    """
    Built-in `cmp` function for sorting.
    """
    x, y = x[1], y[1]
    if abs(x) == abs(y):
        return y - x
    return abs(x) - abs(y)

def _label_abs_sort(label):
    """
    Built-in function for sorting labels according to its absolute value.
    """
    L = [(i, x) for i, x in enumerate(label)]
    L = sorted(L, key=cmp_to_key(_abs_sort))
    return [x[0] for x in L]

def efn_loss(preds, dtrain):
    """
    Computation of objective function.
    Parameters
    ----------
    preds: numpy.array
        An array with shape of (N, ), where N = #data.
    dtrain: xgboost.DMatrix
        Training data with type of `xgboost.DMatrix`. Labels can be obtained by: 
        `labels = dtrain.get_label()`, and it is `numpy.array` with shape of (N, ), where N = #data.
    Returns
    -------
    tuple:
        Name and value of objective function.
    """
    n = preds.shape[0]
    # Sorted Orders
    labels = dtrain.get_label()
    label_order = _label_abs_sort(labels)
    # Statistics for at risk i: R(i)
    R = np.zeros(labels.shape, dtype='int')
    for i, ind in enumerate(label_order):
        if i == 0 or abs(labels[ind]) != abs(labels[label_order[i-1]]):
            R[i] = i
        else:
            R[i] = R[i-1]
    # Compute Loss value
    ### Hazard Ratio
    hr = np.exp(preds)
    ### Compute SR (sum of HR at risk)
    cum_hr = np.cumsum(hr[label_order])
    sum_hr = cum_hr[n-1]
    def SR(idx):
        if R[idx] == 0:
            return sum_hr
        return sum_hr - cum_hr[R[idx]-1]
    out = .0
    cnt_event = 0
    for i, ind in enumerate(label_order):
        if labels[ind] > 0:
            out += np.log(SR(i)) - preds[ind]
            cnt_event += 1
    ### normalize by the number of events
    return "efron_loss", out / cnt_event

def _efn_grads(preds, dtrain):
    """
    Gradient computation of custom objective function - Efron approximation.
    Parameters
    ----------
    preds: numpy.array
        An array with shape of (N, ), where N = #data.
    dtrain: xgboost.DMatrix
        Training data with type of `xgboost.DMatrix`. Labels can be obtained by: 
        `labels = dtrain.get_label()`, and it is `numpy.array` with shape of (N, ), where N = #data.
    Returns
    -------
    tuple:
        The first- and second-order gradients of objective function w.r.t. `preds`.
    """
    n = preds.shape[0]
    # Sorted Orders
    labels = dtrain.get_label()
    label_order = _label_abs_sort(labels)
    # Statistics for at risk i: R(i)
    # Statistics for failures at t: failures(t)
    R = np.zeros(labels.shape, dtype='int')
    failures = collections.OrderedDict()
    for i, ind in enumerate(label_order):
        death_t = labels[ind]
        if i == 0 or abs(death_t) != abs(labels[label_order[i-1]]):
            R[i] = i
        else:
            R[i] = R[i-1]
        if death_t > 0:
            if death_t not in failures:
                failures[death_t] = [i]
            else:
                # ties occured
                failures[death_t].append(i)
    # Compute grad and hessian
    ### Hazard Ratio
    hr = np.exp(preds)
    ### Compute SR (sum of HR at risk)
    cum_hr = np.cumsum(hr[label_order])
    sum_hr = cum_hr[n-1]
    def SR(idx):
        if R[idx] == 0:
            return sum_hr
        return sum_hr - cum_hr[R[idx]-1]
    ### Compute SFR and SFR2
    def SRF(sfr, ord=1):
        sfr[0] = .0
        sfr_prev = sfr[0]
        for t, fails in failures.items():
            sfr[t] = sfr_prev + len(fails) * 1.0 / (SR(fails[0])**ord)
            sfr_prev = sfr[t]
    ### Get SFR and SFR2
    sfr1 = collections.OrderedDict()
    SRF(sfr1, ord=1)
    sfr2 = collections.OrderedDict()
    SRF(sfr2, ord=2)
    ### Compute Gradient and Hessian
    grad = np.zeros_like(preds)
    hess = np.zeros_like(preds)
    death_time = 0
    for ind in label_order:
        death_t = labels[ind]
        if death_t > 0:
            grad[ind] = hr[ind] * sfr1[death_t] - 1.0
            hess[ind] = grad[ind] + 1.0 - (hr[ind]**2) * sfr2[death_t]
            death_time = death_t
        else:
            grad[ind] = hr[ind] * sfr1[death_time]
            hess[ind] = grad[ind] - (hr[ind]**2) * sfr2[death_time]
    ### Finish Compute
    return grad, hess

class GV:
    '''
    Scoring	Function	   Comment
    *Classification
    ‘accuracy’             metrics.accuracy_score
    ‘average_precision’	   metrics.average_precision_score
    ‘f1’	               metrics.f1_score	for binary targets
    ‘f1_micro’	           metrics.f1_score	micro-averaged
    ‘f1_macro’         	   metrics.f1_score	macro-averaged
    ‘f1_weighted’	       metrics.f1_score	weighted average
    ‘f1_samples’	       metrics.f1_score	by multilabel sample
    ‘neg_log_loss’	       metrics.log_loss	requires predict_proba support
    ‘precision’ etc.	   metrics.precision_score	suffixes apply as with ‘f1’
    ‘recall’ etc.	       metrics.recall_score	suffixes apply as with ‘f1’
    ‘roc_auc’	           metrics.roc_auc_score

    *Clustering
    ‘adjusted_rand_score’	metrics.adjusted_rand_score

    *Regression
    ‘neg_mean_absolute_error’	metrics.mean_absolute_error
    ‘neg_mean_squared_error’	metrics.mean_squared_error
    ‘neg_median_absolute_error’	metrics.median_absolute_error
    ‘r2’	metrics.r2_score
    '''

    def xg_find_base(self, scoring, data_x, data_y, model_xg, params, overfit=None):
        kfold = KFold(n_splits=5, shuffle=True, random_state=1234)
        params = {}
        params_test1 = {"max_depth": np.arange(3, 11, 1), "min_child_weight": np.arange(3,18,1)}
        clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=-1, scoring=scoring)
        clf.fit(data_x, data_y)
        params.update({'max_depth': clf.best_params_["max_depth"]})
        params.update({'min_child_weight': clf.best_params_["min_child_weight"]})
        model_xg.max_depth = clf.best_params_["max_depth"]
        model_xg.min_child_weight = clf.best_params_["min_child_weight"]
        print(clf.best_params_)
        print("clf.best_score_", clf.best_score_)

#         params_test1 = {"n_estimators": np.arange(30, 100, 10), 'learning_rate': np.arange(0.01, 0.1, 0.01)}
#         clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=1, scoring=scoring)
#         clf.fit(data_x, data_y)
#         params.update({'learning_rate': clf.best_params_["learning_rate"]})
#         params.update({'n_estimators': clf.best_params_["n_estimators"]})
#         model_xg.learning_rate = clf.best_params_["learning_rate"]
#         model_xg.n_estimators = clf.best_params_["n_estimators"]
#         print(clf.best_params_)
#         print("clf.best_score_", clf.best_score_)

        params_test1 = {"learning_rate": [0.01,0.03,0.05,0.07,0.1,0.15,0.2]}
        clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=-1, scoring=scoring)
        clf.fit(data_x, data_y)
        params.update({'learning_rate': clf.best_params_["learning_rate"]})
        model_xg.learning_rate = clf.best_params_["learning_rate"]
        print(clf.best_params_)
        print("clf.best_score_", clf.best_score_)
        
        params_test1 = {"colsample_bytree": np.arange(0.4, 0.95, 0.05), 'subsample': np.arange(0.4, 0.95, 0.05)}
        clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=-1, scoring=scoring)
        clf.fit(data_x, data_y)
        params.update({'colsample_bytree': clf.best_params_["colsample_bytree"]})
        params.update({'subsample': clf.best_params_["subsample"]})
        model_xg.colsample_bytree = clf.best_params_["colsample_bytree"]
        model_xg.subsample = clf.best_params_["subsample"]
        print(clf.best_params_)
        print("clf.best_score_", clf.best_score_)

        params_test1 = {"gamma": np.arange(0, 0.15, 0.01)}
        clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=-1, scoring=scoring)
        clf.fit(data_x, data_y)
        params.update({'gamma': clf.best_params_["gamma"]})
        model_xg.gamma = clf.best_params_["gamma"]
        print(clf.best_params_)
        print("clf.best_score_", clf.best_score_)
        
        params_test1 = {'reg_lambda': np.arange(0, 1.5, 0.1), 'reg_alpha': np.arange(0, 1.5, 0.1)}
        clf = GridSearchCV(model_xg, params_test1, cv=kfold, n_jobs=-1, scoring=scoring)
        clf.fit(data_x, data_y)
        params.update({'reg_alpha': clf.best_params_["reg_alpha"]})
        params.update({'reg_lambda': clf.best_params_["reg_lambda"]})
        model_xg.reg_lambda = clf.best_params_["reg_lambda"]
        model_xg.reg_alpha = clf.best_params_["reg_alpha"]
        print(clf.best_params_)
        print("clf.best_score_", clf.best_score_)

        return model_xg, params


for i in range(1,6):
    file_name_train = 'x.train_{}.txt'.format(i)
    file_name_test = 'x.test_{}.txt'.format(i)
    train = pd.read_table(file_name_train)
    test = pd.read_table(file_name_test)
    
    train = survival_df(train, t_col="time", e_col="status", label_col="Y")
    test = survival_df(test, t_col="time", e_col="status", label_col="Y")
    x_cols = list(train.columns)[1:-1]
    train_1 = train[x_cols]
    test_1 = test[x_cols]
    dtrain = DMatrix(train_1, label=train['Y'].values)
    dtest = DMatrix(test_1, label=test['Y'].values)

    '''
    调参
    '''
    
    model = XGBRegressor()
    gv = GV()
    params = {}

    model, params = gv.xg_find_base('neg_mean_squared_error', train_1,train['Y'].values, model, {},overfit=True)
    print(params)
    early_stop = 30
    verbose_eval = 0
    num_rounds = 500

    watchlist = [(dtrain, 'train'),(dtest,'test')]

    model = xgb.train(params,dtrain, feval=efn_loss,obj=_efn_grads,num_boost_round=num_rounds, early_stopping_rounds=early_stop, evals=watchlist)
    y_pre = model.predict(dtest).reshape([-1,1])
    y_pre
    y_pre_name = 'pre_{}.txt'.format(i)
    np.savetxt(y_pre_name, y_pre) 


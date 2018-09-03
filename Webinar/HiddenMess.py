import pandas as pd
import numpy as np
import pickle

from sympy.parsing.sympy_parser import parse_expr
from sympy import simplify
from sympy.printing.str import StrPrinter

import matplotlib.pyplot as plt
import seaborn as sns


datasets = ['wineRed', 'cpu', 'F05128-f2', 'energyCooling','yacht', 'Chemical-I', 'energyHeating', 'towerData',
            'concrete', 'F05128-f1', 'airfoil','wineWhite', 'Tower']

algorder = ['SymTree', 'IT-ELM (Lasso)', 'IT-ELM (OMP)', 'Ridge', 'Lars','Lasso', 'ElasticNet', 'MLP', 'XGBoost']

def loadEverything():
    f = open('resultsGabriel.pkl','rb')
    df = pickle.load(f)
    f.close()
    
    df = df[(df.Algorithm!='IT-LS') & (~df.Dataset.isin(['ppb','forestfires','bioavailability']))]
    
    medians = df.groupby(['Dataset', 'Algorithm']).median().reset_index() 
    medians['Rank'] = medians.groupby('Dataset')['RMSE'].rank(ascending=True)
    
    return df, medians
    
def highlight_min(s):
    '''
    highlight the maximum in a Series yellow.
    '''
    is_min = s == s.min()
    return ['background-color: yellow' if v else '' for v in is_min]
    
def plot_boxplots(df, dataset):

    plt.figure(figsize=(16,8))

    sns.boxplot(y='RMSE', x='Algorithm', 
                     data=df[(df.Dataset==dataset) & (df.Algorithm!='IT-LS')], 
                     width=0.5,
                     palette="colorblind", order=algorder).set_title(dataset)

def plot_scatterplots(df, dataset):

    plt.figure(figsize=(12,12))

    sns.scatterplot(y='RMSE', x='Complexity', hue='Algorithm',
                            data=df[(df.Dataset==dataset) & (~df.Algorithm.isin(['IT-LS', 'MLP', 'XGBoost']))],
                            palette="colorblind", hue_order=algorder, s = 200).set_title(dataset)
    

def genExprStr(model):
    modelIT      = model.best_estimator_
    nonZeroCoefs = np.nonzero(model.best_estimator_.modelCV.coef_)[0]
    translator   = np.nonzero(model.best_estimator_.maskNonZero_)[0]
    coefs        = model.best_estimator_.modelCV.coef_[nonZeroCoefs]
    

    interIdx     = translator[nonZeroCoefs]
    
    def fiden(x): return x

    f_set = [fiden] + modelIT.f_set
    dictF = [(0,'id'), (1,'cos'), (2,'sqrt'), (3,'sin'), (4,'tan'), (5,'log1p'), (6,'tanh')]

    expr_str = []
    for i, f in dictF:
        mask = np.logical_and((interIdx >= i*modelIT.n_inter), (interIdx < (i+1)*modelIT.n_inter))
        for inter in interIdx[mask]:
            term_str = []
            for nz in np.nonzero(modelIT.network_[inter - i*modelIT.n_inter,:])[0]:
                term_str.append(f'x{nz}**({modelIT.network_[inter - i*modelIT.n_inter,nz]})')
            inter_str = '*'.join(term_str)
            coef  = coefs[0]
            coefs = coefs[1:]
            if i == 0:
                expr_str.append(f'{coef}*{inter_str}')   
            else:
                expr_str.append(f'{coef}*{f}({inter_str})')
    return '+'.join(expr_str)
    
def getModels():    
    dataset = 'cpu'
    alg = 'IT-ELM (OMP)'
    f = open(f'models/{dataset}_{alg}_2_0.pkl','rb')
    modelCPU = pickle.load(f)
    f.close()

    dataset = 'yacht'
    alg = 'IT-ELM (OMP)'
    f = open(f'models/{dataset}_{alg}_4_3.pkl','rb')
    modelYacht = pickle.load(f)
    f.close()

    dataset = 'F05128_f1'
    alg = 'IT-ELM (OMP)'
    f = open(f'models/{dataset}_{alg}_0_0.pkl','rb')
    modelF1 = pickle.load(f)
    f.close()
    
    return modelCPU, modelYacht, modelF1

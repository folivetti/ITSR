import pickle
from os import listdir
import warnings
import sys

import numpy as np
import pandas as pd


from sklearn.model_selection import GridSearchCV
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV, LassoLarsCV, ElasticNetCV, OrthogonalMatchingPursuitCV

from ITELM import ITELM

# ignoramos os resultados NaN das funções pois vamnos zera-los
np.seterr(invalid='ignore')
# nao quero warning de convergência
warnings.filterwarnings('ignore')

# set dos nomes do diretório
fnames = {f.split('-train')[0].split('-test')[0] 
            for f in listdir(path='../datasets') 
            if 'ipynb' not in f and 'keijzer' not in f
         }
print(fnames)

def load_trainTest(dataname, fold):
    X_train = np.loadtxt(f'../datasets/{dataname}-train-{fold}.dat', delimiter=',')
    X_test  = np.loadtxt(f'../datasets/{dataname}-test-{fold}.dat', delimiter=',')
    
    X_train, y_train = X_train[:, :-1], X_train[:,-1]
    X_test, y_test   = X_test[:, :-1], X_test[:,-1]
    
    return X_train, y_train, X_test, y_test
    
def complexity(algname, model):
    if 'XGBoost' in algname:
        return np.sum( [(est[0].feature_importances_ != 0).sum()
                           for est in model.best_estimator_.estimators_
                       ] 
                     )
    elif 'MLP' in algname:
        return np.sum([(coefs != 0).sum() for coefs in model.best_estimator_.coefs_])
    elif 'IT-ELM' in algname:
        return (model.best_estimator_.modelCV.coef_ != 0).sum() # + interactions
    else:
        return (model.coef_ != 0).sum()
        
def run_gridSearch(dataname, fold, model_fn, algname = 'IT-ELM', modelCV = LassoLarsCV()):
    
    dataset_l, algoritmo_l, fold_l  = [], [], []
    mae_l, rmse_l, cplx_l           = [], [], []    
    
    
    for it in range(6):
        
        X_train, y_train, X_test, y_test = load_trainTest(dataname, fold)
        
        # pre-processing
        paramsIT = {'n_inter': (500, 1000, 5000),
                    'exp_range': ((-3,3), (-2,2), (-1,1), (0,2), (0,3)),
                    'modelCV': [modelCV], #[OrthogonalMatchingPursuitCV(n_jobs=-1), LassoLarsCV(n_jobs=-1)], 
                    'maxInt': (3,4,5)
                   }
        clr   = model_fn()
        model = GridSearchCV(clr, paramsIT)
        
        # fit and predict
        model.fit(X_train, y_train)
        yhat = model.predict(X_test)

        # calculate error
        error = y_test - yhat
        mae   = np.abs(error).mean()
        rmse  = np.sqrt(np.square(error).mean())
        
        # calculate complexity
        cplx  = complexity(algname, model)
        
        dataset_l.append(dataname)
        algoritmo_l.append(algname)
        fold_l.append(fold)
        mae_l.append(mae)
        rmse_l.append(rmse)
        cplx_l.append(cplx)
        
        fw = open(f'models/{dataname}_{algname}_{fold}_{it}.pkl', 'wb')
        pickle.dump(model, fw)
        fw.close()
        print(f'it: {it}, {rmse}, {cplx}')
            

    return dataset_l, algoritmo_l, fold_l, mae_l, rmse_l, cplx_l

dataset_l, algoritmo_l, fold_l  = [], [], []
mae_l, rmse_l, cplx_l           = [], [], []

algname  = 'IT-ELM (Lasso)'
modelCV  = LassoLarsCV(n_jobs=-1)
model_fn = ITELM

#for dataname in fnames:       
dataname = sys.argv[1]
fold     = int(sys.argv[2])
print(f'====================\nData set: {dataname}\n====================\n')


dat_l, alg_l, f_l, ab_l, sq_l, cp_l = run_gridSearch(dataname, fold, model_fn, algname, modelCV)

dataset_l   += dat_l
algoritmo_l += alg_l
fold_l      += f_l
mae_l       += ab_l
rmse_l      += sq_l
cplx_l      += cp_l

mse = np.mean(sq_l)
print(f'{algname}: {mse}')
print('\n')

algname  = 'IT-ELM (OMP)'
modelCV  = OrthogonalMatchingPursuitCV(n_jobs=-1)

dat_l, alg_l, f_l, ab_l, sq_l, cp_l = run_gridSearch(dataname, fold, model_fn, algname, modelCV)

dataset_l   += dat_l
algoritmo_l += alg_l
fold_l      += f_l
mae_l       += ab_l
rmse_l      += sq_l
cplx_l      += cp_l

mse = np.mean(sq_l)
print(f'{algname}: {mse}')
print('\n')


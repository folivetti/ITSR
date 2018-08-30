# seguindo tutorial em: http://danielhnyk.cz/creating-your-own-estimator-scikit-learn/

# Criar um objeto compatível com sklearn
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV, LassoLarsCV, ElasticNetCV
from sklearn.base import BaseEstimator, RegressorMixin
import numpy as np
from numba import jit, autojit, njit, prange          # import the decorator

''' 
@njit(parallel=True)
def calcInteraction(X, net):
  H = np.zeros((X.shape[0], net.shape[0]))
  for i in prange(X.shape[0]):
    for j in range(net.shape[0]):      
      if np.any(X[i,:]==0):
        H[i,j] = 0.0
      else:
        H[i,j] = np.prod(X[i,:]**net[j,:])
  return H
'''

class ITELM(BaseEstimator, RegressorMixin):
    """Interaction-Transformation Extreme Machine Learning algorithm"""
    
    # o método que inicializar as configurações da rede
    # , np.sin, np.tan, np.log1p, np.tanh
    def __init__(self, n_inter = 100, f_set = [np.cos, np.sqrt, np.sin, np.tan, np.log1p, np.tanh], exp_range = (0,3), modelCV = LassoLarsCV(), maxInt = 3, seed = None):
        self.n_inter   = n_inter
        self.n_funcs   = len(f_set) + 1
        self.f_set     = f_set
        self.exp_range = exp_range
        self.seed      = seed
        self.modelCV = modelCV
        self.maxInt  = maxInt
    
    def _createNetwork(self, X):
        np.random.seed(self.seed)
        sizeNet = (self.n_inter, X.shape[1])
        min_e, max_e = self.exp_range
        
        self.network_ = np.random.randint(min_e, max_e + 1, size=sizeNet)
        
        for i in range(self.n_inter):
            idx = np.flatnonzero(self.network_[i,:])
            if len(idx) > self.maxInt:
                perm = np.random.permutation(idx)
                self.network_[i, perm[self.maxInt:]] = 0

        #self.network_ = self.network_* np.random.choice([0,1], size=sizeNet, p=[0.7,0.3])
                                   
        self.network_ = np.unique(self.network_, axis=0)
        self.n_inter  = self.network_.shape[0]

        mask  = np.any(X==0, axis=0)
        self.mask0_ = (X==0).sum(axis=0) > 0.7*X.shape[1]

        
        self.network_[:,mask]  = np.absolute(self.network_[:,mask])
        #self.network_[:,self.mask0_] = 0
        
        return self

    def _calcInteraction(self, X, net):
      H = np.zeros((X.shape[0], net.shape[0]))
      for i in range(X.shape[0]):
        H[i,:] = np.prod(X[i,:]**self.network_,axis=1)
      return H
      
    def _transformData(self, X):
        H = np.ndarray((X.shape[0], self.n_funcs*self.n_inter))
        #X[:,self.mask0_] = 1
        
        H[:, :self.n_inter] = self._calcInteraction(X, self.network_)
        for i, f in enumerate(self.f_set):
            idx_st  = (i+1)*self.n_inter
            idx_end = (i+2)*self.n_inter
            H[:, idx_st:idx_end] = f(H[:,:self.n_inter])        

        maskNan         = np.any(np.isnan(H), axis=0)
        maskInf         = np.any(np.isinf(H), axis=0)
        maskBig         = np.any(np.abs(H) > 1e+100, axis=0)

        H[:, maskNan]   = 0
        H[:, maskInf]   = 0
        H[:, maskBig]   = 0

        return H
    
    # gera o modelo
    def fit(self, X, y):
        self._createNetwork(X)
        
        H                 = self._transformData(X)
        self.maskNonZero_ = np.any(H!=0, axis=0)
        self.maskNonZero_ = np.logical_and(self.maskNonZero_, np.absolute(H.std(axis=0)) > 1e-8)
        
        
        if np.sum(self.maskNonZero_) <= 1:
          Hmask = np.random.random((X.shape[0], 2)) # fit noise for now
        else:
          Hmask = H[:,self.maskNonZero_]
        
#        print(X[0,:]**self.network_[0,:])
#        print(X[0,:]**self.network_[1,:])
#        print(Hmask.shape, y.shape, self.n_inter, self.exp_range, Hmask[0,:10])#(corr==0).sum())
        self.modelCV.fit(Hmask, y)
        
        return self
    
    # gera uma predição
    def predict(self, X):
        try:
            getattr(self, "maskNonZero_")
        except AttributeError:
            raise RuntimeError("You must train regressor before predicting data!")
        
        H = self._transformData(X)
        if np.sum(self.maskNonZero_) <= 1:
          Hmask = np.random.random((X.shape[0], 2)) # fit noise for now
        else:
          Hmask = H[:,self.maskNonZero_]
        
        return self.modelCV.predict(Hmask)
    
    # calcula o score
    def score(self, X, y):
        try:
            getattr(self, "maskNonZero_")
        except AttributeError:
            raise RuntimeError("You must train regressor before predicting data!")
        
        H = self._transformData(X)
        if np.sum(self.maskNonZero_) <= 1:
          Hmask = np.random.random((X.shape[0], 2)) # fit noise for now
        else:
          Hmask = H[:,self.maskNonZero_]
        
        if self.modelCV is None:
            return 0.0
        else:
            return self.modelCV.score(Hmask, y)

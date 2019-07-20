from optparse import OptionParser
import numpy as np
from RecursiveSymbolicRegression import RecursiveSymbolicRegression
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV

parser = OptionParser()
parser.add_option("--train", dest="train_fname", help="training data set in CSV format", metavar="FILE")
parser.add_option("--test", dest="test_fname", help="test data set in CSV format", metavar="FILE", default=None)
parser.add_option("--thr", dest="thr", help="term cutoff threshold (defult=1e-4)", default=1e-4, type="float")
parser.add_option("--inter", dest="inter", help="Iterations with just positive interactions (first phase - default=8)", default=8, type="int")
parser.add_option("--inv", dest="inv", help="Iterations with positive and negative interactions (second phase - default=5)", default=5, type="int")
parser.add_option("--trans", dest="trans", help="Iterations with positive and negative interactions and transformation swap (third phase - default=3)", default=3, type="int")
parser.add_option("--reg", dest="reg", help="Linear Regression Model to use: lr: Linear Regression, l1: L1-reg, l2: L2-reg", default="lr")

def pSqRoot(x):
    return np.sqrt(np.abs(x))

def main():
    (options, rags) = parser.parse_args()
    Z_train = np.loadtxt(options.train_fname)
    if options.test_fname is not None:
        Z_test = np.loadtxt(options.test_fname)
    else:
        Z_test = Z_train.copy()

    X_train, y_train = Z_train[:, :-1], Z_train[:, -1]
    X_test, y_test = Z_test[:, :-1], Z_test[:, -1]

    # change this line to use the functions you want
    functions = [np.sin, np.cos, np.tan, pSqRoot, np.log1p, np.log]

    if options.reg == "lr":
        model = LinearRegression
    elif options.reg == "l1":
        model = LassoCV
    elif options.reg == "l2":
        model = RidgeCV
    else:
        model = LinearRegression

    sr = RecursiveSymbolicRegression(LinearModel=model, functions=functions)
    sr.fit(X_train, y_train, options.inter, options.inv, options.trans, options.thr)
    y_pred = sr.predict(X_test)
    mae = np.abs(y_pred-y_test).mean()
    mse = np.square(y_pred-y_test).mean()

    print(f"Test MAE = {mae}, Test MSE = {mse}")

if __name__ == "__main__":
    main()

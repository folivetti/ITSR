from itertools import product
from RecursiveSymbolicRegression import RecursiveSymbolicRegression

def build_function(dim, order, base):
    
    # idx -> [(idx, exp) for idx in {0..dim-1}]
    base_f = {}
    for d in range(dim):
        base_f[d] = []
        for o in range(order+1):
            base_f[d].append((d,o))

    # basis = [ [(0,exp),...,(dim-1,exp)] for product() ]
    basis = [ list(p) for p in product(*base_f.values()) ]
    basis = [ b for b in basis if sum([e for i,e in b]) <= order ]
    basis = [ tuple([(i,e) for i,e in b if e!=0]) for b in basis ]
    basis = [ b for b in basis if len(b) ]
    basis = list(set(basis))
    
    basisO = [b for b in basis if sum([e for i,e in b])==order]
    
    expr = [list(basisO[np.random.choice(len(basisO))])]
    basis = [b for b in basis if b != tuple(expr[0])]
    if len(basis) > 0:
        expr = expr + [list(basis[idx]) for idx in np.random.choice(len(basis), base-1, replace=False)]
    
    expr = [(np.random.uniform(-1,1), term) for term in expr]
    finalE =   '+'.join([
                 '{}*'.format(c) +
                '*'.join(['(X[:,{}]**{})'.format(term, exp)  for term, exp in e])
                for c, e in expr
            ]) + '+{}'.format(np.random.uniform(-1,1))
    def f(X):
        return eval(finalE, {'X':X} )
    return f, finalE

def main():
    dims = (1, 2, 3, 10)
    orders = (1,2,3,4)
    basis = (1,2,3,4)

    for dim, order, base in product(dims, orders, basis):
        if base > order:
            continue
        correct = 0
        for it in range(30):
            f, expr = build_function(dim, order, base)
            X = np.random.uniform(0,1,(4000,dim))
            y = f(X)
            
            sr = RecursiveSymbolicRegression(LinearModel=LinearRegression, functions=[])
            sr.fit(X[:2500,:], y[:2500], 3,0,0)
            y_pred = sr.predict(X[2500:,:])
            
            mae = np.abs(y_pred - y[2500:]).mean()
            nterms = sr.terms
            
            if sr.terms == base and mae < 1e-6:
                correct = correct + 1
            else:
                print(expr)
                if len(sr.fitcoef) < 8:
                    print(sr.printExpr(sr.fitexpr, sr.fitcoef, sr.fitbias), mae)
            print(correct)
        print(dim, order, base, correct)
        results2[dim][order][base] = correct
        
main()        

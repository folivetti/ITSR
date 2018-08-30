# Interaction-Transformation Symbolic Regression

Interaction-Transformation is a Data Structure created by (de Franca, 2017) as a representation of mathematical expressions for non-linear regression problems.

The main idea is to restrict these expressions to the form:

$$f(x) = \sum_{i=1}^{n}{w_i \cdot trans_i(inter_i(x))},$$

with $trans_i$ being a transformation function of type $\Re \rightarrow \Re$ and $inter_i$ an interaction function of the type $\Re^d \rightarrow \Re$, $w_i$ is the importance weight of the i-th term.

The main advantage of this representation when compared to the more common Expression Tree is that it allows only the generation of simpler expressions while still being capable of generating a good fit for many regression problems.

# Algorithms: 

- Greedy Tree Search for Symbolic Regression (SymTree)

# Webinar Binder:

[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/folivetti/ITSR/master?filepath=%2FWebinar%2FExplorer.ipynb)

# References:

de Franca, F. O. A Greedy Search Tree Heuristic for Symbolic Regression. Information Science, 2017.

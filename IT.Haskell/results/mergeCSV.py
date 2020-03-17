import pandas as pd
import numpy as np

datasets = ["airfoil", "concrete", "energyCooling", "energyHeating", "towerData", "wineRed", "wineWhite", "yacht"]

dfs = []

for dname in datasets:
    df = pd.read_csv(f"{dname}/stats.csv")
    print(df.RMSE_test.size)
    df["Dataset"] = [dname]*30
    df["Algorithm"] = ["ITEA"]*30
    df["Fold"] = [0]*30
    df["Length"] = [0]*30
    df["Lengths"] = [0]*30
    dfs.append(df)

dfAll = pd.concat(dfs, sort=False)
dfAll.to_csv("resultsITEA.csv", index=False)

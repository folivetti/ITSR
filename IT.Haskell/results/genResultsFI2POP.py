import pandas as pd
import numpy as np

datasets = ["aircraft", "aircraft_noise", "F05128_dyn", "F05128_stat", "FlowPsi", "FlowPsi_noise", "FlowStress", "RocketFuel", "RocketFuel_noise"]
datasets = ["F05128_dyn"]

folder = "NoSlice/"

print("Dataset \t|\t Interval \t|\t Slicing")

for dname in datasets:
    if len(dname) > 8:
        print(dname, end = " \t|\t ")
    else:
        print(dname, end = " \t|\t ")
    df = pd.read_csv(f"{folder}{dname}/stats.csv")
    mu = np.round(df.NMSE_train.mean()*100, 4)
    st = np.round(df.NMSE_train.std()*100, 4)
    print(f"{mu} ± {st}", end=" \t|\t ")

    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.NMSE_train.mean()*100, 4)
    st = np.round(df.NMSE_train.std()*100, 4)
    print(f"{mu} ± {st}")

print("\n")

print("Dataset \t|\t Interval \t|\t Slicing")

for dname in datasets:
    if len(dname) > 8:
        print(dname, end = " \t|\t ")
    else:
        print(dname, end = " \t|\t ")
    df = pd.read_csv(f"{folder}{dname}/stats.csv")
    mu = np.round(df.NMSE_test.mean()*100, 4)
    st = np.round(df.NMSE_test.std()*100, 4)
    print(f"{mu} ± {st}", end=" \t|\t ")

    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.NMSE_test.mean()*100, 4)
    st = np.round(df.NMSE_test.std()*100, 4)
    print(f"{mu} ± {st}")

print("\n")

print("Dataset \t|\t Interval \t|\t Slicing")
for dname in datasets:
    if len(dname) > 8:
        print(dname, end = " \t|\t ")
    else:
        print(dname, end = " \t|\t ")
    df = pd.read_csv(f"{folder}{dname}/stats.csv")
    mu = np.round(df.NMSE_test.min()*100, 4)
    print(f"{mu}", end=" \t|\t ")

    df = pd.read_csv(f"{dname}/stats.csv")
    mu = np.round(df.NMSE_test.min()*100, 4)

    print(f"{mu}")




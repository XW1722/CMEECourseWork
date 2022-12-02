__author__ = "Xuan Wang (xuan.wang22@imperial.ac.uk)"
__date__ = "27 Nov 2022"

# import packages
import pandas as pd

# load data
data_subset = pd.read_csv("../data/LogisticGrowthData.csv")
pd.read_csv("../data/LogisticGrowthMetaData.csv")

# ensure the time records are all non-negative
data_subset["Time"] = abs(data_subset["Time"])
data_subset["PopBio"] = abs(data_subset["PopBio"])

# insert single growth curve by combining species, medium, temp and citation columns
data_subset.insert(0, "ID", data_subset.Species + "_" + data_subset.Temp.map(str) + "_" + data_subset.Medium + "_" + data_subset.Citation)

# factorisation for subset analysis
data_subset.fact_ID = pd.factorize(data_subset.ID)

# save the data
data_subset.to_csv("../data/data_subset.csv")
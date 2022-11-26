# import packages
import pandas as pd
import scipy as sc
import numpy as np
import matplotlib.pylab as plt
import seaborn as sns 
from lmfit import Minimizer, Parameters, report_fit


# load data
data = pd.read_csv("../data/LogisticGrowthData.csv")
data_meta = pd.read_csv("../data/LogisticGrowthMetaData.csv")
print("Loaded {} columns.".format(len(data.columns.values)))

# insert single growth curve by combining species, medium, temp and citation columns
data.insert(0, "ID", data.Species + "_" + data.Temp.map(str) + "_" + data.Medium + "_" + data.Citation)
data_subset = data[data['ID']=='Chryseobacterium.balustinum_5_TSB_Bae, Y.M., Zheng, L., Hyun, J.E., Jung, K.S., Heu, S. and Lee, S.Y., 2014. Growth characteristics and biofilm formation of various spoilage bacteria isolated from fresh produce. Journal of food science, 79(10), pp.M2072-M2080.'] # replace with numbers

# plot the graph of the data
sns.lmplot(x= "Time", y = "PopBio", data = data_subset, fit_reg = False)
t = np.array(data['Time'])

#################################
# fit linear model to the data
#################################

######### NLLS fitting ##########
parm_linear = Parameters()
parm_linear.add('a', value = 1)
parm_linear.add('b', value = 1)
parm_linear.add('c', value = 1)
parm_linear.add('d', value = 1)

def residuals_linear(params, t, data):
    """Calculating cubic growth and substract data"""
    v = params.valuesdict()
    lin_model = v['a'] * t ** 3 + v['b'] * t ** 2 + v['c'] * t  + v['d']
    return lin_model - data

mini = Minimizer(residuals_linear, parm_linear, fcn_args = (data_subset["Time"], data_subset["PopBio"]))
fit_linear_NLLS = mini.minimize()

# summary of the fit
report_fit(fit_linear_NLLS)

######### OLS fitting ###########
fit_linear_OLS = np.polyfit(data_subset["Time"], data_subset["PopBio"], 3)
print(fit_linear_OLS)

######### Comparison between OLS and NLLS fitting ##########

print(fit_linear_NLLS.params)

par_dict = fit_linear_NLLS.params.valuesdict().values()
par = np.array(list(par_dict))
print(fit_linear_OLS - par)

poly_eqn = np.poly1d(fit_linear_OLS)
predicted_values = poly_eqn(data_subset["Time"])
residuals = predicted_values - data_subset["PopBio"]

##################################
###### Non-linear model fit ######
##################################

# calculate the growth rate from the linear fit
parm_logistic = Parameters()
parm_logistic.add('N_0', value = data_subset["PopBio"][0])
parm_logistic.add('N_max', value = list(data_subset["PopBio"])[-1])

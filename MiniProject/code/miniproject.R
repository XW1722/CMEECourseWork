# import the required packages
library(ggplot2)
require("minpack.lm")

# load dataset
data_subset <- read.csv("../results/data_subset.csv")
t <- as.numeric(data_subset$Time)
pop <- as.numeric(data_subset$PopBio)
log_pop <- log(pop)
data_subset$log_pop <- log_pop

################################
##### fitting linear model #####
################################

# quadratic
quadratic_model <- lm(log_pop ~ poly(Time, 2), data = data_subset)
quadratic_predict <- predict.lm(quadratic_model, data.frame(Time = t))

df1 <- data.frame(t, quadratic_predict)
df1$model <- "Linear_Quadratic"
names(df1) <- c("Time", "LogN", "model")

# cubic
cubic_model <- lm(log_pop ~ poly(Time, 3), data = data_subset)
cubic_predict <- predict.lm(cubic_model, data.frame(Time = t))

df2 <- data.frame(t, cubic_predict)
df2$model <- "Linear_Cubic"
names(df2) <- c("Time", "LogN", "model")

###############################
####### Buchanan model ########
###############################

# function for Buchanan model
# lag phase - t<=tlag: logNt = logN0
# exponential growth phase - tlag < t < tmax: logNt = logN0 + k(t - tlag)
# stationary phase - t>=tmax: logNt = logNmax + k(tmax - t)
buchanan_model <- function(t, t_lag, t_max, N_0, k){
    return(N_0 * I(t <= t_lag) + (N_0 + k * (t - t_lag)) * I(t > t_lag & t < t_max) + (N_0 + k * (t_max - t_lag)) * I(t >= t_max))
}

# defining starting parameters
k <- 0.004
N_0 <- min(log_pop)
t_lag <- data_subset$Time[which.max(diff(diff(data_subset$log_pop)))]
t_max <- data_subset$Time[which.max(diff(diff(data_subset$log_pop[data$Time > t_lag_start])))]

# fitting the model
fit_buchanan <- nlsLM(log(PopBio) ~ buchanan_model(t = Time, t_lag, t_max, N_0, k),
                        data = data_subset,
                        start = list(N_0 = N_0_start,
                        t_lag = t_lag_start,
                        t_max = t_max_start))
summary(fit_buchanan)

buchanan_points <- buchanan_model(t,
                    t_lag = coef(fit_buchanan)["t_lag"],
                    t_max = coef(fit_buchanan)["t_max"],
                    N_0 = coef(fit_buchanan)["N_0"],
                    k)

df3 <- data.frame(t, buchanan_points)
df3$model <- "Buchanan model"
names(df3) <- c("Time", "LogN", "model")

##################################
########## Logistic model ########
##################################

lm_growth <- lm(log(PopBio) ~ Time, data = data_subset)
summary(lm_growth) # to approximate the start value of growth rate

logistic_model <- function(t, r_max, K, N_0){
    return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

# defining starting parameters
N_0_start <- min(log_pop)
K_start <- max(log_pop)
r_max_start <- 0.004

fit_logistic <- nlsLM(log(PopBio) ~ logistic_model(t = Time, r_max, K, N_0),
    data_subset, list(r_max = r_max_start, N_0 = N_0_start, K = K_start))
summary(fit_logistic)

logistic_points <- logistic_model(t, 
                    r_max = coef(fit_logistic)["r_max"],
                    K = coef(fit_logistic)["K"],
                    N_0 = coef(fit_logistic)["N_0"])

df4 <- data.frame(t, logistic_points)
df4$model <- "Logistic equation"
names(df4) <- c("Time", "LogN", "model")

##########################
##### Gompertz model ##### (Zwietering)
##########################

# functon for Gompertz model
gompertz_model <- function(t, r_max, K, N_0, t_lag){
    return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
}

# starting value
N_0_start <- min(log_pop)
K_start <- max(log_pop)
r_max_start <- 0.004
t_lag_start <- data_subset$Time[which.max(diff(diff(data_subset$log_pop)))]

# fitting the model
fit_gompertz <- nlsLM(log_pop ~ gompertz_model(t = Time, r_max, K, N_0, t_lag), data_subset,
                    list(t_lag = t_lag_start, r_max = r_max_start, N_0 = N_0_start, K = K_start))

summary(fit_gompertz)

gompertz_points <- gompertz_model(t,
                    r_max = coef(fit_gompertz)["r_max"],
                    K = coef(fit_gompertz)["K"],
                    N_0 = coef(fit_gompertz)["N_0"],
                    t_lag = coef(fit_gompertz)["t_lag"])

df5 <- data.frame(t, gompertz_points)
df5$model <- "Gompertz model"
names(df5) <- c("Time", "LogN", "model")

###############################
####### Baranyi model #########
###############################

# function for Baranyi model

baranyi_model <- function(t, y_0, y_max, r_max, h_0, v){
    return(y_0 + r_max * t + (1/r_max) * (log(exp(-v * t) + exp(-h_0) - exp(-v * t - h_0))) - log(1 + (exp(r_max * t) + (1/r_max) * log(exp(-v*t)+exp(-h_0)+exp(-v*t - h_0)) - 1) / exp(y_max - y_0)))
}

# y0 = ln(x0), ymax = ln(xmax), x0 is the initial and xmax the asymptotic cell concentration
# yt = ln(xt), xt the cell concentration, PopBio
# mumax, maximum secific growth rate
# m, curvature parameter to characterize the transition from the exponential phase
# h0, dimensionless parameter quantifying the initial physiological state of the cells

# starting value
y_0_start <- min(log_pop)
y_max_start <- max(log_pop)
r_max_start <- 0.004
h_0_start <- (log_pop[28] - log_pop[29]) / (t[28] - t[29]) # the initial growth rate

# fitting the model using the defined starting values
fit_baranyi <- nlsLM(log_pop ~ baranyi_model(t = Time, y_0, y_max, r_max,
    h_0, v = r_max), data = data_subset,
                    start = list(y_0 = y_0_start,
                    y_max = y_max_start,
                    r_max = r_max_start,
                    h_0 = h_0_start))
summary(fit_baranyi)

baranyi_points <- baranyi_model(t,
                    y_0 = coef(fit_baranyi)["y_0"],
                    y_max = coef(fit_baranyi)["y_max"],
                    r_max = coef(fit_baranyi)["r_max"],
                    h_0 = coef(fit_baranyi)["h_0"],
                    v = coef(fit_baranyi)["r_max"])

df6 <- data.frame(t, baranyi_points)
df6$model <- "Baranyi model"
names(df6) <- c("Time", "LogN", "model")

##################################
###### Comparing the models ######
##################################

model_frame <- rbind(df1, df2, df3, df4, df5, df6)

# generation of the graph
p <- ggplot(data_subset, aes(x = t, y = log_pop)) +
    geom_point(size = 3) +
    geom_line(data = model_frame, aes(x = Time, y = LogN, col = model),
        linewidth = 1) +
    theme(aspect.ratio = 1) +
    labs(x = "Time", y = "Cell number")

# AIC and BIC values of the fitted models
AIC <- data.frame("AIC", AIC(quadratic_model), AIC(cubic_model),
    AIC(fit_buchanan), AIC(fit_baranyi), AIC(fit_logistic), AIC(fit_gompertz))
BIC <- data.frame("BIC", BIC(quadratic_model), BIC(cubic_model),
    BIC(fit_buchanan), BIC(fit_baranyi), BIC(fit_logistic), BIC(fit_gompertz))
names(AIC) <- c("Criteria", "Quadratic", "Cubic", "Buchanan",
    "Baranyi", "Logistic", "Gompertz")
names(BIC) <- c("Criteria", "Quadratic", "Cubic", "Buchanan", 
    "Baranyi", "Logistic", "Gompertz")
comparison <- rbind(AIC, BIC)
write.csv(comparison, "../results/model_comparison.csv",
    row.names = TRUE)


# CMEE Miniproject - R script
# Author: Xuan Wang (xuan.wang22@imperial.ac.uk)
# Date: 4 Dec 2022
# Description: This script includes the model fitting and plotting. The dataset generated from dataprocess.py is used.


###################################################################
# Prerequisite 
###################################################################

# clean up
rm(list = ls())

# import the required packages
require("ggplot2")
require("stats")
require("minpack.lm")

# load dataset
data_subset <- read.csv("../results/data_subset.csv")

# data preparation
t <- as.numeric(data_subset$Time)
data_subset$log_pop <- log(data_subset$PopBio)

# factorizing the the dataframe for subsets
data_subset$ID <- as.factor(data_subset$ID)

###################################################################
# Definition of models
###################################################################

# Buchanan model
buchanan_model <- function(t, t_lag, t_max, N_max, N_0, r_max){
    t_max <- t_lag + ((N_max - N_0) / r_max)
    return(N_0 * I(t <= t_lag) + (N_0 + r_max * (t - t_lag)) * I(t > t_lag & t < t_max)
     + (N_max) * I(t >= t_max) & (t >= t_lag))
}

# Logistic model
logistic_model <- function(t, r_max, N_max, N_0){
    return(N_0 * N_max * exp(r_max * t)/(N_max + N_0 * (exp(r_max * t) - 1)))
}

# modified Gompertz model
gompertz_model <- function(t, r_max, N_max, N_0, t_lag){
    return(N_0 + (N_max - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((N_max - N_0) * log(10)) + 1)))
}

# Baranyi model
baranyi_model <- function(t, N_0, N_max, r_max, t_lag){
    return(N_0 + r_max * (t + (1/r_max) * log(exp(-r_max * t)
    + exp(-r_max * t_lag) - exp(-r_max * (t + t_lag))))
    -log(1+(exp(r_max*t + log(exp(-r_max*t+exp(-r_max * t_lag)-exp(-r_max*(t+t_lag)))))-1)
    / exp(N_max - N_0)))
}

# defining AIC and BIC calculation
AIC_func <- function(n, RSS, p){
    return(n + 2 + n * log((2 * pi) / n) + n * log(RSS) + 2 * p)
}
BIC_func <- function(n, RSS, p){
    return(n + 2 + n * log((2 * pi) / n) + n * log(RSS) + p * log(n))
}
AIC_small <- function(aic, n, RSS){
    return(n + 2 + n * log((2 * pi) / n) + n * log(RSS))
}

# Initialization and pre-allocation
count <- 0
plot_list <- vector("list")
AIC_list <- vector("list")
BIC_list <- vector("list")

###############################################################################
# Model fitting
###############################################################################

for (i in unique(data_subset$ID)){
    # Initialise
    sub <- subset(data_subset, data_subset$ID == i)
    N_0_start <- min(sub$log_pop)
    N_max_start <- max(sub$log_pop)
    n_row <- nrow(sub)
    t <- seq(min(sub$Time), max(sub$Time), len=200)
    count <- count + 1

    # calculation of maximum growth rate
    r_max_start <- 0; 
    for (x in 2:length(sub$Time)){
        roll <- (sub[x, "log_pop"] - sub[x-1, "log_pop"])/(sub[x, "Time"] - sub[x-1, "Time"])
        if (roll > r_max_start & roll != "NaN"){
            r_max_start <- roll;
            loc <- x}
    }
    r_max_start <- as.numeric(r_max_start)
    t_lag_start <- sub[loc, "Time"] - (sub[loc, "log_pop"] - N_0_start) / r_max_start

    # quadratic model
    quadratic_model <- lm(PopBio ~ poly(Time, 2), data = sub)
    quadratic_predict <- predict.lm(quadratic_model, data.frame(Time = t))

    # cubic model
    cubic_model <- lm(PopBio ~ poly(Time, 3), data = sub)
    cubic_predict <- predict.lm(cubic_model, data.frame(Time = t))
    
    # buchanan model
    fit_buchanan <- try(nlsLM(log(PopBio) ~ buchanan_model(t = Time, t_lag, t_max, N_0, r_max),
                    data = subset,
                    start = list(
                    t_lag = t_lag_start,
                    t_max = t_max_start,
                    N_0 = N_0_start,
                    r_max = r_max_start),
                    control = nls.lm.control(200)),
                    silent = TRUE)

    # logistic model
    fit_logistic <- try(nlsLM(log(PopBio) ~ logistic_model(t = Time, r_max, N_max, N_0),
        data = sub, 
        start = list(r_max = r_max_start, N_0 = N_0_start, N_max = N_max_start),
        control = list(maxiter = 200)), silent = TRUE)

    # gompertz model
    fit_gompertz <- try(nlsLM(log(PopBio) ~ gompertz_model(t = Time, r_max, N_max, N_0, t_lag), 
                        data = sub,
                        start = list(r_max = r_max_start, N_0 = N_0_start, 
                        N_max = N_max_start,
                        t_lag = t_lag_start),
                        control = nls.lm.control(maxiter = 200)),
                        silent = TRUE)

    # baranyi model
    fit_baranyi <- try(nlsLM(log(PopBio) ~ baranyi_model(t = Time, N_0, N_max, r_max, 
                        t_lag), 
                        data = sub,
                        start = list(N_0 = N_0_start,
                        N_max = N_max_start,
                        r_max = r_max_start,
                        t_lag = t_lag_start),
                        control = nls.lm.control(maxiter = 200)),
                        silent = TRUE)

    # Model Comparison
    # AIC and BIC generation
    if (class(quadratic_model) == 'lm'){
        p_quadratic <- length(coef(quadratic_model))
        residuals_qua <- (residuals(quadratic_model))^2
        residuals_qua[is.nan(residuals_qua)] <- 0
        RSS_quadratic <- sum(log(residuals_qua))
        AIC_quadratic <- AIC_func(n_row, RSS_quadratic, p_quadratic)
        # check if the sample size is small
        if(length(quadratic_model) < 10){
            AIC_quadratic <- AIC_small(AIC_quadratic, n_row, RSS_quadratic)
        }
        BIC_quadratic <- BIC_func(n_row, RSS_quadratic, p_quadratic)
    } else {
        AIC_quadratic <- "NA"
        BIC_quadratic <- "NA"
    }

    if (class(cubic_model) == 'lm'){
        p_cubic <- length(coef(cubic_model))
        residuals_cubic <- (residuals(cubic_model))^2
        RSS_cubic <- sum(log(residuals_cubic))
        AIC_cubic <- AIC_func(n_row, RSS_cubic, p_cubic)
        if(length(cubic_model) < 10){
            AIC_cubic <- AIC_small(AIC_cubic, n_row, RSS_cubic)
        }
        BIC_cubic <- BIC_func(n_row, RSS_cubic, p_cubic)
    } else {
        AIC_cubic <- "NA"
        BIC_cubic <- "NA"
    }

    if (class(fit_logistic) == 'nls'){
        p_logistic <- length(coef(fit_logistic))
        residuals_logistic <- (residuals(fit_logistic))^2
        RSS_logistic <- sum(residuals_logistic)
        AIC_logistic <- AIC_func(n_row, RSS_logistic, p_logistic)
        if(length(fit_logistic) < 10){
            AIC_logistic <- AIC_small(AIC_logistic, n_row, RSS_logistic)
        }
        BIC_logistic <- BIC_func(n_row, RSS_logistic, p_logistic)
    } else {
        AIC_logistic <- "NA"
        BIC_logistic <- "NA"
    }

    if (class(fit_gompertz) == 'nls'){
        p_gompertz <- length(coef(fit_gompertz))
        residuals_gompertz <- (residuals(fit_gompertz))^2
        RSS_gompertz <- sum(residuals_gompertz)
        AIC_gompertz <- AIC_func(n_row, RSS_gompertz, p_gompertz)
        if(length(fit_gompertz) < 10){
            AIC_gompertz <- AIC_small(AIC_gompertz, n_row, RSS_gompertz)
        }
        BIC_gompertz <- BIC_func(n_row, RSS_gompertz, p_gompertz)
    } else {
        AIC_gompertz <- "NA"
        BIC_gompertz <- "NA"
    }

    if (class(fit_baranyi) == 'nls'){
        p_baranyi <- length(coef(fit_baranyi))
        residuals_baranyi <- (residuals(fit_baranyi))^2
        RSS_baranyi <- sum(residuals_baranyi)
        AIC_baranyi <- AIC_func(n_row, RSS_baranyi, p_baranyi)
        if(length(fit_baranyi) < 10){
            AIC_baranyi <- AIC_small(AIC_baranyi, n_row, RSS_baranyi)
        }
        BIC_baranyi <- BIC_func(n_row, RSS_baranyi, p_baranyi)
    } else {
        AIC_baranyi <- "NA"
        BIC_baranyi <- "NA"
    }

    if (class(fit_buchanan) == 'nls'){
        p_buchanan <- length(fit_buchanan)
        residuals_buchanan <- (residuals(fit_buchanan))^2
        RSS_buchanan <- sum(residuals_buchanan)
        AIC_buchanan <- AIC_func(n_row, RSS_buchanan, p_buchanan)
        if(length(fit_buchanan) < 10){
            AIC_buchanan <- AIC_small(AIC_buchanan, n_row, RSS_buchanan)
        }
        BIC_buchanan <- BIC_func(n_row, RSS_buchanan, p_buchanan)
    } else {
        AIC_buchanan <- "NA"
        BIC_buchanan <- "NA"
    }    

    # create dataframe for models
    df1 <- data.frame(t, log(quadratic_predict))
    df1$model <- "Quadratic"
    names(df1) <- c("Time", "pop", "model")

    df2 <- data.frame(t, log(cubic_predict))
    df2$model <- "Cubic"
    names(df2) <- c("Time", "pop", "model")

    if (AIC_logistic != "NA"){
        logistic_points <- logistic_model(t, 
                    r_max = coef(fit_logistic)["r_max"],
                    N_max = coef(fit_logistic)["N_max"],
                    N_0 = coef(fit_logistic)["N_0"])
        df3 <- data.frame(t, logistic_points)
        df3$model <- "Logistic"
        names(df3) <- c("Time", "pop", "model")
    }

    if (AIC_gompertz != "NA"){
        gompertz_points <- gompertz_model(t, 
                    r_max = coef(fit_gompertz)["r_max"],
                    N_max = coef(fit_gompertz)["N_max"],
                    N_0 = coef(fit_gompertz)["N_0"],
                    t_lag = coef(fit_gompertz)["t_lag"])
        df4 <- data.frame(t, gompertz_points)
        df4$model <- "Gompertz"
        names(df4) <- c("Time", "pop", "model")
    }

    if (AIC_baranyi != "NA"){
        baranyi_points <- baranyi_model(t, 
                    N_max = coef(fit_baranyi)["N_max"],
                    N_0 = coef(fit_baranyi)["N_0"],
                    r_max = coef(fit_baranyi)["r_max"],
                    t_lag = coef(fit_baranyi)["t_lag"])
        df5 <- data.frame(t, baranyi_points)
        df5$model <- "Baranyi"
        names(df5) <- c("Time", "pop", "model")
    } else {
        NA_5 <- rep(NA, length(t))
        df5 <- data.frame(t, NA_5)
        df5$model <- "Baranyi"
        names(df5) <- c("Time", "pop", "model")
    }

    if (AIC_buchanan != "NA"){
        buchanan_points <- buchanan_model(t,
                    t_lag = coef(fit_buchanan)["t_lag"],
                    t_max = coef(fit_buchanan)["t_max"],
                    N_max = coef(fit_buchanan)["N_max"],
                    N_0 = coef(fit_buchanan)["N_0"],
                    r_max = coef(fit_buchanan)["r_max"])
        df6 <- data.frame(t, buchanan_points)
        df6$model <- "Buchanan"
        names(df6) <- c("Time", "pop", "model")
    } else {
        NA_6 <- rep(NA, length(t))
        df6 <- data.frame(t, NA_6)
        df6$model <- "Buchanan"
        names(df6) <- c("Time", "pop", "model")
    }

    AIC_list[[i]] <- c(AIC_quadratic, AIC_cubic, AIC_logistic, AIC_gompertz, 
                    AIC_baranyi)
    BIC_list[[i]] <- c(BIC_quadratic, BIC_cubic, BIC_logistic, BIC_gompertz,
                    BIC_baranyi)

    # Creating plots
    results <- rbind(df1, df2, df3, df4, df5, df6)
    p <- ggplot(data = sub, aes(x = Time, y = log_pop)) +
        geom_point(size = 3) +
        geom_line(data = results, aes(x = Time, y = pop, col = model), size = 1) +
        theme_bw() +
        theme(aspect.ratio = 1) +
        labs(x = "Time", y = "log(Abundance)")
    plot_list[[i]] <- p
}

# save and export data
AIC_data <- data.frame(do.call(rbind, AIC_list))
names(AIC_data) <- c("quadratic", "cubic", "logistic", "gompertz", "baranyi")
BIC_data <- data.frame(do.call(rbind, BIC_list))
names(BIC_data) <- c("quadratic", "cubic", "logistic", "gompertz", "baranyi")

AIC_compare <- colnames(AIC_data)[as.numeric(apply(AIC_data, 1, which.min))]
BIC_compare <- colnames(BIC_data)[as.numeric(apply(BIC_data, 1, which.min))]

AIC_plot <- ggplot(data.frame(AIC_compare), aes(x = AIC_compare)) +
                geom_bar()
BIC_plot <- ggplot(data.frame(BIC_compare), aes(x = BIC_compare)) +
                geom_bar()

write.csv(AIC_data, "../results/AIC_comparison.csv")
write.csv(BIC_data, "../results/BIC_comparison.csv")

pdf("../results/plot_subsets.pdf")
print(plot_list)
graphics.off()

pdf("../results/AIC_plot.pdf")
print(AIC_plot)
graphics.off()

pdf("../results/BIC_plot.pdf")
print(BIC_plot)
graphics.off()

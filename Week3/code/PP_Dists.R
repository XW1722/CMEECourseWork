# loading data
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# loading packages
require(ggplot2)
require(tidyverse)
require(dplyr)

# changing certain column to factor
MyDF$Type.of.feeding.interaction <- as.factor(MyDF$Type.of.feeding.interaction)
str(MyDF)
category <- unique(MyDF$Type.of.feeding.interaction)
length(category)

# computing the size ratio of prey mass over predator mass by feeding interaction type
ratio <- function(prey, predator){
    result <- prey/predator
    return(result)
}

MyDF$ratio <- ratio(MyDF$Prey.mass, MyDF$Predator.mass)

# pred subplots
pdf("../results/Pred_Subplots.pdf", width = 11, height = 8)
n = 1
par(mfrow = c(5, 1))
mean_pred = c()
median_pred = c()
for (each in category){
    par(mfg = c(n, 1))
    plot(density(log10(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == each])), 
        xlab = "log10(Pred Mass)",
        ylab = "Density",
        main = each)
    mean_pred <- c(mean_pred, log(mean(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == each])))
    median_pred <- c(median_pred, median(MyDF$Predator.mass[MyDF$Type.of.feeding.interaction == each]))
    n <- n + 1
}
graphics.off();

# prey subplots
pdf("../results/Prey_Subplots.pdf", width = 11, height = 8)
m = 1
par(mfrow = c(5, 1))
mean_prey = c()
median_prey = c()
for (each in category){
    par(mfg = c(m, 1))
    plot(density(log10(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == each])),
        xlab = "log10(Prey Mass)",
        ylab = "Density",
        main = each)
    mean_prey <- c(mean_prey, log(mean(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == each])))
    median_prey <- c(median_prey, median(MyDF$Prey.mass[MyDF$Type.of.feeding.interaction == each]))
    m <- m + 1
}
graphics.off();

# size ratio subplots
pdf("../results/SizeRatio_Subplots.pdf", width = 11, height = 8)
g = 1
par(mfrow = c(5, 1))
mean_ratio = c()
median_ratio = c()
for (each in category){
    par(mfg = c(g, 1))
    plot(density(log10(MyDF$ratio[MyDF$Type.of.feeding.interaction == each])),
        xlab = "log10(Size ratio of prey mass over predator mass)",
        main = each)
    mean_ratio <- c(mean_ratio, log(mean(MyDF$ratio[MyDF$Type.of.feeding.interaction == each])))
    median_ratio <- c(median_ratio, median(MyDF$ratio[MyDF$Type.of.feeding.interaction == each]))
    g <- g + 1
}
graphics.off();

# creating the dataframe
df <- data.frame(category, mean_pred, mean_prey, mean_ratio, median_pred, median_prey, median_ratio)
write.csv(df, "../results/PP_results.csv")
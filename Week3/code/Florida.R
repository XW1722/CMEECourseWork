rm(list = ls())
load("../data/KeyWestAnnualMeanTemperature.RData")

# calculating the correlation coefficient between years and temperature
corrcoeff <- cor(ats$Year, ats$Temp)

# repeating the calculation, each time randomly reshuffling the temperatures
set.seed(1)
randcorr <- c()
for (i in 1:100) {
    randcorr[i] <- cor(ats$Year, sample(ats$Temp))
}


# calculating the fraction of greater correlation coefficients
i <- 0
for (j in randcorr){
    if (j > corrcoeff){
        i <- i + 1
    }
}
p_value <- i/100

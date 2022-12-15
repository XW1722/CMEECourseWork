rm(list = ls())
load("../data/KeyWestAnnualMeanTemperature.RData")

# calculating the correlation coefficient between years and temperature
corrcoeff <- cor(ats$Year, ats$Temp)

# repeating the calculation, each time randomly reshuffling the temperatures
set.seed(1)
randcorr <- c()
for (i in 1:100) {
    randcorr[i] <- cor(ats$Year, sample(ats$Temp, replace = FALSE))
}


# calculating the fraction of greater correlation coefficients
i <- 0
for (j in randcorr){
    if (j > corrcoeff){
        i <- i + 1
    }
}
p_value <- i/100

# plotting graphs and save the results
pdf("../data/Floridaplot.pdf")
hist(randcorr, xlab = "Correlation coefficients", ylab = "Frequency",
        main = "The frequency of coefficients between years and temperature",
        xlim = c(-0.6,0.6))
abline(v = corrcoeff, col = "red")
text(0.4, 10, paste("original correlation coefficient", corrcoeff),
    col = "red", cex = 0.7)
text(0.4, 5, "p-value: 0")
graphics.off()


## TAutoCorr.R ##
load("../data/KeyWestAnnualMeanTemperature.RData")


df <- data.frame(ats$Temp[1:99], ats$Temp[2:100])
names(df) <- c("Temp", "Successive Temp")
 
# paired t-test
cor.test(df$Temp, df$`Successive Temp`)

## randomly permute time series ##


n <- 100000                 #number of trials
random_successes <- 0       #count of randomly generated correlations which are stronger than test statistic
success_list <- list()      #list random successes

for (i in 1:n){
  new_tempseries <- sample(ats$Temp, 100, replace = F)        #permutes temperature measurements
  new_DF <- data.frame(new_tempseries[1:99], new_tempseries[2:100])                 #forms new data frame of first 99 years and their successive years
  correlation <- as.numeric(cor.test(new_DF[[1]], new_DF[[2]])['estimate'])         #determines correlation
  if ( abs(correlation) >= cor.test(df$Temp, df$`Successive Temp`)['estimate'])  {  #counts and stores correlation if stronger than correlation observed for true data
    random_successes = random_successes + 1
    success_list <- append(success_list, correlation)
  }
}

#find percentage
rate = random_successes / n
print(rate)

#differenciate between negative and positive correlations
length(success_list[success_list < 0])
length(success_list[success_list > 0])
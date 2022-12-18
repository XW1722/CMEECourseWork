# Cleaning the environment
rm(list = ls())

# Loading the required packages
library(tidyverse)

# Loading the data as MyDF
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# Converting the Prey mass to grams 
MyDF$Prey.mass[MyDF$Prey.mass.unit == 'mg'] <- 
    MyDF$Prey.mass[MyDF$Prey.mass.unit == 'mg']/1000

# Creating a dataframe of required columns only
new <- MyDF %>% select(Prey.mass, Predator.mass, 
                       Type.of.feeding.interaction, Predator.lifestage,
                       Location)

# Creating an empty data frame
new_df <- data.frame(Type.of.feeding.interaction = character(),
                     Predator.lifestage = character(),
                     Location = character(),
                     Regression.slope = double(), 
                     Regression.intercept = double(), 
                     R.squared = double(), 
                     F.statistic = double(),
                     p.value = double())

# Fitting linear regressions subset-wise and saving the required values to the 
# initialized dataframe

for(inter in unique(new$Type.of.feeding.interaction)){
    for(lifestage in unique(new$Predator.lifestage)){
        for(loc in unique(new$Location)){
            tmp <- new %>%
                filter(Type.of.feeding.interaction == inter,
                       Predator.lifestage == lifestage,
                       Location == loc)
            if(nrow(tmp) > 2){ # Only fitting regression if there are more than 2 observations
                mod <- lm(log10(Predator.mass)~log10(Prey.mass), data = tmp)
                tmp1 <- summary(mod)
                needed <- c(tmp1$coefficients[2,1],
                            tmp1$coefficients[1,1],
                            tmp1$adj.r.squared, 
                            tmp1$fstatistic[1], 
                            tmp1$coefficients[,4][2])
                new_df[nrow(new_df)+1,] <- c(inter, lifestage, loc, needed)
            }
        }
    }
}


# Writing the data frame to a results file
write.csv(new_df, file = '../results/PP_Regress_Results.csv', row.names = F)

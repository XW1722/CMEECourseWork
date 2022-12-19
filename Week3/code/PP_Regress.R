# loading the dataset and package
require(ggplot2)
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# factorize
MyDF$Predator.lifestage <- as.factor(MyDF$Predator.lifestage)
MyDF$Type.of.feeding.interaction <- as.factor(MyDF$Type.of.feeding.interaction)

# plotting the graph
pdf("../results/PP_Regress.pdf")
print(ggplot(MyDF, aes(x = log(Prey.mass), y = log(Predator.mass),
                        colour = Predator.lifestage)) +
        geom_point(shape = I(3)) +
        theme_bw() +
        geom_smooth(method = "lm", fullrange = TRUE, size = 0.5) +
        facet_wrap(Type.of.feeding.interaction ~., ncol = 1) +
        labs(x = "Prey Mass in grams", y = "Predator mass in grams") +
        theme(legend.position = "bottom", aspect.ratio = 0.3) +
        guides(colour=guide_legend(nrow = 1)))
graphics.off();

# computation of regression results
# initialising
reg.slope <- c()
reg.intercept <- c()
r.squared <- c()
f.statistics <- c()
p.value <- c()
results <- data.frame()

lifestage <- unique(MyDF$Predator.lifestage)

for(l in lifestage){
    # dataset for each lifestage
    stage = subset(MyDF, Predator.lifestage == l)

    for(t in unique(stage$Type.of.feeding.interaction)){
        # dataset for each type of feeding interaction
        type = subset(stage, Type.of.feeding.interaction == t)
        lm_summary <- summary(lm(log(Predator.mass)~log(Prey.mass), data = type))
        
        if(is.null(lm_summary$fstatistic[1])){
            f.statistics = c(f.statistics, "NA")
        } else {
            f.statistics = c(f.statistics, as.numeric(lm_summary$fstatistic[1]))
            }
        
        reg.slope <- c(reg.slope, lm_summary$coefficients[2])
        reg.intercept = c(reg.intercept, lm_summary$coefficients[1])
        r.squared = c(r.squared, lm_summary$r.squared)
        p.value <- c(p.value, anova(lm(log(Predator.mass)~log(Prey.mass), data = type))$'Pr(>F)'[1])
        df <- data.frame(t, l, reg.slope, reg.intercept, r.squared, p.value,f.statistics)
        final_results = rbind(results, df)
  }
}
# writing the output dataframe
colnames(final_results) <- c('Type_of_feeding_interaction', 'predator_lifestage', 'regression_slope', 'regression_intercept', 'R-squared', 'p-value', 'F-statistics')
write.csv(final_results, "../results/PP_Regress_Results.csv")
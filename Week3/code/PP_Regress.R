# loading the dataset and package
require(ggplot2)
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# factorize
MyDF$Predator.lifestage <- as.factor(MyDF$Predator.lifestage)
MyDF$Type.of.feeding.interaction <- as.factor(MyDF$Type.of.feeding.interaction)

# plotting the graph
pdf("../results/PP_Regress.pdf")
p <- ggplot(MyDF, aes(x = log(Prey.mass), y = log(Predator.mass),
                        colour = Predator.lifestage)) +
        geom_point(shape = I(3)) +
        theme_bw() +
        geom_smooth(method = "lm", fullrange = TRUE, size = 0.5) +
        facet_wrap(Type.of.feeding.interaction ~., ncol = 1) +
        labs(x = "Prey Mass in grams", y = "Predator mass in grams") +
        theme(legend.position = "bottom", aspect.ratio = 0.3) +
        guides(colour=guide_legend(nrow = 1))
print(p)
graphics.off();

# computation of regression results
reg.slope <- c()
reg.intercept <- c()
r.squared <- c()
f.statistics <- c()
p.value <- c()
results <- data.frame()
for(l in unique(MyDF$Predator.lifestage)){
    stage = subset(MyDF, Predator.lifestage == l)
    for(t in unique(stage$Type.of.feeding.interaction)){
    type = subset(stage, Type.of.feeding.interaction == t)
    s <- summary(lm(log(Predator.mass)~log(Prey.mass), data = type))
    if(is.null(s$fstatistic[1])){
        f.statistics = c(f.statistics, "NA")
    }else{
        f.statistics = c(f.statistics, as.numeric(s$fstatistic[1]))
        }
    reg.slope <- c(reg.slope, s$coefficients[2])
    reg.intercept = c(reg.intercept, s$coefficients[1])
    r.squared = c(r.squared, s$r.squared)
    p.value <- c(p.value, anova(lm(log(Predator.mass)~log(Prey.mass), data = type))$'Pr(>F)'[1])
    df <- data.frame(t, l, reg.slope, reg.intercept, r.squared, p.value,f.statistics)
    final_results = rbind(results, df)
  }
}
colnames(final_results) <- c('Type_of_feeding_interaction', 'predator_lifestage', 'regression_slope', 'regression_intercept', 'R-squared', 'p-value', 'F-statistics')
write.csv(final_results, "../results/PP_Regress_Results.csv")
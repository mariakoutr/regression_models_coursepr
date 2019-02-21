coursepr_rm <- function()
{
library(datasets)
library(dplyr)
library(ggplot2)
mt <- mtcars
head(mt)
str(mt)
summary(mt$mpg)
mt$am <- as.factor(mt$am)
levels(mt$am) <- c("auto", "manual")

png("boxplot.png")
boxplot(mpg ~ am, data = mt, xlab = "Transmission Type", 
col = c("blue", "red"), ylab = "Miles per Gallon",
main = "Boxplot of MPG vs. Transmission")
dev.off()

aggregate(mpg~am, data = mt, mean)
auto <- mt[mt$am == "auto", ]
manual <- mt[mt$am == "manual", ]
t.test(auto$mpg, manual$mpg) 
fit <- lm(mpg ~ am, data = mt)
summary(fit) 
fitm2 <- lm(mpg ~ am + cyl + disp + hp + wt + qsec + drat
+ vs + gear + carb, data = mt) 
bestmod <- step(fitm2, direction = "backward")
fitbest <- lm(mpg ~ am + wt +qsec, data = mt)
summary(fitbest)
anova(fit, fitbest)

png("residual_plots.png")
par(mfrow = c(2,2))
plot(fitbest)
dev.off()
}
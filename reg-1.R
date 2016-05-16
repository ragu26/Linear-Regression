setwd("E:/Mstat Courses/Regression/new project")
# test code
mydata = read.table("hybrid cars.txt", header = T)
# detach(all)

# changing the decimals
options(scipen = 100)
options(digits = 2)

# summary
names(mydata)
summary(mydata)
mydata$class <- factor(mydata$class)

# data exploration
library(xtable)
library(data.table)

# for latex
l1 <- xtable(summary(mydata))
print(l1)
summary(mydata)

# renaming year in mydata as 'year' is a system variable
setnames(mydata, 2, "myear")
attach(mydata)

# creating latex table
varlist <- c()
varlist <- data.frame(t(t(names(mydata))))
desc <- c("vehicle Make", "Manufactured Year", "Retail Price", "Acceleration Rate", 
          "Mileage given in miles/gallon", "Class of the vehicle")
varlist <- cbind(varlist, desc)
names(varlist) <- c("Variable Name", "Description")
xtable(varlist)
pairs(mydata)

# visual inspection of histograms
par(mfrow = c(2, 2))
hist(myear, main = "Histogram of year", xlab = "Year", prob = TRUE)
lines(density(myear, adjust = 2), lty = "dotted")

hist(msrp, main = "Histogram of retail price", xlab = "Retail price", prob = TRUE)
lines(density(msrp, adjust = 2), lty = "dotted")

hist(accelrate, main = "Histogram of acceleration rate", xlab = "Acceleration rate", 
     prob = TRUE)
lines(density(accelrate, adjust = 2), lty = "dotted")

hist(mpg, main = "Histogram of fuel economy", xlab = "mpg", prob = TRUE)
lines(density(mpg, adjust = 2), lty = "dotted")

# exploration for trend
boxplot(myear, main = "Boxplot of year", xlab = "Year")

boxplot(msrp, main = "Boxplot of retail price", xlab = "Retail price")

boxplot(accelrate, main = "Histogram of acceleration rate", xlab = "Acceleration rate", 
        prob = TRUE)

boxplot(mpg, main = "Histogram of fuel economy", xlab = "mpg", prob = TRUE)

# The box-plots for all the variables except the response do not show any
# deviation from the normal case. The box-plot for the variable retail price
# captures a lot of outliers which can be attributed to the heavy right tail seen
# in the histogram. This may comprise a positive or a negative effect on the
# model to be built, depending on whether these are going to contribute to good
# or bad leverage points.

# no multicollinearity concern

# latex table
l2 <- xtable(cor(mydata[, 4:5]))
print(l2)

# basic model
fit1 <- lm(msrp ~ accelrate + mpg)
plot(fit1)
summary(fit1)

# latex table
l3 <- xtable(fit1)
print(l3)

# anova To gain better insights into the variance added to the model by each
# predictor, the variance is decomposed
anfit1 <- anova(fit1)
l4 <- xtable(anfit1)
print(l4)
anfit1

# creating regression sum of squares
fit0.1 <- lm(msrp ~ accelrate)
fit0.1SSR <- sum((fit0.1$fitted.values - mean(msrp))^2)
fit0.1SSR

fit1SSR <- sum((fit1$fitted.values - mean(msrp))^2)
fit1SSR/219800274

fit1SSR.X2X1 <- fit1SSR - fit0.1SSR
SSE <- sum((fit1$residuals)^2)
SSTO <- fit1SSR.X2X1 + fit0.1SSR + SSE

n = length(mydata[, 1])

# creating ANOVA table manually
Reg <- t(t(c("SSR(accelrate)", "SSR(mpg|accelrate)", "Error", "Total")))
SS <- t(t(c(fit0.1SSR, fit1SSR.X2X1, SSE, SSTO)))
df <- t(t(c(1, 1, n - 1 - 2, n - 1)))
MS <- t(t((c(SS[1]/df[1], SS[2]/df[2], SS[3]/df[3], ""))))
decomp.table <- data.frame((cbind(Reg, SS, df, MS)), stringsAsFactors = default.stringsAsFactors())
colnames(decomp.table) <- c("Regression", "SS", "df", "MS")
print(decomp.table)

xtable(decomp.table)

# checking if all the categories are important Each model class is plotted
# against the retail price in order to check if any relation could be
# hypothesized, which will help in adding the variable to the model.

par(mfrow = c(1, 1))
# boxplot(msrp ~ class, xlab = 'Model Class', ylab = 'Max Retail Price', main =
# 'Retail price for every model class')

# If we carefully observe the box-plot and the fitted model, we can infer that
# the vehicles belonging to class large have more impact on retail price than
# vehicles which do not.


fit2 <- lm(msrp ~ accelrate + mpg + class)
# t4 <- xtable(summary(fit2))
plot(fit2)

# replacing class size with a variable large having two levels 1-Large, 0-Not
# Large
large <- ifelse(class == "L", 1, 0)
large <- factor(large)
mydata <- cbind(mydata, large)
detach(mydata)
attach(mydata)

# fitting the model with new variable and visual inspection if it satisfies the
# assumptions
fit2.1 <- lm(msrp ~ accelrate + mpg + large)
xtable(summary(fit2.1))
summary(fit2)
plot(fit2.1)

# component residual plots to check if a quadratic term is required
library(car)
crPlots(fit2.1)
par(mfrow = c(2, 1))
fit3 <- lm(msrp ~ accelrate + mpg + large + I(mpg^2))
summary(fit3)
plot(fit3)

# From the CR plots, the variable mpg has steep curvature indicating that higher
# order terms might be required for the model. Although a curvature can be
# observed for accelrate it is not very steep. Hence, it will be a good idea to
# check for higher order terms pertaining to variable mpg.A second degree term is
# added to the model owing to the inference from CR plots.

# contour plots to check for interaction between the continous predictors
fit2.3 <- lm(msrp ~ accelrate * mpg + large)
summary(fit2.3)
library(rsm)
par(mfrow = c(1, 1))
contour.lm(fit2.3, accelrate ~ mpg)

# final model
fit4 <- lm(msrp ~ accelrate + mpg + large + I(mpg^2) + accelrate * mpg)
xtable(summary(fit4))
par(mfrow = c(1, 2))
plot(fit4)

# the residuals have constant variance throughout, barring a deviation at the
# start. Again from the QQ plot we can observe that the residuals are normally
# distributed.
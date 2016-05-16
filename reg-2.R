# The data has been collected from a cafe called Executive Express, run by
# undergraduate business students at aMidwestern public university. It was
# collected over a ten-week period from January to April 2010.

setwd("E:/Mstat Courses/Regression/new project")
mydata = read.table("cafedata.txt", header = T)
mydata <- mydata[-34, ]
mydata <- mydata[, -(1)]
newdata <- mydata[1:40, ]
newdata.val <- mydata[41:47, ]
library(xtable)
names(newdata)

# standardizing the data
mydata$Day.of.Week <- factor(mydata$Day.of.Week, levels = c("Mon", "Tue", "Wed", 
                                                            "Thu", "Fri"))
newdata[, 2:17] <- scale(newdata[2:17])

# Summary of variables and their descriptions
varlist <- c()
varlist <- data.frame(t(t(names(mydata))))
desc <- c("Day of week", "Number of bread sand sold", "Number of bread sand wasted", 
          "Number of wraps sold", "Number of wraps wasted", "Number of muffins sold", "Number of muffins wasted", 
          "Number of cookies sold", "Number of cookies wasted", "Number of fruit cups sold", 
          "Number of fruit cups wasted", "Number of chips sold", "Number of juices sold", 
          "Number of sodas sold", "Number of coffees sold", "Maximum daily temperature (F)", 
          "Sales ($)")
varlist <- cbind(varlist, desc)
names(varlist) <- c("Variable Name", "Description")
xtable(varlist)

# visual inspection
pairs(mydata)

# model with all parameters
attach(newdata)
fit1 <- lm(Sales ~ ., data = newdata)
xtable(summary(fit1))
# Day of the week contributes a small proportion to the sales with Tuesday being
# the highest and Friday being the least along withMonday, Wednesday and Thursday
# in increasing order of contribution . One unit increase in chips contributes to
# 1.58 $ increase in sales . One unit increase in coffee contributes to 1.58 $
# increase in sales . All other estimates are not significant at 5% level. Hence,
# interpreting them would make little sense


# the estimates for many variables are not significant, a better approach
# inmodelling is required. We try to use a different coding scheme for day of the
# variable and see if it causes any change. Instead of choosingMonday as the
# reference,Wednesday is chosen as the reference.  # Coding scheme for day of the
# week changed
changedata <- mydata
changedata$Day.of.Week <- factor(changedata$Day.of.Week, levels = c("Wed", "Thu", 
                                                                    "Fri", "Mon", "Tue"))
attach(changedata)
fitchange <- lm(Sales ~ ., data = changedata)
summary(fitchange)

# devise a strategy to select fewer predictors so that an exhaustive combination
# search could be performed by fitting linear models with all possible subsets
# among the fewer predictors. Although there are many different ways to do it, we
# will resort to one of the simplest techniques i.e. mean-centering the
# data(except Day of the week) and fitting a linear model. The coefficients thus
# obtained will be a measure of impact of the covariate on the response.  model
# with variables that cotribute more

datacp <- data.frame(cbind(newdata$Day.of.Week, newdata$Wraps.Waste, newdata$Muffins.Sold, 
                           newdata$Chips, newdata$Muffins.Waste, newdata$Fruit.Cup.Waste, newdata$Cookies.Sold, 
                           newdata$Sales))
fittry <- lm(datacp[, 8] ~ .^7, data = datacp)
library(leaps)
a <- leaps(x = mydata[, 3:5], y = mydata[, 17], method = "Cp")
# fittry(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips + Muffins.Waste
# + Fruit.Cup.Waste + Cookies.Sold, data = newdata)
n <- length(newdata[, 1])
# fit2<-lm(Sales~Day.of.Week+Wraps.Waste+Muffins.Sold+Chips+Muffins.Waste+Fruit.Cup.Waste+Cookies.Sold,data=newdata)
fit2 <- lm(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips + Muffins.Waste + 
             Fruit.Cup.Waste + Cookies.Sold, data = newdata)

# display options
options(scipen = 100)
options(digits = 2)
summary(fit2)



# All possible subsets(128) are fitted with linear model and their results for
# each criterion initializing model attribute parameters
model <- c()
R2 <- c()
AICval <- c()
MSE <- c()
Cp <- c()
p <- c()
test <- c()

# model chosen
Xcombi <- cbind(newdata$Day.of.Week, newdata$Wraps.Waste, newdata$Muffins.Sold, newdata$Chips, 
                newdata$Muffins.Waste, newdata$Fruit.Cup.Waste, newdata$Cookies.Sold)
sat.model <- lm(newdata$Sales ~ .^3, data = data.frame(Xcombi))
S2 <- sum((sat.model$residuals)^2)/(n - 7)

# function returning all possible subsets
combinations = function(n) {
  comb = NULL
  for (i in 1:n) comb = rbind(cbind(1, comb), cbind(0, comb))
  return(comb)
}

# all possible subset of models
combi <- combinations(length(Xcombi[1, ]))

# for(i in 1:length(combi[,1])){ temp <- combi[i,] rowvar <- which(temp!=0) X <-
# Xcombi[,rowvar] tempmodel<-lm(newdata$Sales~X) print(head(X))
# R2[i]<-summary(tempmodel)$adj.r.squared AICval[i]<-AIC(tempmodel)
# SSE<-sum((tempmodel$residuals)^2) p[i]<-sum(temp) MSE[i]<-SSE/(n-p)
# test[i]<-summary(tempmodel)$sigma^2 Cp[i]<-SSE/S2-(n-2*p) }


# checking the model parameters
for (i in 1:127) {
  temp <- combi[i, ]
  rowvar <- which(temp != 0)
  X <- Xcombi[, rowvar]
  if (rowvar[1] == 1) {
    rowvar = rowvar[-1]
    tempmodel <- lm(newdata$Sales ~ factor(newdata$Day.of.Week, levels = c("Mon", 
                                                                           "Tue", "Wed", "Thu", "Fri")) + X)
  } else {
    tempmodel <- lm(newdata$Sales ~ X)
  }
  
  R2[i] <- summary(tempmodel)$adj.r.squared
  AICval[i] <- AIC(tempmodel)
  SSE <- sum((tempmodel$residuals)^2)
  p[i] <- sum(temp)
  MSE[i] <- SSE/(n - p)
  test[i] <- summary(tempmodel)$sigma^2
  Cp[i] <- SSE/S2 - (n - 2 * p)
  
}
# plotting the parameters for the models fitted
plot(p, R2, xlab = "number of predictors", ylab = "R-square", main = "R-Square")
plot(p, AICval, xlab = "number of predictors", ylab = "AIC", main = "AIC")
plot(p, MSE, xlab = "number of predictors", ylab = "MSE", main = " Mean Square Error")

# best model chosen as per R-sq
modelR2 <- lm(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips + Fruit.Cup.Waste, 
              data = newdata)
xtable(summary(modelR2))
summary(modelR2)

# best model chosen as per AIC
modelAIC <- lm(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips, data = newdata)
xtable(summary(modelAIC))

# best model chosen as per MSE
modelMSE <- lm(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips + Muffins.Waste + 
                 Fruit.Cup.Waste + Cookies.Sold, data = newdata)
xtable(summary(modelMSE))
# newdata.val<-newdata.val[,-(1)]
newdata.val[, 2:17] <- scale(newdata.val[, 2:17], center = T)
# The observations(41-48) which were left out during variable selection will be
# used to check how well the data fits themodel selected prediction capablities
# of all 3 models chosen
newdata.val$Day.of.Week <- factor(newdata.val$Day.of.Week, levels = c("Mon", "Tue", 
                                                                      "Wed", "Thu", "Fri"))
MSPE.R2 <- mean((predict(modelR2, newdata = newdata.val) - newdata.val$Sales)^2)
MSPE.AIC <- mean((predict(modelAIC, newdata = newdata.val) - newdata.val$Sales)^2)
MSPE.MSE <- mean((predict(modelMSE, newdata = newdata.val) - newdata.val$Sales)^2)
library(CombMSC)

# PRESS Statistic for the models
PRESSR2 <- sum((newdata.val$Sales - predict(modelR2, newdata = newdata.val))^2)
PRESSAIC <- sum((newdata.val$Sales - predict(modelAIC, newdata = newdata.val))^2)
PRESSMSE <- sum((newdata.val$Sales - predict(modelMSE, newdata = newdata.val))^2)

# The stepwise regression can be conducted through forward, backward or both the
# directions. the direction here implies the sequence i.e. addition or deletion
# of variables. Through various combinations of initial values and direction on
# the first 40 observations, we obtain only one unique modelThe stepwise
# regression can be conducted through forward, backward or both the directions.
# the direction here implies the sequence i.e. addition or deletion of variables.
# Through various combinations of initial values and direction on the first 40
# observations, we obtain only one unique model variable selection through
# stepwise algorithms
attach(newdata)

fitfull <- lm(Sales ~ ., data = newdata)
summary(fitfull)
fitnull <- lm(Sales ~ NULL, data = newdata)
summary(fitnull)

library(MASS)

# backward selection based on AIC
modelAICback <- stepAIC(fitfull, scope = list(upper = ~., lower = ~1), direction = "backward")
summary(modelAICback)
AIC(modelAICback)
# Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Muffins.Waste + Cookies.Sold
# + Fruit.Cup.Waste + Chips + Sodas + Coffees AIC 55

# Forward selection based on AIC
modelAICforward <- stepAIC(fitnull, scope = list(upper = fitfull, lower = ~1), direction = "forward")
summary(modelAICforward)
# Sales ~ Day.of.Week + Chips + Wraps.Waste + Muffins.Sold + Coffees + Sodas +
# Fruit.Cup.Waste + Cookies.Sold + Muffins.Waste AIC 55

# Stepwise selection based on AIC backward stepwise
modelAICboth.1 <- stepAIC(fitfull, scope = list(upper = fitfull, lower = ~1), direction = "both")
summary(modelAICboth.1)
AIC(modelAICboth.1)
# Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Muffins.Waste + Cookies.Sold
# + Fruit.Cup.Waste + Chips + Sodas + Coffees AIC 55

# forward stepwise
modelAICboth.2 <- stepAIC(fitnull, scope = list(upper = fitfull, lower = ~1), direction = "both")
summary(modelAICboth.2)
AIC(modelAICboth.2)
# Sales ~ Day.of.Week + Chips + Wraps.Waste + Muffins.Sold + Coffees + Sodas +
# Fruit.Cup.Waste + Cookies.Sold + Muffins.Waste AIC 55

newdata.val[, 2:17] <- scale(newdata.val[2:17])
newdata.val$Day.of.Week <- factor(newdata.val$Day.of.Week, levels = c("Mon", "Tue", 
                                                                      "Wed", "Thu", "Fri"))
MSPE.model <- mean((predict(modelAICback, newdata = newdata.val) - newdata.val$Sales)^2)
summary(modelAICback)

# PRESS Statistic
PRESS.model <- sum((newdata.val$Sales - predict(modelAICback, newdata = newdata.val))^2)
plot(modelAICback)

# prediction
predictdata = read.table("predict.txt", header = T)
predict(modelR2, predictdata, interval = "predict", level = 0.99)

# outlier detection

# classical diagnostics
modelAICback.stdres <- stdres(modelAICback)
plot(modelAICback.stdres, ylim = c(-3, 3), main = "Residual Plot", ylab = "Standardized Residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# # no outliers Althoughwe cannot find any residual fromthe above plot, it will
# be better to check the robust estimates and leverage if there are any
# residuals. Regression fitted with robust estimates might overcome certain
# disadvantage with classical diagnostics and hence would be very useful if we
# are not able to find any input through classical diagnostics.

modelAICback.stdres <- stdres(modelR2)
plot(modelAICback.stdres, ylim = c(-3, 3), main = "Residual Plot", ylab = "Standardized Residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# Not all outliers are bad. Some of them have a good leverage and hence they tend
# to fit in the model well. The bad leverage point observation 3 and hence it is
# removed from the final model.


# ROBUST Regression- for outliers
library(robustbase)
mydata$Day.of.Week <- factor(mydata$Day.of.Week, levels = c("Mon", "Tue", "Wed", 
                                                            "Thu", "Fri"))

RLTS <- ltsReg(Sales ~ Day.of.Week + Chips + Wraps.Waste + Muffins.Sold + Coffees + 
                 Sodas + Fruit.Cup.Waste + Cookies.Sold + Muffins.Waste, data = mydata, alpha = 0.8)
summary(RLTS)

# Diagnostic plots
plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag")

# predicting results for test variables
mydata = read.table("cafedata.txt", header = T)
mydata <- mydata[-c(3), ]
modelR2 <- lm(Sales ~ Day.of.Week + Wraps.Waste + Muffins.Sold + Chips + Fruit.Cup.Waste, 
              data = mydata)
par(mfrow = c(2, 2))
plot(modelR2)
predictdata = read.table("predict.txt", header = T)
predict(modelR2, predictdata, interval = "predict", level = 0.99)
# Mean Lowerbound Upperbound 117 6.9 227 
setwd("E:/Mstat Courses/Regression/new project")
mydata = read.table("golfers.txt", header = T)
summary(mydata)
names(mydata) <- c("prize", "drive", "fairway", "greens", "putts", "saves", "tourplay", 
                   "green", "tourcomplete", "average", "rounds", "strokes")
names(mydata)

varlist <- c()
varlist <- data.frame(t(t(names(mydata))))
desc <- c("Prize winnings (1000 of dollars)", "Average drive (yards)", "Percent of fairways hit", 
          "Percent of greens reached in regulation", "Average putts per round", "Percent of sand saves (2 shots to hole)", 
          "Number of tournaments played", "Green in regulation putts per hole", "Completed tournaments", 
          "Average percentile in tournaments (high is good)", "Rounds completed", "Average strokes per round")
varlist <- cbind(varlist, desc)
names(varlist) <- c("Variable Name", "Description")
xtable(varlist)

# changing the decimals
options(scipen = 100)
options(digits = 2)
attach(mydata)

fit1 <- lm(prize ~ ., data = mydata)
summary(fit1)
xtable(summary(fit1))
par(mfrow = c(1, 2))
# All the estimates of the model were not significant and so it will be better to
# explore the response.  Histogram of the response was plotted so as to check the
# distribution of the response. It was observed that the data is skewed heavily
# towards the right. Log of the response was plotted to check if log
# transformation improves the normality. As expected log transformation improved
# the normality.

hist(prize, main = "Histogram of prize", xlab = "Prize", prob = TRUE)
lines(density(prize, adjust = 2), lty = "dotted")

hist(log(prize), main = "Histogram of log prize", xlab = "Log of Prize", prob = TRUE)
lines(density(log(prize), adjust = 2), lty = "dotted")
xtable(cor(mydata[2:11]))

fit2 <- lm(log(prize) ~ ., data = mydata)
summary(fit2)
xtable(summary(fit2))
par(mfrow = c(2, 2))

for (i in 2:12) {
  hist(mydata[, i], main = paste("Histogram of ", names(mydata)[i], sep = ""), 
       xlab = names(mydata)[i], prob = TRUE)
  lines(density(mydata[, i], adjust = 2), lty = "dotted")
}



# If we carefully observe the estimates we can find the effect of variable putts
# is positive among many negative effects. Hence it will be worthwhile to plot
# the confidence regions between putts and greens as they are logically
# correlated to each other.  Joint confidence region
par(mfrow = c(1, 1))
confinterval <- confint(fit1, parm = c(5, 8), level = 1 - 0.05)
library(ellipse)
plot(ellipse(fit1, which = c("putts", "green"), level = 0.95), type = "l", main = "Confidence Regions")
points(fit2$coefficients["putts"], fit2$coefficients["green"])
abline(v = confinterval[1, 1], lty = 1, col = "red")
abline(v = confinterval[1, 2], lty = 1, col = "red")
abline(h = confinterval[2, 1], lty = 1, col = "red")
abline(h = confinterval[2, 2], lty = 1, col = "red")
abline(h = 0, col = "black")
abline(v = 0, col = "black")

# Since the joint confidence interval may be too conservative in the approach,
# simultaneous confidence interval are required which take a more lenient
# approach.
sconfinterval <- confint(fit2, parm = c(5, 8), level = 1 - 0.05/3)

abline(v = sconfinterval[1, 1], lty = 2, col = "blue")
abline(v = sconfinterval[1, 2], lty = 2, col = "blue")
abline(h = sconfinterval[2, 1], lty = 2, col = "blue")
abline(h = sconfinterval[2, 2], lty = 2, col = "blue")
# It is important to consider simultaneous confidence intervals analogous to
# multiple confidence intervals.  But when multiple confidence intervals are
# considered, the chances of excluding a value is higher than its individual
# counterparts. Simultaneous confidence intervals correct this deficiency and
# allow more liberal estimates, so that the overall confidence level is
# maintained.

par(mfrow = c(2, 2))
plot(fit2)

# For the model fitted with log-transformed response, the residuals were plotted
# against the fitted values.  It can be observed that the residuals follow a time
# trend which is a violation of the fundamental assumption. Hence we apply
# weighted least squares(to the model represented by 3.2) to check if it can
# improve the fit.

# weighted least squares
par(col = "black")
resid <- residuals(fit2)
stdev <- lm(abs(resid) ~ drive + fairway + greens + putts + saves + tourplay + green + 
              tourcomplete + average + rounds + strokes, data = mydata)
weightgolf <- 1/stdev$fitted^2
fitw <- lm(log(prize) ~ ., data = mydata, weights = weightgolf)
summary(fitw)
xtable(summary(fitw))
# The size of the weight indicates the precision of the information contained in
# the associated observation.  Optimizing the weighted fitting criterion to find
# the parameter estimates allows the weights to determine the contribution of
# each observation to the final parameter estimates.

# plot(fitw)
plot(log(prize), resid(fitw) * sqrt(weightgolf), ylab = "Weighted residuals", main = "Residual Plot")
par(col = "red")
lines(lowess(resid(fitw) * sqrt(weightgolf) ~ log(prize)))
# We can observe from the residual plot that the time trend of the residuals has
# improved, but still we cannot say that the residual variance is constant. Also
# if we take into account that the confidence regions insinuated about
# multicollinearity,

# Multicollinearity diagnostics 1) Correlations
mydata.cor <- cor(mydata)
mydata.cor
corx <- mydata.cor[-1, -1]
corx > 0.7
# Variable green is highly correlated with variables tourcomplete and average .
# Tourplay is very highly correlated with tourcomplete and rounds . Green is
# negatively correlated with average . Tourcomplete is very highly correlated
# with average and rounds . Average is highly correlated with rounds

# standardizing data
mydata_scaled <- scale(mydata, center = TRUE, scale = TRUE)
mydata_scaled <- mydata_scaled/sqrt(length(mydata[, 1]) - 1)
summary(mydata_scaled)

# Before starting the PCR, the data has to be standardized.This has to be done as
# some variables have a large variance and some small, PCA (maximizing variance)
# will load on the large variances (or) in other words the units of predictors
# might not be the same and hence standardizing them would render them on an
# equal scale and the data reduction technique can rely on the large variance
# since the data is on the same scale.  PCR (all predictors have the same units)
library(xtable)
PCA <- princomp(mydata_scaled[, 2:12])
loadings <- summary(PCA, loadings = T)
dim(loadings)
data.frame(ids = names(loadings), nums = loadings)
loadings$Loadings
# From the PCR components, we can observe that the first 3 components together
# account for more than 80% of the variance in the data and hence first 3
# components are good enough to explain the data instead of 12 predictors.



eigen(cov(mydata_scaled[, 2:12]))
PCA.scores <- PCA$scores[, 1:3]
PCR <- lm(mydata_scaled[, 1] ~ PCA.scores[, 1] + PCA.scores[, 2] + PCA.scores[, 3] - 
            1)
summary(PCR)

# PCR (all predictors have the same units)

PCR.coef <- colSums(coefficients(PCR) * t(PCA$loadings[, 1:3]))
PCR.coef <- 
  a <- confint(PCR, level = 0.99)
b <- confint(fit1, level = 0.99)
a1 <- cbind(a[1, 1], a[2, 1], a[3, 1])
a2 <- cbind(a[1, 2], a[2, 2], a[3, 2])
PCR.coef1 <- colSums(a1 %*% t(PCA$loadings[, 1:3]))
PCR.coef2 <- colSums(a2 %*% t(PCA$loadings[, 1:3]))
PCR.confint <- cbind(PCR.coef1, PCR.coef2)
b <- b[-1, ]
xtable(b)
xtable(PCR.confint)
b
# 99%confidence interval is computed for the least square estimates corresponding
# to initialmodel(3.1) and for PCR. As can be observed from the table, the
# confidence interval for PCR is more consistent( fewer terms have 0 in their
# intervals) than their least squares counterparts. 
setwd("E:/Mstat Courses/Regression/new project/Code")
mydata = read.table("transmittance.txt", header = T)
library(xtable)
names(mydata) <- c("tr", "wv")
attach(mydata)
options(scipen = 100)
options(digits = 8)
n <- dim(mydata)[1]

# Since the data has less information, it will be a good beginning to start with
# a correlation test between the predictor and response to find if they are
# statistically correlated.

fit2 <- lm(log(tr) ~ wv + I(wv^2))
summary(fit2)
t1 <- xtable(summary(fit2))

# The null hypothesis tested here is that the correlation between the parameters
# is zero against the alternative hypothesis that true ½ is not zero. When tested
# for correlation, we got the resulting correlation coefficient as 0.006 and the
# corresponding p-value 0.9725. Hence we do not have sufficient proof to reject
# the null hypothesis.  spearman test for non-parametric hypothesis test of
# relationship

cor <- cor.test(wv, tr, method = "spearman")

# LM

fit1 <- lm(tr ~ wv)
summary(fit1)
xtable(summary(fit1))

# As can be observed from the p-value of the linear model that we do not have
# sufficient proof to reject the null hypothesis that the effect of all the
# coefficients are zero, the fitted model is of no use.


# shapiro-wilk
shapiro.test(exp(log(tr)))


# we take the predictor along x-axis and response along y-axis for further
# exploration. We can observe the reason why a linear regression was not
# possible. The response increases initially till a point and then starts
# decreasing.  The plot has curvatures which cannot be achieved with a linear
# line and hence we resort to nonparametric fit. But, the main concern in
# non-parametric fit is that we have to choose the right span.  Span represents
# the fraction of the data that will be included in each fit. Lower the span,
# smoother the fit will be. Not choosing the right span may lead to over-fitted
# or under-fitted regression.

# Fitting non-parametric curve
plot(wv, tr, main = " Non-parametric fit", xlab = "Wavelength", ylab = "Transmission")
# lines(lowess(wv, tr), col = 'red' )
s <- c(1/3, 2/3, 1/10)
for (i in 1:length(s)) {
  lines(wv, predict(loess(tr ~ wv, span = s[i], degree = 2), data = mydata), col = i + 
          16)
  legend(470, 0.3, c("span = 2/3", "span = 1/3", "span = 1/10"), lty = 1, col = c("red", 
                                                                                  "green", "black"))
}

loess.fit <- loess(tr ~ wv, span = 1/3, degree = 2)
qqnorm(residuals(loess.fit))
qqline(residuals(loess.fit))
# scatter.smooth(residuals(loess.fit), span = 1, degree = 1)
scatter.smooth(fitted(loess.fit), sqrt(abs(residuals(loess.fit))), span = 1, degree = 1, 
               xlab = "Fitted values", ylab = "Standardized residuals", main = "Residual plot")
# constant variance is questionable (region with times = 1:14 has small variance,
# region with times 15:57 has larger variance)


# the span 1/3 is better as the other two tend to over-fit the data.  As with any
# other fit, we have to measure if the fitted non-parametric curve is better than
# the simple linear model fitted earlier.

# Test for variation
loess.fit <- loess(tr ~ wv, span = 1/3, degree = 2)
traceS <- loess.fit$trace.hat
SSE0 <- sum(residuals(fit1)^2)
SSE1 <- sum(residuals(loess.fit)^2)
Fvalue <- ((SSE0 - SSE1)/(traceS - 2))/((SSE1)/(n - traceS))
Fvalue
Fcrit <- qf(0.95, traceS - 2, n - traceS)
Fcrit
1 - pf(Fvalue, traceS - 2, n - traceS)

# It is observed that the p-value for the above F critical value is 0 and hence
# indicating that the non-parametric is fit is better than the simple linear
# model.  But again, this is just a comparison.As mentioned earlier, in
# non-parametric fit, selecting a span value plays an important role. In this
# case, the selection of the span values are good, but they are not accurate
# enough. This is one of the major problems in non-parametric regression.


# have prior information that the non-linear model of the form given in the paper
# might fit the data. But the catch is we have to find appropriate initial values
# so that the singular gradient problem is avoided. So, the linearized equation
# has to be derived first to get a realistic value for the unknowns.  Non-linear
# regression
plot(wv, tr, main = " Non-linear regression", xlab = "Wavelength", ylab = "Transmission")

# initial values 1
fit.nls1 <- nls(tr ~ A * exp(-((wv - C)^2)/(2 * B^2)), data = mydata, start = list(A = 1, 
                                                                                   B = -10, C = 440), trace = TRUE)
sum.nls1 <- summary(fit.nls1)

A <- sum.nls1$coef[1, 1]
B <- sum.nls1$coef[2, 1]
C <- sum.nls1$coef[3, 1]
xx <- seq(400, 500, length = 35)
yy <- A * exp(-((wv - C)^2)/(2 * B^2))
lines(xx, yy, col = "red", pch = 22, lty = 1, lwd = 1)


# initial values 2
fit.nls2 <- nls(tr ~ A * exp(-((wv - C)^2)/(2 * B^2)), data = mydata, start = list(A = -15, 
                                                                                   B = 100, C = 450), trace = TRUE)
sum.nls2 <- summary(fit.nls2)

A <- sum.nls2$coef[1, 1]
B <- sum.nls2$coef[2, 1]
C <- sum.nls2$coef[3, 1]
xx <- seq(400, 500, length = 35)
yy <- A * exp(-((wv - C)^2)/(2 * B^2))
lines(xx, yy, col = "blue", pch = 21, lty = 2, lwd = 1.5)

# initial values 3
fit.nls3 <- nls(tr ~ A * exp(-((wv - C)^2)/(2 * B^2)), data = mydata, start = list(A = -0.1, 
                                                                                   B = 30, C = 460), trace = TRUE)
sum.nls3 <- summary(fit.nls3)

A <- sum.nls3$coef[1, 1]
B <- sum.nls3$coef[2, 1]
C <- sum.nls3$coef[3, 1]
xx <- seq(400, 500, length = 35)
yy <- A * exp(-((wv - C)^2)/(2 * B^2))
lines(xx, yy, col = "green", pch = 19, lty = 4, lwd = 2)
plot(wv, tr, main = " Non-linear regression", xlab = "Wavelength", ylab = "Transmission", 
     bg = 21)

# comparison plot
plot(wv, tr, main = " Regression Comparison", xlab = "Wavelength", ylab = "Transmission")
abline(fit1, col = "red")
s <- c(2/3)
for (i in 1:length(s)) {
  lines(wv, predict(loess(tr ~ wv, span = s[i], degree = 2), data = mydata), col = "blue")
  # legend(480, .2, c('span = 2/3', 'span = 1/3', 'span = 1/10'), lty=1,
  # col=c('red', 'green', 'blue'))
}
lines(xx, yy, col = "green", pch = 19, lty = 1, lwd = 1)
legend(465, 0.35, c("Least Square", "Non-parametric", "Non-linear"), lty = 1, col = c("red", 
                                                                                      "blue", "green"))
# Residual sum
fit.nls3.res <- residuals(fit.nls3)
fit.nls3.fitted.values <- fitted.values(fit.nls3)
fit.nls3.stdres <- fit.nls3.res/sum.nls3$sigma
sum(fit.nls3.res)
# As expected the least squares regression does not suit the data at all. The
# non-parametric fit is better than the least squares fit but the non-linear fit
# is better than the other two.

# Check model assumptions
par(mfrow = c(2, 2))
qqnorm(fit.nls3.stdres, main = "Q-Q Plot")
qqline(fit.nls3.stdres)
# plot(fit.nls3.fitted.values, xlab='Index', ylab='Response', main='Fitted
# Model')
plot(fit.nls3.fitted.values, fit.nls3.res, xlab = "Fitted value", ylab = "Residual", 
     main = "Residual vs Fitted Values")
lines(lowess(fit.nls3.res ~ fit.nls3.fitted.values), col = "red")
plot(fit.nls3.res, xlab = "Index", ylab = "Residual", main = "Residual index")

plot(fit.nls3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-4, 
                                                                               4), main = "Standardized residual")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2) 
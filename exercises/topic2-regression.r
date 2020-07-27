# Topic 2  Regression

#install.packages("mlr")
#install.packages("glmnet")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("MASS")
#install.packages("dplyr")
library(mlr)
library(glmnet)
library(ggplot2)
library(cowplot)
library(MASS)
library(dplyr)
theme_set(theme_bw())

# Linear Regresson Demo

# Data
mouse.data <- data.frame(
  weight=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))
mouse.data
plot(mouse.data$weight, mouse.data$size)

## create a "linear model" - that is, do the regression
mouse.regression <- lm(size ~ weight, data=mouse.data)

## generate a summary of the regression
summary(mouse.regression)

## add the regression line to our x/y scatter plot
abline(mouse.regression, col="blue")

# Ex: Linear Regresssoin

data(quakes)
plot(quakes$stations, quakes$mag)
quake.regression = lm (mag~stations, data=quakes)
summary(quake.regression)
abline(quake.regression, col="blue")

# Multiple Regression Demo

# Data
mouse.data <- data.frame(
  size = c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
  weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

mouse.data
plot(mouse.data)

# create a "multi regression model"
multiple.regression <- lm(size ~ weight + tail, data=mouse.data)

# generate a summary of the regression
summary(multiple.regression)

# Ex: Multi Regression Demo
data(mtcars)
mtcars.regressiion <-lm(mpg~hp+wt+disp,data=mtcars)
predict(mtcars.regressiion,data.frame(disp=160,hp=110,wt=2.6))

# Ridge Regularization

set.seed(123) 

# Center y, X will be standardized in the modelling function
y <- mtcars %>% select(mpg) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- mtcars %>% select(-mpg) %>% as.matrix()

# Perform 10-fold cross-validation to select lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,standardize = TRUE, nfolds = 10)

# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min
lambda_cv

# Prediction
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_hat_cv  - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

# Lasso Regularization

# Perform 10-fold cross-validation to select lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# Setting alpha = 1 implements lasso regression
lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE, nfolds = 10)

# Plot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min
lambda_cv

# Prediction
model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_hat_cv  - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(X, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)

# Ex: Elastic Net Regularizaton 

# Perform 10-fold cross-validation to select lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# Setting alpha = 1 implements lasso regression
elasticnet_cv <- cv.glmnet(X, y, alpha = 0.2, lambda = lambdas_to_try, standardize = TRUE, nfolds = 10)

# Plot cross-validation results
plot(elasticnet_cv)

# Best cross-validated lambda
lambda_cv <- elasticnet_cv$lambda.min
lambda_cv

# Prediction
model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_hat_cv  - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq


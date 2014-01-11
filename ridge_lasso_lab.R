#Ridge Regression and Lasso Lab from 6.6 of James, Witten, Hastie & Tibshirani

#Requires the packages glmnet and ISLR
library(glmnet)
library(ISLR)


names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#model.matrix creates a matrix of predictors and transforms qualitative variables into dummy variables which is necessary for glmnet()
x <- model.matrix(Salary ~ ., data = Hitters)
y <- Hitters$Salary

#The argument 0 <= alpha <= 1 is the elastic net mixing parameter. A value of 0 gives ridge and a value of 1 gives ridge

#By default glmnet() standardizes variables. To change this, set standardize = FALSE.

#For both ridge and lasso we need to choose a value for lambda. Although glmnet() defaults to an automatically selected range, we can also supply our own values as we'll do here.
grid <- 10^seq(from = 10, to = -2, length = 100)

#Fit Ridge Regression
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

#For each lambda in grid glmnet returns a vector of coefficients which we can access, as usual, with coef
dim(coef(ridge.mod))

#Larger values of lambda imply a harsher penaltly and hence smaller (in L2-norm) coefficients. Here is lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

#How big are the coefficients in L2-norm, excluding the intercept, when lambda = 11498?
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#Now lambda = 704
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#If we want to try a new value of lambda that we didn't include in grid, we can get it using predict. For help, see predict.glmnet
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

#Now we'll estimate the test error of ridge and lasso by splitting the sample into a training and test set
set.seed(1)
train <- sample(1:nrow(x), size = nrow(x)/2) #50% split
y.test <- y[-train]

#Fit ridge on training set and evaluate RMSE on test set using lambda = 4. We can again do this using predict see the help file at ?predict.glmnet
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[-train,])
sqrt(mean((ridge.pred - y.test)^2))

#How does this compare to the RMSE of simply calculating the mean of y?



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

#Helper Functions MSE and RMSE
MSE <- function(pred, truth){
  return(mean((pred - truth)^2))
}

RMSE <- function(pred, truth){
  return( sqrt(mean((pred - truth)^2)))
}

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
test <- (-train)

#Fit ridge on training set and evaluate RMSE on test set using lambda = 4. We can again do this using predict see the help file at ?predict.glmnet
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
MSE(ridge.pred, y[test])
RMSE(ridge.pred, y[test])

#How does this compare to the RMSE of simply using the mean of y?
MSE(mean(y[train]), y.test)
RMSE(mean(y[train]), y.test)

#Since larger values of the ridge penalty shrink all coefficients (except the intercept) towards zero, we can get similar behavior by making lambda very large
ridge.pred <- predict(ridge.mod, s = 10^10, newx = x[test,])
MSE(ridge.pred, y[test])
RMSE(ridge.pred, y[test])

#Now we'll compare to OLS which is simply ridge with lambda = 0. When using the predict function in this case, we'll need the argument exact = TRUE to prevent the function from simply interpolating linearly over the grid of values for lambda used in the original ridge fit
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = TRUE)
MSE(ridge.pred, y[test])
RMSE(ridge.pred, y[test])

#Let's just verify that we get the expected results here
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = TRUE, type = "coefficients")[1:20,]

#How about choosing lambda using cross-validation rather than arbitrarily setting it equal to 4? We can use the member function cv.glmnet() for this. By default it uses tenfold cross-validation but we can change this with the argument folds. The output of cross-validation is random so we set the seed.
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

#What is the RMSE associated with the value of lambda chosen by cross-validation?
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
MSE(ridge.pred, y[test])
RMSE(ridge.pred, y[test])

#Now lets use the full dataset and the value of lambda chosen by cross-validation. This is the default if 
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

#I think the previous isn't quite right. Doesn't it make more sense to do cross-validation on the full dataset in this case?
cv.out <- cv.glmnet(x, y, alpha = 0)
bestlam.full <- cv.out$lambda.min
bestlam.full

#This does in fact change the results somewhat
train.lam <- predict(out, type = "coefficients", s = bestlam)[1:20,]
full.lam <- predict(out, type = "coefficients", s = bestlam.full)[1:20,]
cbind(train.lam, full.lam)


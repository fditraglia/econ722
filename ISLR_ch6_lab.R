#My notes for the Lab from Chapter 6 of James, Witten, Hastie & Tibshirani. The code given here is slightly different from that in the book and corrects some errors.

#Requires the packages glmnet and ISLR
library(glmnet)
library(ISLR)

#Load and clean data
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
MSE(mean(y[train]), y[test])
RMSE(mean(y[train]), y[test])

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


#Now the LASSO. The only change is that we need to set alpha = 1 rather than 0.
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#The preceding plot shows that, depending on the value of the L1 norm that we impose, LASSO sets some coefficients to be exactly zero. The numbers at the top of the plot indicate the total number of nonzero coefficients. Contrast this with ridge, which does not
plot(ridge.mod)

#Now we'll try cross-validation to choose lambda for LASSO and compare RMSE to what we got above
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
MSE(lasso.pred, y[test])
RMSE(lasso.pred, y[test])

#The results are a little worse than what we got for ridge, but "in the ballpark" (what a great joke!). Unlike ridge, however, LASSO sets many coefficients to zero in this example: 12 to be precise
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
round(lasso.coef[lasso.coef != 0], 3)

#We can put these side-by-side with the ridge results as follows:
out <- glmnet(x, y, alpha = 0)
ridge.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
cbind(ridge.coef, lasso.coef)


#Principle Components Regression

#Here we'll use the function pcr() in the library pls
library(pls)
?pcr

#Essentially, the syntax of this function is the same as lm but there are some nifty options. If we set scale = TRUE all predictors are standardized before generating the principal components. If we wet validation = "CV" then tenfold cross-validation is used to choose the number of principle components used. We can examine the fit using summary.

#QUESTION: There are discrete predictors in this dataset, right? Doesn't that screw up PCA? 

set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)

#The preceding output gives RMSE. To get MSE use val.type = "MSEP"
validationplot(pcr.fit)

#It's far from clear how many principal components to use in this example: 1 and 16 are almost identical and neither is very far from 19, which corresponds to simply doing (orthogonalized) OLS!

#The summary function lets us see not just the RMSE from cross-validation for different numbers of principal components, it also gives us the proportion of variance explained: both for X and for y!

#Not we'll try out-of-sample evaluation using our training data, making sure to set the same seed that we used for lasso and ridge
set.seed(1)
pcr.fit <- pcr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

#Cross-validation on the training set suggests using 7 principal components. How does this compare to ridge and lasso?
pcr.pred <- predict(pcr.fit, x[test,-1], ncomp = 7)
#In the book, the forget to remove the constant term from x which results in an error!
MSE(pcr.pred, y[test])
RMSE(pcr.pred, y[test])

#The results are practically identical to those of ridge and hence a little better than those of lasso. The downside of pcr is that we don't get coefficient estimates since each principal component depends on all of the predictors

#Finally, fit pcr to the full dataset
pcr.fit <- pcr(Salary ~., data = Hitters, scale = TRUE, ncomp = 7)
#The book has an error in the preceding: it has y ~ x which throws an error
summary(pcr.fit)

#Partial Least Squares: use the plsr() function in the pls library. The syntax is identical.
set.seed(1)
pls.fit <- plsr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit)

#The validation plot suggests two components
pls.pred <- predict(pls.fit, x[test,-1], ncomp = 2)
#again, the book has a mistake and forgets to take out the constant term!
MSE(pls.pred, y[test])
RMSE(pls.pred, y[test])
#This is somewhat worse that pcr, lasso and ridge

#PLS on the full dataset
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)

#Notice how a two-component pls fit explains about 46.4% of the variation in salary while it takes a seven-component pcr fit to explain 46.69% of the variation in Salary. The reason of the difference is that pcr only looks at x while pcr searches for directions that explain variance in both the predictors and response.


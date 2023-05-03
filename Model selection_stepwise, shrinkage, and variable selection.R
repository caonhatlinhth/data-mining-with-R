
# Solution for assignment of week 7 -------------------------------------------

# read table
dat <- read.csv("bank.csv")
str(dat)

# binarization for cateogorical variables
install.packages("fastDummies")
library(fastDummies)

# create dummy variables for categorical variables
dat <- dummy_cols(dat, select_columns = c("job", "civil", "edu", "credit",
                                             "hloan", "ploan", "ctype", "month",
                                             "day", "presult"),
                     remove_first_dummy = T, remove_selected_columns = T)

# check correlation
# cor(dat$ploan_unknown, dat$hloan_unknown)
# remove personal loan unknown since perfectly correlated with housing loan unknown
dat$ploan_unknown <- NULL

# make outcome binary
dat$outcome <- 1*(dat$outcome == "yes")
# we also going to rename it to y and put it at the end for convenience
dat$y <- dat$outcome
#remove outcome variable
dat$outcome <- NULL

# Q1: How many variable has the newly created datset?
ncol(dat)
# A: 52

# boxplot to check on imbalance
source("Rfunctions.R")
barp(dat,"y",freq ="freq")

# Q2: What is the severity of class imbalance
table(dat$y)[2]/table(dat$y)[1]
# A: Moderate
# Mild
# Severe
# No imbalance is present

# Split data in training and test
set.seed(42)
train <- sample(1:nrow(dat),nrow(dat)*0.7)

dat_train <- dat[train, ]
dat_test <- dat[-train, ]

# Fit full model
fit.full <- glm(y ~ ., data = dat_train, family=binomial)

# Q3: How many variables have been found to be statistically significant in the full model (p =< 0.5)
sumFull <- summary(fit.full)
length(which(sumFull$coefficients[, 4] <=0.05))
# A: 7

# fit null model only with intercept
fit.null <- glm(y ~ 1, data = dat_train, family=binomial)

# stepwise selection using BIC
fit.step <- step(fit.null, scope=formula(fit.full), direction="forward", k=log(nrow(dat_train)))
summary(fit.step)

# Q4: How many variables did stepwise foreward selection using BIC include
length(fit.step$coefficients)
# A: 5
# A: 4 without intercept

# fit cv stepwise using AUC (ignore warnings due to small sample size)
source("Rfunctions.R")
fit.cvstep <- stepCV(Data = dat_train, response = 'y', K = 10, perf = 'auc')

# Q5: How many variables did cross validation stepwise selection
length(fit.cvstep$BestModel$coefficients)
# A: 2
# A: 1 without intercept

# Shrinkage methods
library(glmnet)
# running lasso

# Q6: Which is/are the last variable(s) that will shrink(s) to 0 on the L1 Norm
fit.lasso_vis <- glmnet(x = as.matrix(dat_train[,-52]), y=dat_train$y,
                       family = "binomial", alpha = 1)
plot(fit.lasso_vis, label = T)
which(fit.lasso_vis$beta[,2] != 0)
# A: employees
# A: presult_success
# cconf 
# ctype_telephone
# none of the above

set.seed(42)
# Set up folds for 10-fold cv:
fid = sample(1:10, size = nrow(dat_train), replace = TRUE)

fit.lasso <- cv.glmnet(x = as.matrix(dat_train[,-52]), y=dat_train$y,
                    family = "binomial",
                    alpha = 1, type.measure = "auc", nfolds = 10, foldid = fid)

# Q6: How many variables does lasso selects for lambda min and the lambda by one standard deviation rule
length(coef(fit.lasso, s="lambda.min")@x)
# A: 5
# A: 4 without intercept
length(coef(fit.lasso, s="lambda.1se")@x)
# A: 5
# A: 4 without intercept

# Build Ridge
fit.ridge <- cv.glmnet(x = as.matrix(dat_train[,-52]), y=dat_train$y,
                       family = "binomial",
                       alpha = 0, type.measure = "auc", nfolds = 10, foldid = fid)

# Predicting part
pred.full <- predict(fit.full, newdata = dat_test[-52], type = "response")
pred.step <- predict(fit.step, newdata = dat_test[-52], type = "response")
pred.cvst <- predict(fit.cvstep$BestModel, newdata = dat_test[-52], type = "response")

# lasso with 1sd rule
pred.lasso <- predict(fit.lasso, newx = as.matrix(dat_test[-52]), s="lambda.1se", type = "response")
pred.ridge <- predict(fit.ridge, newx = as.matrix(dat_test[-52]), s="lambda.1se", type = "response")

# Look into their ROC properties
require(pROC)
roc.f <- roc(dat_test$y, pred.full, plot=T, grid=T, col = "red")
roc.s <- roc(dat_test$y, pred.step, plot=F, grid=T, col = "blue")
roc.c <- roc(dat_test$y, pred.cvst, plot=F, grid=T, col = "green")
roc.l <- roc(dat_test$y, as.numeric(pred.lasso), plot=F, grid=T, col = "pink")
roc.r <- roc(dat_test$y, as.numeric(pred.ridge), plot=F, grid=T, col = "purple")

plot(roc.s, add=T, col = "blue")
plot(roc.c, add=T, col = "green")
plot(roc.l, add=T, col = "pink")
plot(roc.r, add=T, col = "purple")

# Q7: Which model has the best AUC performance on the test set?
aucRes <- c(auc(roc.f), auc(roc.s), auc(roc.c), auc(roc.l), auc(roc.r))
names(aucRes) <- c('full', 'step', 'cvstep', 'lasso', 'ridge')
which(aucRes == max(aucRes))
# A: Lasso

# Q8: What further steps would you suggest to the manager to consider so the predictive performance might increase?
# A: Address class imbalance by over- or undersampling
# A: Look into algorithms that can handle categorical variables more naturally
# A: Do more thorough visual analysis (e.g. non-linearity, outliers, missing values)
# A: Consider further future engineering (e.g. discretization, binmomials, PCA)
# Use fewer k-folds in cross-validation to have better training estimates
# Buy a faster computer as we currently cannot run j-k-fold validation
# Tell the manager that this is as good as it can get!
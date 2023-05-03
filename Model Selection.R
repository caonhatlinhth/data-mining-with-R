
# Assignment Week 6: solution -------------------------------------------------------

# ## Variable Descriptions:
# crim: Per capita crime rate by town
# zn: Proportion of residential land zoned for lots over 25,000 ft2\
# indus: Proportion of nonretail business acres per town
# chas: Charles River dummy variable (1 if tract bounds river,  0 # otherwise)
# nox: Nitric oxide concentration (parts per 10 million)\
# rm: Average number of rooms per dwelling 
# age: Proportion of owner-occupied units built prior to 1940
# dis: Weighted distances to five Boston employment centers
# rad: Index of accessibility to radial highways (1-8,24)
# tax: Full-value property-tax rate per $10,000
# ptratio: Pupil/teacher ratio by town
# lstat: Percentage lower status of the population
# median_val: Below = 1 if the median home value is below the overall median, 30k, otherwise above = 0


# load housing
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)

# change factors to numeric values of 0 and 1
dat_train$mvalue <- as.numeric(dat_train$mvalue)-1
dat_test$mvalue <- as.numeric(dat_test$mvalue)-1

# Let's first create new variables
# adding poly degrees
prm_train <- poly(dat_train$rm, 6)
colnames(prm_train) <- paste0("rmpoly", 1:6)

prm_test <- poly(dat_test$rm, 6)
colnames(prm_test) <- paste0("rmpoly", 1:6)

# Next we use discretization to form discrete variables from continuous form
# First we create vectors of length
drm_train <- matrix(0, ncol = 3, nrow = nrow(dat_train))
colnames(drm_train) <- paste0("rmdis", 1:3)

# replace 0 with 1 if conditions is met
drm_train[which(dat_train$rm < 5), 1] <- 1
drm_train[which(dat_train$rm >= 5 & dat_train$rm < 6), 2] <- 1
drm_train[which(dat_train$rm >= 6 & dat_train$rm < 7), 3] <- 1


# repeat for test set
drm_test <- matrix(0, ncol = 3, nrow = nrow(dat_test))
colnames(drm_test) <- paste0("rmdis", 1:3)

drm_test[which(dat_test$rm < 5), 1] <- 1
drm_test[which(dat_test$rm >= 5 & dat_test$rm < 6), 2] <- 1
drm_test[which(dat_test$rm >= 6 & dat_test$rm < 7), 3] <- 1

# Q1: What is the reason why we do not encode the bin 7+ rooms
# (extra credit as this is not covered on slides)?
# A: We can drop one variable since it is represented in the intercept
# We avoid the last variable since higher room number will increase the coefficients
# We always prefer Simpler models
# The bin with 7+ rooms will create  multicolinarity with the variable age

# now put everything together
dat_train <- cbind(dat_train, prm_train, drm_train)
dat_test <- cbind(dat_test, prm_test, drm_test)

names(dat_train)

# Investigate the numbers of polynomials needed

degrees <- 1:6
# initialize vectors to store aic, bic, log-l
aic <- bic <- log.l <- rep(0,max(degrees))

for (d in degrees) {

  # select mvalue, poly1-6 and age
  logR <- glm(mvalue ~ . , data = dat_train[, c(13, 14:(14+d-1),7)], family=binomial)
  
  # compute and store log-likelihood and information criteria
  log.l[d] <- -2*logLik(logR)
  aic[d] <- AIC(logR)
  bic[d] <- BIC(logR)
}

# craete plot of log
plot(log.l, type="b",
     main="information criteria for model selection",
     ylab="(penalised) log-likelihood",
     xlab="polynomial degree/ model complexity",
     ylim=c(min(log.l), max(bic)))
lines(aic, type="b", col= "blue")
lines(bic, type="b", col= "red")
# labels/ ticks at every integer on horizontal axis
axis(1,1:6)
# add legend to improve readability
legend("bottomleft", c("-2*logl","aic", "bic"),
       col=c("black","blue","red"),
       pch=c(1,1,1), lty=c(1,1,1))

# Q2: Based on BIC and AIC, which polynomial degree is the best, the most
# conservative (least complex) option to predict the median house price?
# A: 2
# We can see that from the graph, both AIC and BIC are increasing after Poly2.
# This is a sign that we are start to overfit. AIC having minimum value at
# Poly = 6 means that the overfit is so severe that it outlasts the penalty

# using 2 degree polynomial according to AIC and BIC findings
log.p <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmpoly1","rmpoly2","age")], family=binomial)
pred.p <- predict(log.p, newdata = dat_test[c("rmpoly1","rmpoly2","age")], type="response")

# using discretization room
log.d <- glm(mvalue ~ ., data = dat_train[c("mvalue","rmdis1","rmdis2","rmdis3","age")], family=binomial)
pred.d <- predict(log.d, newdata = dat_test[c("rmdis1","rmdis2","rmdis3","age")], type="response")

# using normal room
log.n <- glm(mvalue ~ ., data = dat_train[c("mvalue","rm","age")], family=binomial)
pred.n <- predict(log.n, newdata = dat_test[c("rm","age")], type="response")

# using only age
log.a <- glm(mvalue ~ ., data = dat_train[c("mvalue","age")], family=binomial)
pred.a <- predict(log.a, newdata = dat_test["age"], type="response")

# Q3: Which model based on AIC and BIC has the best performance on the training data?
# A: Model using age + polynomial room
c(AIC(log.p), BIC(log.p))
# Model using age + discritzation rm
c(AIC(log.d), BIC(log.d))
# Model using age + room
c(AIC(log.n), BIC(log.n))
# Model using only age
c(AIC(log.a), BIC(log.a))

require(pROC)
roc.p <- roc(dat_test$mvalue, pred.p, plot=T, grid=T, col = "red")
roc.d <- roc(dat_test$mvalue, pred.d, plot=F, grid=T, col = "blue")
roc.n <- roc(dat_test$mvalue, pred.n, plot=F, grid=T, col = "green")
roc.a <- roc(dat_test$mvalue, pred.a, plot=F, grid=T, col = "pink")

plot(roc.d, add=T, col = "blue")
plot(roc.n, add=T, col = "green")
plot(roc.a, add=T, col = "pink")


# Q4: What can you learn from the plotted ROC curve on the test data?
# A: Age only, has a higher specificty but lower sensitivity than the polynomial
# A: The model with age and room (normal) achieves the overall best performance for sensitivity
# A: The model with discretized room variable has a similar specificity performance than its continuous counterpart 
# The polynomial model has the overall best performance
# None of the model can outperform the random classifier

# Q5: From this best performing model what is the optimal
# sensitivity/specificity trade-off values (round to two decimals)?
r <- c("threshold","specificity","sensitivity","accuracy","tp","tn","fp","fn")
opt <- coords(roc.n, x = "best",method="closest.topleft", ret=r)

# specificity:
round(opt["specificity"],2)
# A: 0.74
# sensitivity:
round(opt["sensitivity"],2)
# A: 0.94

# also plotting for visualizing
plot(roc.n, col="green", grid = TRUE)
# Add point in ROC curve that represent optimal trade-off
lines(opt["specificity"], opt["sensitivity"], type="p")

# Q6: Why is AUC a good option for our real estate manager?
  # <!-- A: It is able to take into account class imbalance -->
  # <!-- A: No threshold required  -->
  # <!-- A: Allows to directly compare models -->
  # <!-- Directly compares to a random classifier when value 0 -->

# Q7: What AUC value does the best performing model show on the test set?
# Model using age + 2 degree polynomial room
round(auc(roc.p),2)
# Model using age + discritzation rm
round(auc(roc.d),2)
# A: Model using age + room
round(auc(roc.n),2)
# Model using only age
round(auc(roc.a),2)


# Q8: What is the percentage point improvement in AUC over your best model when we use all
# the original variables without the previously introduced polynomials or
# discretized variables (round two decimals)?


# Build model that only contains original variables
log.all <- glm(mvalue ~ ., data = dat_train[, c(1:13)], family=binomial)
pred.all <- predict(log.all, newdata = dat_test[, c(1:12)], type="response")

# we can also add line in our ROC curve
roc.all <- roc(dat_test$mvalue, pred.all, plot=F, grid=T, col = "purple")
round(auc(roc.n)-auc(roc.all),2)
# A: 0.06
plot(roc.all, add=T, col = "purple")

# Also accepting full percentage
round((auc(roc.n)-auc(roc.all))*100,2)
# A: 5.60

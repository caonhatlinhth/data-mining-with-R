dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)
# to find out the variables contained, we use the function names()
names(dat_train)

# summary() provides summary statistics for each variable
summary(dat_train)

# change factors to numeric values of 0 and 1
dat_train$mvalue <- as.numeric(dat_train$mvalue)-1
dat_test$mvalue <- as.numeric(dat_test$mvalue)-1

# Let's first create new variables
# adding polynomials to the 6th degree
prm_train <- poly(dat_train$rm, 6)
colnames(prm_train) <- paste0("rmpoly", 1:6)
prm_test <- poly(dat_test$rm, 6)
colnames(prm_test) <- paste0("rmpoly", 1:6)

# Next, we use discretization to form discrete variables from the continuous form
# First, we create vectors of length
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

# now put everything together
dat_train <- cbind(dat_train, prm_train, drm_train)
dat_test <- cbind(dat_test, prm_test, drm_test)

#Q2.The most conservative (least complex) option to predict the median house price
aic <- c()
bic <- c()
log.likli <- c()
for (i in 1:6) {
  log.model <- glm(mvalue ~ poly(rm, i) + age, data = dat_train, family = "binomial")
  aic[i] <- AIC(log.model)
  bic[i] <- BIC(log.model)
  log.likli[i] <- -2*logLik(log.model)
}

plot(log.likli, type="b",
     main="information criteria for model selection",
     ylab="(penalised) log-likelihood",
     xlab="polynomial degree/ model complexity",
     ylim=c(min(log.likli), max(bic1)))
lines(aic, type="b", col= "blue")
lines(bic, type="b", col= "red")
# labels/ ticks at every integer on horizontal axis
axis(1,1:6)
# add legend to improve readability
legend("bottomleft", c("-2*logl","aic", "bic"),
       col=c("black","blue","red"),
       pch=c(1,1,1), lty=c(1,1,1), cex = c(0.6, 0.6, 0.6))


#Q3.
library(polynom)

# Create the models
model1 <- glm(mvalue ~ poly(rm, 3, raw = TRUE) + age, data = dat_train, family = "binomial")
model2 <- glm(mvalue ~ cut(rmdis1, 5) + cut(rmdis2, 5) + cut(rmdis3, 5) + age, data = dat_train, family = "binomial")
model3 <- glm(mvalue ~ rm + age, data = dat_train, family = "binomial")
model4 <- glm(mvalue ~ age, data = dat_train, family = "binomial")

# Assess their in-sample performance using AIC and BIC
models <- list(model1, model2, model3, model4)
AIC_values <- sapply(models, AIC)
BIC_values <- sapply(models, BIC)

# Identify the best performing model based on AIC and BIC
best_AIC_model <- models[which.min(AIC_values)]
best_BIC_model <- models[which.min(BIC_values)]

# Print the results
cat("Best performing model based on AIC:", names(best_AIC_model), "\n")
cat("Best performing model based on BIC:", names(best_BIC_model), "\n")

#Q4. 
library(pROC)
# Compute the predicted probabilities using the trained logistic regression model
probs <- predict(log.model, newdata = dat_test, type = "response")
# Compute the ROC curve
roc <- roc(dat_test$mvalue, probs)
# Plot the ROC curve
plot(roc, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

#Q5.Find coordinates of the ROC curve for the optimal trade-off
opt.coords <- coords(roc, "best", ret = c("threshold", "specificity", "sensitivity"))
# Print the optimal trade-off values
round(opt.coords, digits=2)

#Q7. # Define the formula for the best-performing model
formula <- as.formula("mvalue ~ age + rm")
# Fit the model on the training data
model <- glm(formula, data = dat_train, family = "binomial")
# Convert the test data to the matrix format
test_matrix <- as.matrix(dat_test[, c("mvalue", "age", "rm")])
# Predict the probabilities on the test set
pred <- predict(model, newdata = dat_test, type = "response")
# Compute the AUC value
auc <- round(auc(dat_test$mvalue, pred), 2)
# Print the AUC value
cat("AUC value:", auc, "\n")

#Q8. # Define the formula for the logistic regression model using original variables
formula_all <- as.formula("mvalue ~ age + rm + dis + rad + crim + zn + indus + chas + nox + ptratio + lstat + tax")
# Fit the model on the training data
model_all <- glm(formula_all, data = dat_train, family = "binomial")
# Predict the probabilities on the test set
pred_all <- predict(model_all, newdata = dat_test, type = "response")
# Compute the AUC value for the new model
auc_all <- auc(dat_test$mvalue, pred_all)
# Compute the percentage point improvement
improvement <- round((auc_all - auc),2)
# Print the results
cat("AUC value (best-performing model):", auc, "\n")
cat("AUC value (new model):", auc_all, "\n")
cat("Percentage point improvement:", improvement, "\n")


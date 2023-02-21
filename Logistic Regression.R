#read two data set
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)

# to find out the variables contained we use the function names()
names(dat_train)

summary(dat_train)

#Q1. change factors to numeric values of 0 and 1
dat_train$mvalue <- as.numeric(dat_train$mvalue)-1
dat_test$mvalue <- as.numeric(dat_test$mvalue)-1

#Q2. Create linear regression model
linear.model <- lm(dat_train$mvalue ~ dat_train$rm, data = dat_train)

# Plot the model
plot(dat_train$rm, dat_train$mvalue, col = factor(dat_train$mvalue),
     xlab = "rm", ylab = "mvalue")
abline(model, col = "red")

#Q3:  fit logistic regression model
log.model <- glm(dat_train$rm ~ dat_train$mvalue , data = dat_train, family = binomial)
# Summary of estimated logistic regression model
summary(log.model)

# Get coefficient estimate for beta1
round(coef(log.model)[2], digits=2)

#Q4. The probability for a house with 5.5 rooms to be below the median house price
#calculate the estimated log odds for a house with 5.5 rooms
logit.hat <- hat.beta[1] + hat.beta[2]*5.5
#convert the log odds back to a probability using the inverse logit function
p.hat <- exp(logit.hat) / (1 + exp(logit.hat))
#convert to percentage
percentage <- round(p.hat*100)
percentage

#Q5. Compute the estimated probabiity of mvalue for all the data in dat_test
probs <- predict(log.model, newdata = dat_test, type = 'response')
pred.classes <- ifelse(probs > 0.2, 0,1)
truth.table <- table(dat_test$mvalue, pred.classes)
truth.table [2,2]


#Q6. Compute the misclassification error as a percentage
misclass_err <- round((truth.table[1,2] + truth.table[2,1]) / sum(truth.table) * 100, digits=2)
misclass_err

#Q7. # create a vector of threshold values to test
library(pROC)
# predict class probabilities on test set
prob <- predict(model, newdata = dat_test, type = "response")
# calculate true positive rate and false positive rate for all possible thresholds
roc <- roc(dat_test$mvalue, prob)
tpr <- roc$sensitivities
fpr <- 1 - roc$specificities
# plot the ROC curve
plot(fpr, tpr, type = "l", xlab = "False Positive Rate", ylab = "True Positive Rate")
# find the optimal threshold
optimal.threshold <- roc$thresholds[which.max(tpr - fpr)]
optimal.threshold


#Q8.build another logistic regression model using age and rooms variable on the target mvalue on the training set
#estimate logistic regression with 2 predictors
log.model2 <- glm(dat_train$mvalue ~ age + rm, data=dat_train, family = binomial)
#model summary
summary(log.model2)

#Q9. find the number of predicted house price below median with threshold of 0.2 on training set
probs <- predict(log.model2, newdata = dat_test, type = 'response')
pred.classes <- ifelse(probs > 0.2, 1, 0)
truth.table <- table(data_test$mvalue, pred.classes)
truth.table [2,2]

#Q10. decision boundary
# Scatter plot of income against balance using default to colour data
plot(dat_train$age, dat_train$rm, col = as.factor(dat_train$mvalue))
# Get estimated coefficients
hb <- coef(log.model2)
# Decision boundary for threshold = 0.2
abline(a=(-hb[1]-log(5))/hb[3], b=-hb[2]/hb[3], col="red")
# Decision boundary for threshold = 0.5
abline(a=-hb[1]/hb[3], b=-hb[2]/hb[3], col="blue")
# Decision boundary for threshold = 0.8
abline(a=(-hb[1]-log(1.25))/hb[3], b=-hb[2]/hb[3], col="green")


# Q11. Load necessary libraries
library(dplyr)
library(caret)
# Fit the first logistic regression model using only the "room" variable
model_room <- glm(dat_train$mvalue ~ dat_train$rm, data = dat_train, family = binomial(link = "logit"))
# Predict the outcome for the test set using model 1
probs_room_room <- predict(model_room, newdata = dat_test, type = "response")
# Convert the predicted probabilities to binary values based on a threshold of 0.2
pred_room <- ifelse(probs_room > 0.2, 1, 0)
# Calculate the misclassification error for model 1

truth.table_room <- table(dat_test$mvalue, pred_room)
misclass_err_room <- (truth.table_room[1,2] + truth.table_room[2,1]) / sum(truth.table_room)

# Fit the second logistic regression model using all variables
model_full <- glm(mvalue ~ ., data = dat_train, family = binomial(link = "logit"))
# Predict the outcome for the test set using model 2
probs_full <- predict(model_full, newdata = dat_test, type = "response")
# Convert the predicted probabilities to binary values based on a threshold of 0.2
pred_full <- ifelse(probs_full > 0.2, 1, 0)
# Calculate the misclassification error for model 2
truth.table_full <- table(dat_test$mvalue, pred_full)
misclass_err_full <- (truth.table_full[1,2] + truth.table_full[2,1]) / sum(truth.table_full)

#calculate the misclassification error improvement
misclass_error_improvement <- ((misclass_error_room - misclass_error_full) / misclass_error_room) * 100
round(misclass_error_improvement, digits=2)


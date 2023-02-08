#read two data set
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)

# to find out the variables contained we use the function names()
names(dat_train)

summary(dat_train)

#Q1. change factors to numeric values of 0 and 1
dat_train$mvalue <- as.numeric(dat_train$mvalue)-1
dat_test$mvalue <- as.numeric(dat_test$mvalue)-1

#Q2: Plot the linear model to find the correlation between rm and mvalue in training data set

# Estimate linear regression
linear.model <- lm(dat_train$rm ~ dat_train$mvalue)
# Visualize data
plot(dat_train$mvalue, dat_train$rm)
# Visualize fitted linear regression
abline(linear.model, col="red")

#summary the linear model to interpret the coefficients
summary(linear.model)

#fit logistic regression model
log.model <- glm(dat_train$mvalue ~ dat_train$rm, data = dat_train, family = binomial)
# Summary of estimated logistic regression model
summary(log.model)

#first plot the observations (as points)
plot(dat_train$rm, dat_train$mvalue)

#to plot the estimated probability first define a vector x that takes values in the range of the rm valuable
x <- seq(from = min(dat_train$rm), to=max(dat_train$rm))
#obtain estimated coefficients using the coef() function
hat.beta <- coef(log.model)

#coefficient
round(hat.beta[1], 2)

#estimate probabilities from Log.Regr.formula
lines(x, (1 + exp(-hat.beta[1] - hat.beta[2]*x))^(-1), col="blue")

#Q4. The probability for a house with 5.5 rooms to be below the median house price
#calculate the estimated log odds for a house with 5.5 rooms
logit.hat <- hat.beta[1] + hat.beta[2]*5.5
#convert the log odds back to a probability using the inverse logit function
p.hat <- exp(logit.hat) / (1 + exp(logit.hat))
#convert to percentage
percentage <- round(p.hat*100)
percentage


#Q5: find the number of house prices which are classified as below median using a theshold of 0.2 on the test set
# Compute the actual class of mvalue for all the data in the dat_test
actual.class <- ifelse(dat_test$mvalue == 0, 1, 0)
# Classify the predictions based on the threshold of 0.2
predicted_classes <- ifelse(predictions > 0.2, "above", "below")
# Count the number of correct classifications
sum(actual.class == class.pred)


#Q6. Find the corresponding misclassification error at a threshold of 0.2 on the test set
#create truth table: rows represents actual class, column represents predicted
truth.table <- table(dat_test$mvalue, class.pred)
truth.table
#total number of observations in truth.table
N <- sum(truth.table)
#misclassification error
round(100*(truth.table[1,2] + truth.table[2,1])/N, digits=2)

#Q8. build another logistic regression model using age and rooms variable on the target mvalue on the training set
#estimate logistic regression with 2 predictors
log.model2 <- glm(dat_train$mvalue ~ age + rm, data=dat_train, family = binomial)
#model summary
summary(log.model2)

#Q9. find the number of predicted house price below median with threshold of 0.2 on training set
# Compute the actual class of mvalue for all the data in the dat_train
actual.class <- ifelse(dat_train$mvalue == 0, 1, 0)
# Classify the predictions based on the threshold of 0.2
predicted_classes <- ifelse(predictions > 0.2, "above", "below")
# Count the number of correct classifications
sum(actual.class == class.pred)


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


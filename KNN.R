
# Week 4 assignment: KNN (solutions) --------------------------------------

dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)
# to find out the variables contained we use the function names()
names(dat_train)
# summary() provides summary statistics for each variable
summary(dat_train)

# Q1: Which of the following insights you get through visual inspection is true
# check the proportions
plot(dat_train[ ,13], main = "Median Value")
plot(dat_test[ ,13], main = "Median Value")
# check correlations
cor(dat_train[, -13])
# A: The proportion between train and test set of the target variable is similar
# A: Job centers tend to located in closer proximity of older neighborhoods
# Proximity to highways substantially lowers the tax-value
# The target variable mvalue is imbalanced

# Q2: From a boxplot how many outliers are in the variable of proportional residential land zoned for lots over 25,000 ft$^2$ in the training set?
boxplot(dat_train$zn)
# <!-- 9 -->
# <!-- 10 -->
# <!-- 4 -->
# <!-- A: We cannot use boxplot in this case -->
# Note: As we can clearly see the distribution is not normal and therefore outlier detection for variables 
# is not possible

# scale values for analysis
Xtrain <- scale(dat_train[, -13])
Xtest <- scale(dat_test[, -13])
# define the target variables for training and test set
cl <- dat_train$mvalue
Xval <- dat_test$mvalue

# Q3: What is the misclassification rate on your test set for $k=12$? (provide percent as integer)require(class)
# make sure we get multiple replications to address random split of ties
q3 <- matrix(NA, nrow = 1000, ncol = 2)

for (i in 1:1000) {
  knn.pred <- knn(Xtrain, Xtest, cl, k=12, prob = T)
  # Compute misclassification error on the test set (round to two decimals)
  q3[i, 1] <- round(mean(knn.pred != Xval), 2)*100
  q3[i, 2] <- as.integer(mean(knn.pred != Xval)*100)
  
}
min(q3)
max(q3)
# A: values between 11 and 16

# Q4: What does the misclassification rate mean?
# A: The percentage of a class being assigned to the wrong class
# The percentage of class being assigned correctly to the value "above"
# The percentage of class being assigned correctly to the value "below"
# None of the above

# Q5: Given your initial k-NN model. How many of the neighbors belong to the
# class "above" for the example \hat{P}(above|test[109]) = 0.75?
knn.prob <- attr(knn.pred, "prob")
knn.prob[109]
# 0.75
# We multiply the probablity with number of neighbors (k=12)
knn.prob*12
# A: 9

# Q6: How many predictions in your test set are ties?
length(which(knn.prob == 0.5))
# A: 6

# Q7: What does a tie mean for a k-NN algorithm?
# ?knn() manual and tutorial answer
# A: The algorithm will randomly select a class 
# A: The algorithm will randomly select out of the labels that have the highest estimated probability
# The algorithm will choose the class that is represented more often in the training data
# The algorithm will revert to a uneven number of k and re-run this data point

# Note:
# Awarded full points for:
# - The algorithm will randomly select a class (answer from ?knn() manuel)
# - The algorithm will randomly select out of the labels that have the highest estimated probability (answer how it is discribed in the tutorial outline)
# The following answers are wrong:
# - The algorithm will choose the class that is represented more often in the training data (this will lead to a bias towards the majority class. The algorithm probably tends into this direction anyways already)
# - The algorithm will revert to an uneven number of k and re-run this data point (this would include a lot of additional costs)

# Q8: What does a model with a high value of k differentiate from a model with a small value of k?
# A: The model considers more neighbors
# A: The model is less complex
# A: The model becomes less agile
# The model is more likely to over-fit
# The model is more prone to outliers

kvals <- c(1:50)
TrainError = rep(NA,length(kvals))
TestError =  rep(NA,length(kvals))
set.seed(42)
for (K in kvals) {
  # Get predicted labels for training data
  knn.pred <- knn(Xtrain, Xtrain, cl, k=K)
  # Compute misclassification error on the training set
  TrainError[K] <- mean(knn.pred != cl)
  
  # Get predicted labels for test data
  knn.pred <- knn(Xtrain, Xtest, cl, k=K)
  # Compute misclassification error on the test set
  TestError[K] <- mean(knn.pred != Xval)
}

# Plot the misclassification error of the train and the test set
ymax = max(max(TrainError), max(TestError))
plot(kvals, TrainError, type="b", cex=0.5, col="blue", xlab = "k", 
     ylab="Misclassification Error", ylim=c(0,ymax))
lines(kvals, TestError, type="b", cex=0.5, col="red")

# Add legend so reader can distinguish what each line represents
legend("topleft", c("Train","Test"),cex=1.,col=c("blue","red"), 
       pch=c(1,1), lty=c(1,1))

# Q9: What can you interpret from the produced misclassification plot?
# A: We should be careful in trusting this model as there is no stable misclassification error in the training data
# A: On average the model is roughly 35% points better than a random classifier
# Best model we should use to predict the house price is k=1
# The increasing misclassification error is a sign of over-fitting



### PCA
# apply PCA for variable selection
pc <- prcomp(Xtrain)

# Create proportion of variance explained plot
pve = cumsum(pc$sdev^2)/sum(pc$sdev^2)
plot(pve, t="b", col="blue", xlab="PC", xaxp=c(1,13,3), 
     main="Proportion of variance explained", ylab="")

# How many PCA components do we need to explain at least 90% of the variance?
which(pve > 0.9)[1]
# A: 7

# apply PCA for variable selection
pc <- prcomp(Xtrain)
# We use the first 7 components that explain 90% of the variance
pcTrain <- pc$x[, 1:7]
# We can predict PCA values for unseen data
pcTest <- predict(pc, newdata=Xtest)[, 1:7]

TrainErrorPCA = rep(NA,length(kvals))
TestErrorPCA =  rep(NA,length(kvals))

set.seed(42)
kvals <- c(1:50)
overallTest <- NULL
overallTrain <- NULL

# testing 10000 runs if to test if reliable
for (i in 1:1000) {

  for (K in kvals) {
    # Get predicted labels for training data
    knn.pred <- knn(pcTrain, pcTrain, cl, k=K)
    # Compute misclassification error on the training set
    TrainErrorPCA[K] <- mean(knn.pred != cl)
    
    # Get predicted labels for test data
    knn.pred <- knn(pcTrain, pcTest, cl, k=K)
    # Compute misclassification error on the test set
    TestErrorPCA[K] <- mean(knn.pred != Xval)
  }
overallTest[i] <- mean(TestError) - mean(TestErrorPCA)
overallTrain[i] <- mean(TrainError) - mean(TrainErrorPCA)
}

# Q9: From the two models (PCA vs. non-PCA), which model shows on average a
# better classification performance on the training and test sets?
min(overallTest)
max(overallTest)

min(overallTrain)
max(overallTrain)

ymax = max(max(TrainErrorPCA), max(TestErrorPCA))
plot(kvals, TrainErrorPCA, type="b", cex=0.5, col="blue", xlab = "k", 
     ylab="Misclassification Error", ylim=c(0,ymax))
lines(kvals, TestErrorPCA, type="b", cex=0.5, col="red")
lines(kvals, TrainError, type="b", cex=0.5, col="purple")
lines(kvals, TestError, type="b", cex=0.5, col="green")
# Add legend so reader can distinguish what each line represents
legend("topleft", c("TrainPCA","TestPCA", "Train-NonPCA", "Test-NonPCA"),cex=1.,col=c("blue","red", "purple", 'green'), 
       pch=c(1,1), lty=c(1,1))

# A: On average the PCA model outperforms the non-PCA model on the train but trails on the test set
# A: On average the PCA model outperforms the non-PCA model on the train and test set
# On average the PCA model trails the non-PCA model on the train and test set
# On average the PCA model trails the non-PCA model but outperforms it on the test set

# I also award the following answer with full credit as it can also be derived on depending your sample you can derive:
# - On average the PCA model outperforms the non-PCA model on the train and test set
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = T)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = T)

#to find out the variables contained we use the function name()
names(dat_train)

# summary() provides summary statistics for each variable
summary(dat_train)


#Q2.PLOT BOXPLOT
boxplot(dat_train$zn, col='red', notch = T)

#Q3.SCALE VALUES FOR ANALYSIS
require(class)
?knn
Xtrain <- scale(dat_train[, -13])
Xtest <- scale(dat_test[, -13])
# define the target variables for training and test set
cl <- dat_train$mvalue
Xval <- dat_test$mvalue
kvals <- seq(1, nrow(Xtrain))
TestError <- rep(0,length(kvals))
for (K in kvals) {
  # Get predicted labels for testing data
  knn.pred <- knn(Xtrain, Xtest, cl, k=12)
  # Compute misclassification error on the training set
  TestError[12] <- mean(knn.pred != Xval)
}
TestError[12]

#Q6. PREDICTION IN TEST SET ARE TIED
knn.pred <- knn(Xtrain, Xtest, cl, k=12, prob=TRUE)
# To access the attribute "prob" we use the attr() function in R
knn.prob <- attr(knn.pred, "prob")
knn.prob
sum(knn.prob == 0.5)

#Q9. MISCLASSIFICATION PLOT
set.seed(42)
kvals <- seq(1, 50)
TrainError <- rep(0,length(kvals))
TestError <- rep(0,length(kvals))
for (K in kvals) {
  # Get predicted labels for training data
  knn.pred <- knn(Xtrain, Xtrain, cl, k=K)
  # Compute misclassification error on the training set
  TrainError[K] <- mean(knn.pred != cl)
  # Get predicted labels for testing data
  knn.pred <- knn(Xtrain, Xtest, cl, k=K)
  # Compute misclassification error on the testing set
  TestError[K] <- mean(knn.pred != Xval)
}

plot(kvals, TrainError, type="b", cex=0.5, col="blue",
     xlab = "k", ylab="Misclassification Error", ylim=c(0,max(c(TrainError, TestError))))
lines(kvals, TestError, type="b", cex=0.5, col="red")
# Add legend so reader can distinguish what each line represents
legend("topright", c("Train","Test"), cex=1., col=c("blue","red"),
       pch=c(1,1), lty=c(1,1))

#Q10 PCA 
# apply PCA for variable selection (note this data is assumed to be scaled)
pc <- prcomp(Xtrain)
# setting p to numbers of PCA needed to explain 90% variability (not this number
# is arbitrary for demonstration only)
d <- 12 # NOT THE SOLUTION!!!
# We use the first x components that explain 90% of the variance
pcTrain <- pc$x[, 1:d]
# We can predict PCA values for unseen data
pcTest <- predict(pc, newdata=Xtest)[, 1:d]
# Set seed before running KNN
set.seed(42)
kvals <- seq(1, 50)
TrainError <- rep(0, length(kvals))
TestError <- rep(0,length(kvals))

# Loop over each value of k
for (j in 1:length(kvals)) {
  # Get predicted labels for the training data
  knn.pred <- knn(Xtrain_pca, Xtrain_pca, cl, k=kvals[j])
  # Compute misclassification error on the training set
  TrainError[j] <- mean(knn.pred != cl)
  
  # Get predicted labels for the test data
  knn.pred <- knn(Xtrain_pca, Xtest_pca, cl, k=kvals[j])
  # Compute misclassification error on the test set
  TestError[j] <- mean(knn.pred != Xval)
}

# Plot misclassification error for each value of k
plot(kvals, TestError, type="l", col="red", ylim=c(0,1),
     xlab="k", ylab="Misclassification Error")
lines(kvals, TrainError, col="blue")
legend("topright", c("Test Error", "Train Error"), col=c("red", "blue"), lty=1)


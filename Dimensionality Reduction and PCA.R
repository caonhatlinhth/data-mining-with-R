#Read the file
cars <- read.csv("automobile.csv", stringsAsFactors = TRUE)
names(cars)

#store "make" column as "make variable
make <-cars$make

#Remove "make" column from dataset
cars = subset(cars, select = -c(make))

#Q4: Find valuable in the dataset after remove "make" column
ncol(cars)

#Q5: Fine the number of NA value
cars[cars == "?"] <-- NA
sum(is.na(cars))

#Change data to numeric data
cars1 <- apply(cars, 2, function(x) as.numeric(x))

library(softImpute)
library(Matrix)


#scale the column of dataset
datScal <- biScale(cars1, col.center=T, col.scale=T, row.scale=F, row.center = F)

# fit softImpute using the soft-thresholded singular value decomposition method
fit1 <- softImpute(datScal, rank.max = 12, lambda = 10, trace=TRUE, type="svd")

# store results into a new variable
res <- complete(cars1, fit1, unscale = TRUE)

#Complete the missing values
cars_complete = complete(cars1, fit1)

#Q6: price of Isuzu missing cars
round(cars_complete[45,13], 0)
round(cars_complete[46,13], 0)

#Remove all NA values
cars = na.omit(cars)

#Q7: check the remain row in dataframe
nrow(cars)

# Q8: pairwise correlation of numerical attributes
cars = apply(cars, 2, function(x) as.numeric(x))
cor(cars[, 1:13])

# Q9
dimnames(cars)

#apply function mean of 2nd dimension
apply(cars, 2, mean)

#apply function var along 2nd dimension
apply(cars, 2, var)

#cpa setting scale=TRUE stndardizes variables
pc <- prcomp(cars, scale=TRUE)
pc

#proportion of variance captured by each pc
pro <- pc$sdev^2/sum(pc$sdev^2)
pro

# Proportion of variance explained when using first q CPs
pve <- cumsum(pro)

# Create proportion of variance explained plot
plot(pve, t="b", col="blue", xlab="PC", xaxp=c(1,13,12),
     main="Proportion of variance explained", ylab="")

# Q10: biplot
biplot(pc, scale = 0, cex=0.6, main="Biplot")

# Q11:
plot(pc$x[,1], pc$x[,2], col = make, xlab = "PC1", ylab = "PC2", main = "Cars")
legend("topright", levels(make), pch = 1, col = c("black", "red", "green", "blue", "orange", "purple", "cyan", "cadetblue", "brown", "magenta", "navy"))

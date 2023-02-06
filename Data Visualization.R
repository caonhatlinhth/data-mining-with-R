#Read the file and assign it to a variable name dat
dat <- read.csv("Default.csv", stringsAsFactors = TRUE)
attach(dat)
source("plot_fcns.R")


#Read the collumn income
yearly_income <- dat$income

#Create new variable month which present the monthly income
month <- dat$income/12
dat["month"]=month

#Q1: Check the variable types
str(dat)

#Access the bottom 100 entries of dataset
last_entries <- tail(dat,100)

#Q2: Find the average balance of the bottom 100 entries of dataset and round the result to two decimals
round(sum(last_entries$balance)/100, digits=2)

#Q3: Find standard deviation of the last 100 entries in the dataset and round to two decimals
round(sd(dat$balance[9901:10000]), digits=2)

#Q4: Find top 3 income in the dataset
head(sort(dat$income, decreasing=TRUE), n=3)

#Q5: Number of customers having credit balance that is more than monthly income
sum(dat$balance > month)

#Q6: Percent of customers with a credit balance that is more than monthly income are student
round((sum(dat$student=="Yes" & dat$balance > month)/nrow(dat))*100, digits=2)

#Q7: Percent of customers with a credit balance that is more than their monthly income have defaulted
round((sum(dat$defaulted=="Yes" & dat$balance > month)/nrow(dat))*100, digits=2)

#Q8: average amount of time to pay off balance if putting all income
round((sum(dat$balance/month))/nrow(dat), digits=2)

#Q9: Average amount of time to pay off balance if putting quarter income
round((sum(dat$balance/(month/4)))/nrow(dat), digits=2)

#Create "time" variable for number of months it take for customers to pay off their balance if they put quarter of income toward their balance
dat["time"]=time
time <- dat$balance/(month/4)

#Q10: Number of people need more than 6 months to pay their balance if putting quarter of income
sum(time>6)

#Q11: Barplot shows number of people have defaulted vs those don't have defaulted
barp(dat, "defaulted")

#Q12: Barplot shows defaulting of non-students and students
cc_barplot(Data=dat, "student", "defaulted", freq = "condprob")

#Q16: scatterplot matrix
install.packages("tidyverse")
library("tidyverse")
pairs(~month + balance + time, data=dat, lower.panel=panel.smooth, col=defaulted)

#Q17: boxplot  presenting balance between students and non-students
cc_boxplot(dat, "balance", "student")

#Q18: boxplot comparing time variable of students and non-students
dat["time"] = time
cc_boxplot(dat, "time", "student")
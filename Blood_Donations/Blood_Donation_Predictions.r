# Source: This problem has been taken from the site http://www.drivendata.org/. It is the "Warm Up: Predict Blood Donations" challenge hosted by DrivenData.
# Problem Overview: Donated blood remains a critical resource during emergencies. This dataset is from a mobile blood donation vehicle in Taiwan. 
# The vehicle has been collecting blood from different universities.
# Goal: We want to predict whether or not a donor will give blood the next time the vehicle comes to campus.
# For more details, please visit: http://www.drivendata.org/competitions/2/


library(psych)
library(car)
library(plyr)
library(corrplot)
# Step 1: Get the data. 
# Download the training and test set data from the website and load it in to R.
setwd( "/Users/jananee009/Zananee/Projects/Machine_Learning_Algorithms/Blood_Donations")
blood_don_train = read.csv("blood_donation_train.csv",header=TRUE)
blood_don_test = read.csv("blood_donation_test.csv",header=TRUE)

# Step 2: Examine the training set data. Clean it and prepare it if needed.
head(blood_don_train)
str(blood_don_train)

#renaming columns in both training set and test set.
blood_don_train = rename(blood_don_train, c("Months.since.Last.Donation"="Months_since_Last_Donation", "Number.of.Donations"="Number_of_Donations", "Total.Volume.Donated..c.c.."="Total_Volume_Donated_cc", "Months.since.First.Donation"="Months_since_First_Donation", "Made.Donation.in.March.2007"="Made_Donation_in_March_2007"))
blood_don_test = rename(blood_don_test, c("Months.since.Last.Donation"="Months_since_Last_Donation", "Number.of.Donations"="Number_of_Donations", "Total.Volume.Donated..c.c.."="Total_Volume_Donated_cc", "Months.since.First.Donation"="Months_since_First_Donation"))

# Convert the dependent variable "Made.Donation.in.March.2007" from int to factor.
blood_don_train$Made_Donation_in_March_2007 = factor(blood_don_train$Made_Donation_in_March_2007, levels = c(0, 1), labels = c("Not_Donated", "Donated"))

summary(blood_don_train)
# we can see that there are no missing data in the dataset.


describe(blood_don_train[,c("Months_since_Last_Donation","Number_of_Donations","Total_Volume_Donated_cc","Months_since_First_Donation")])

# Plot the data to check for skews, outliers, etc 

# Let us plot the "Months_since_Last_Donation" variable.
hist(blood_don_train$Months_since_Last_Donation, prob=T)
lines(density(blood_don_train$Months_since_Last_Donation))
# we can see that the variable "Months_since_Last_Donation" is not normally distributed. The distribution is multimodal.

boxplot(blood_don_train$Months_since_Last_Donation, ylab="Months_since_Last_Donation") 
# There are 4 values that are more than 1.5*IQR value i.e. these are outliers. We may want to remove those cases where the "Months_since_Last_Donation" > 60.
abline(h=mean(blood_don_train$Months_since_Last_Donation,na.rm=T), lty=2)
rug(jitter(blood_don_train$Months_since_Last_Donation), side = 2)

# Let us plot the "Number_of_Donations" variable.
hist(blood_don_train$Number_of_Donations, prob=T)
lines(density(blood_don_train$Number_of_Donations))
# Distribution is positively skewed.
boxplot(blood_don_train$Number_of_Donations, ylab="Number_of_Donations") 
# There are several outliers. 

# Let us plot the "Total_Volume_Donated_cc" variable.
hist(blood_don_train$Total_Volume_Donated_cc, prob=T)
lines(density(blood_don_train$Total_Volume_Donated_cc))
# Distribution is positively skewed.
boxplot(blood_don_train$Total_Volume_Donated_cc, ylab="Total_Volume_Donated_cc") 
# There are several outliers. 

# Let us plot the "Months_since_First_Donation" variable.
hist(blood_don_train$Months_since_First_Donation, prob=T)
lines(density(blood_don_train$Months_since_First_Donation))
# Distribution is not normal.

# Do the Shapiro Wilk test.
shapiro.test(blood_don_train$Months_since_First_Donation)
# W = 0.9304, p-value = 1.033e-15. Since value of p < 0.001, data is significantly not normal.
boxplot(blood_don_train$Months_since_First_Donation, ylab="Months_since_First_Donation") 
abline(h=mean(blood_don_train$Months_since_First_Donation,na.rm=T), lty=2)
# There are several outliers.

# Test for homogenity of variance

leveneTest( blood_don_train$Months_since_Last_Donation, blood_don_train$Made_Donation_in_March_2007 )
# Levene's test is significant (p<0.01).Hence, the assumption of homogenity of variance has been violated.

leveneTest( blood_don_train$Number_of_Donations, blood_don_train$Made_Donation_in_March_2007 )
# Levene's test is significant (p<0.01).Hence, the assumption of homogenity of variance has been violated.

leveneTest( blood_don_train$Total_Volume_Donated_cc, blood_don_train$Made_Donation_in_March_2007 )
# Levene's test is significant (p<0.01).Hence, the assumption of homogenity of variance has been violated.

leveneTest( blood_don_train$Months_since_First_Donation, blood_don_train$Made_Donation_in_March_2007 )
# Levene's test is significant (p<0.01).Hence, the assumption of homogenity of variance has been violated.

# Analyze the two groups of data separately.
# Create 2 new datafarames for each group.
donated_data = subset(blood_don_train,blood_don_train$Made_Donation_in_March_2007=="Donated")
notdonated_data = subset(blood_don_train,blood_don_train$Made_Donation_in_March_2007=="Not_Donated")

# Plot histograms for each predictor in the donated_data.
hist(donated_data$Months_since_Last_Donation, prob=T)
lines(density(donated_data$Months_since_Last_Donation))
# Data is positively skewed.

hist(donated_data$Number_of_Donations, prob=T)
lines(density(donated_data$Number_of_Donations))
# Data is positively skewed.

hist(donated_data$Total_Volume_Donated_cc, prob=T)
lines(density(donated_data$Total_Volume_Donated_cc))
# Data is positively skewed.

hist(donated_data$Months_since_First_Donation, prob=T)
lines(density(donated_data$Months_since_First_Donation))
# Data is positively skewed.

# Plot histograms for each predictor in the notdonated_data.
hist(notdonated_data$Months_since_Last_Donation, prob=T)
lines(density(notdonated_data$Months_since_Last_Donation))
# Data is positively skewed.

hist(notdonated_data$Number_of_Donations, prob=T)
lines(density(notdonated_data$Number_of_Donations))
# Data is positively skewed.

hist(notdonated_data$Total_Volume_Donated_cc, prob=T)
lines(density(notdonated_data$Total_Volume_Donated_cc))
# Data is positively skewed.

hist(notdonated_data$Months_since_First_Donation, prob=T)
lines(density(notdonated_data$Months_since_First_Donation))
# Data is positively skewed.




# Step 3: Build a statistical model using the training data set. Since, this is a classification problem, We will use logistic regression.
blood_don_model.1 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Total_Volume_Donated_cc+Months_since_First_Donation, data=blood_don_train, family=binomial())

summary(blood_don_model.1)
# we see that the estimate for Total_Volume_Donated_cc is NA. Hence we remove it and create a new  model .

blood_don_model.2 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_train, family=binomial())
# From the p values we can see that the coefficients of all independent variables are siginificantly different from 0. 

#Calculate model chi square statistic:
modelChi.blood_don = blood_don_model.2$null.deviance - blood_don_model.2$deviance
chidf.blood_don = blood_don_model.2$df.null - blood_don_model.2$df.residual
chisq.prob.blood_don = 1-pchisq(modelChi.blood_don,chidf.blood_don)
# 1.110223e-16
# Since the p value <0.001, we can reject the null hypothesis that the model is not better at predicting the outcomes.

#Test for multicollinearity.
vif(blood_don_model.2)
# All values are less than 10. Hence, we can assume that our data does not have multicollinearity problem.

# Obtain the correlation matrix and visualize it.
corrMatrix = cor(blood_don_train[,c("Months_since_Last_Donation","Number_of_Donations","Total_Volume_Donated_cc","Months_since_First_Donation")])
corrplot(corrMatrix,method = "circle",title="Correlation Matrix For All  Predictors")

# Test for linearity of the logit.
# Create all interaction terms.
blood_don_train$logMonths_since_Last_DonationInt = log(blood_don_train$Months_since_Last_Donation)*blood_don_train$Months_since_Last_Donation
blood_don_train$logNumber_of_DonationsInt = log(blood_don_train$Number_of_Donations)*blood_don_train$Number_of_Donations
blood_don_train$logMonths_since_First_DonationInt = log(blood_don_train$Months_since_First_Donation)*blood_don_train$Months_since_First_Donation

# create a new logistic regression model including all the interaction terms along with regular terms.
blood_don_model.3 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation+logMonths_since_Last_DonationInt+logNumber_of_DonationsInt+logMonths_since_First_DonationInt, data=blood_don_train, family=binomial())

summary(blood_don_model.3)
# We see that the interaction term for Number_of_Donations is significant (p value = 0.008715). hence the assumption of linearity for "Number_of_Donations" is violated.

# Step 4: Evaluate the model by testing it on the test set.
blood_don_test$predictedProbabilities <- predict(blood_don_model.2, newdata = blood_don_test, type = "response")

	
# If the predicted probability is >= 0.5, classify it as donated, else not donated.
blood_don_test$prediction = ifelse(blood_don_test$predictedProbabilities>=0.4,1,0)
blood_don_test$prediction = factor(blood_don_test$prediction, levels = c(0, 1))

# prepare for submission# 1. 
my_prediction_1 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_1) = colnames(submission_format)
write.csv(my_prediction_1, "my_prediction_1.csv", row.names=FALSE)
# Score: 0.4457

# Attempt No. 2.
# We know that the predictors "Number_of_Donations " and "Total_Volume_Donated_cc" are highly correlated. 
# In the previous attempt, we built a model using the "Number_of_Donations" predictor and we excluded the "Total_Volume_Donated_cc" predictor.
# Now, we build a new model using the "Total_Volume_Donated_cc" predictor and we will exclude the "Number_of_Donations" predictor.
blood_don_model.4 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Total_Volume_Donated_cc+Months_since_First_Donation, data=blood_don_train, family=binomial())

summary(blood_don_model.4)
# We see that the AIC and th residual deviance for blood_don_model.2 and blood_don_model.4 are the same. 
# Hence, using "Total_Volume_Donated_cc" instead of "Number_of_Donations" should make very little to no  difference to the accuracy of the model.

# Test for the linearity of the logit.
# Create the interaction terms.
blood_don_train$logTotal_Volume_Donated_ccInt = log(blood_don_train$Total_Volume_Donated_cc)*blood_don_train$Total_Volume_Donated_cc
blood_don_train$logNumber_of_DonationsInt = NULL

blood_don_model.5 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Total_Volume_Donated_cc+Months_since_First_Donation+logMonths_since_Last_DonationInt+logTotal_Volume_Donated_ccInt+logMonths_since_First_DonationInt, data=blood_don_train, family=binomial())

summary(blood_don_model.5)
# We see that the interaction term for Total_Volume_Donated_cc is significant (p value = 0.00871). Hence the assumption of linearity for "Total_Volume_Donated_cc" is violated.

# Use the model to predict on test set
blood_don_test$predictedProbabilities = predict(blood_don_model.4, newdata = blood_don_test, type = "response")


# prepare for submission# 2. 
my_prediction_2 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_2) = colnames(submission_format)
write.csv(my_prediction_2, "my_prediction_2.csv", row.names=FALSE)

# Score: 0.4457

# Attempt No. 3
# We know that the predictors "Number_of_Donations " and "Total_Volume_Donated_cc" are highly correlated. 
# In the previous attempts, we built a model using either one of these predictors.
# Now, we build a new model using the the product of "Total_Volume_Donated_cc" and "Number_of_Donations" predictors.
blood_don_train$numberOfDonationsAndTotalVolumeDonated = blood_don_train$Number_of_Donations * blood_don_train$Total_Volume_Donated_cc

# build the model
blood_don_model.6 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+numberOfDonationsAndTotalVolumeDonated+Months_since_First_Donation, data=blood_don_train, family=binomial())
summary(blood_don_model.6)
# we see that the Residual deviance has increased to 573.65 from 556.61 in the previous models. Hence we discard this model.  

# Attempt No. 4
# Since all of our predictor variables are positively skewed, we will transform the data and see if we can improve the accuracy of our model.
# Taking log transformation is a good way to reduce positive skew. Hence, we will apply log transformation to all our predictor variables.

# Since "Months_since_Last_Donation" has some zeroes, we will add the constant 1 to all values in this column before we apply log transformation
blood_don_train_log = blood_don_train
blood_don_train_log$Months_since_Last_Donation = log(blood_don_train$Months_since_Last_Donation +  1)
blood_don_train_log$Number_of_Donations = log(blood_don_train$Number_of_Donations)
blood_don_train_log$Total_Volume_Donated_cc = log(blood_don_train$Total_Volume_Donated_cc)
blood_don_train_log$Months_since_First_Donation = log(blood_don_train$Months_since_First_Donation)

# Plot each predictor variable and check for normality. We can see that the positive skew is reduced considerably. 
hist(blood_don_train_log$Months_since_Last_Donation, prob=T)
lines(density(blood_don_train_log$Months_since_Last_Donation))

hist(blood_don_train_log$Number_of_Donations, prob=T)
lines(density(blood_don_train_log$Number_of_Donations))

hist(blood_don_train_log$Months_since_First_Donation, prob=T)
lines(density(blood_don_train_log$Months_since_First_Donation))

# Do the Shapiro Wilk test.
shapiro.test(blood_don_train_log$Months_since_Last_Donation)
#  Since value of p < 0.001, data is significantly not normal.
shapiro.test(blood_don_train_log$Number_of_Donations)
#  Since value of p < 0.001, data is significantly not normal.
shapiro.test(blood_don_train_log$Months_since_First_Donation)
#  Since value of p < 0.001, data is significantly not normal.

# Build model using the log transformed data.
blood_don_model.7 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_train_log, family=binomial())
summary(blood_don_model.7) 
# We see that the residual deviance is 551.62, the lowest obtained till now.

# Evaluate the model on the test set. 
# First log transform the test set.
blood_don_test_log = blood_don_test
blood_don_test_log$Months_since_Last_Donation = log(blood_don_test$Months_since_Last_Donation +  1)
blood_don_test_log$Number_of_Donations = log(blood_don_test$Number_of_Donations)
blood_don_test_log$Total_Volume_Donated_cc = log(blood_don_test$Total_Volume_Donated_cc)
blood_don_test_log$Months_since_First_Donation = log(blood_don_test$Months_since_First_Donation)

# Use the model to predict on test set
blood_don_test_log$predictedProbabilities = predict(blood_don_model.7, newdata = blood_don_test_log, type = "response")
# prepare for submission# 3. 
my_prediction_3 = blood_don_test_log[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_3) = colnames(submission_format)
write.csv(my_prediction_3, "my_prediction_3.csv", row.names=FALSE)

# Score: 0.4654.

# Attempt No. 5
# Using log transformation increased the error rather than decreasing it. Now, we will use a now algorithm called Naive Bayes to build the model.
# Remove the unwanted interaction columns from the training data set.
blood_don_train$logMonths_since_Last_DonationInt = NULL
blood_don_train$logMonths_since_First_DonationInt = NULL
blood_don_train$logTotal_Volume_Donated_ccInt = NULL
blood_don_train$numberOfDonationsAndTotalVolumeDonated = NULL

# Build the model using Naive Bayes algorithm
library(e1071)
blood_don_model.8 = naiveBayes(blood_don_train[,c(-1,-6)], blood_don_train$Made_Donation_in_March_2007,type="raw")
summary(blood_don_model.8)
# Evaluate the model on the test set. 
blood_don_pred = predict(blood_don_model.8, blood_don_test,type="raw")
blood_don_test$predictedProbabilities = blood_don_pred[,"Donated"]

# prepare for submission# 4. 
my_prediction_4 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_4) = colnames(submission_format)
write.csv(my_prediction_4, "my_prediction_4.csv", row.names=FALSE)

# Score: 0.6829

# Attempt No. 6
# We will use a now algorithm called Support Vector to build the model.
#
library(kernlab)
blood_don_model.9 = ksvm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Total_Volume_Donated_cc+Months_since_First_Donation,data=blood_don_train,kernel="rbfdot",prob.model=TRUE)
summary(blood_don_model.9)
# Evaluate the model on the test set. 
blood_don_pred = predict(blood_don_model.9, blood_don_test,type="probabilities")
blood_don_test$predictedProbabilities = blood_don_pred[,"Donated"]

# prepare for submission# 5. 
my_prediction_5 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_5) = colnames(submission_format)
write.csv(my_prediction_5, "my_prediction_5.csv", row.names=FALSE)

# Score: 0.4769

# Attempt No. 7
# We will  apply support vector machine algorithm to the log transformed data.
blood_don_model.10 = ksvm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Total_Volume_Donated_cc+Months_since_First_Donation, data=blood_don_train_log, fkernel="rbfdot",prob.model=TRUE)
summary(blood_don_model.10) 
# Use the model to predict on test set
blood_don_pred_log = predict(blood_don_model.10, blood_don_test_log, type = "probabilities")
blood_don_test_log$predictedProbabilities = blood_don_pred_log[,"Donated"]
# prepare for submission# 6. 
my_prediction_6 = blood_don_test_log[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_6) = colnames(submission_format)
write.csv(my_prediction_6, "my_prediction_6.csv", row.names=FALSE)

# Score: 0.4663

# Attempt No. 8
# Here we will split our training data in to training set and test set. We will build our model on the training data. 
# We will test the model on the test set that we obtained from our training data. 
# We will know the error on the test set before we apply the model to the blind test set (the one for which we dont know the outcome variable).

# when we examined our data in the beginning, we identified some outliers in the "Months_since_Last_Donation" and"Number_of_Donations". 


blood_don_train[blood_don_train$Months_since_Last_Donation>=60,]
# row number: 386, 576
blood_don_train[blood_don_train$Number_of_Donations>=40,]
# row numbers: 1,9,387,389

# Let us remove those outliers and run logistic regression again. 
blood_don_train_no_out = blood_don_train[-c(1,9,386,387,389,576),]

# create our own training set and test set.
 
set.seed(123)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(blood_don_train_no_out)),size=450,replace=F)
blood_don_set.trng = blood_don_train_no_out[trng_ind,]
blood_don_set.test = blood_don_train_no_out[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
 prop.table(table(blood_don_set.trng$Made_Donation_in_March_2007))
 prop.table(table(blood_don_set.test$Made_Donation_in_March_2007))

 # Build logistic regression model using training set data.
 blood_don_model.11 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_set.trng, family=binomial())

# Evaluate the model by testing it on the test set.
blood_don_set.test$predictedProbabilities <- predict(blood_don_model.11, newdata = blood_don_set.test, type = "response")

#  Find log loss
# First create a separate column that contains 1s and 0s for Donated and Not_Donated respectively. 
blood_don_set.test$actual = 0
blood_don_set.test$actual[blood_don_set.test$Made_Donation_in_March_2007=="Donated"] = 1

# calculate log loss for each case
blood_don_set.test$logLoss = ( blood_don_set.test$actual*log(blood_don_set.test$predictedProbabilities)) + ( (1-blood_don_set.test$actual)*log(1-blood_don_set.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_set.test$logLoss)*(-1)

# logloss = 0.59222 (when logistic regression was applied on data as is.)
# logloss = 0.535031 (when logistic regression was applied on data after removing outliers.)

# Apply model on actual test set.
blood_don_test$predictedProbabilities = predict(blood_don_model.11, newdata = blood_don_test, type = "response")

# prepare for submission# 7. 

my_prediction_7 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_7) = colnames(submission_format)
write.csv(my_prediction_7, "my_prediction_7.csv", row.names=FALSE)

# Score: 0.4450

# Attempt No. 9
# Now that we have improved our score by removing outliers, let us apply log transformation to  the data and see if we can improve the score further.
# We will first try this out on our training and test set first.

# First, create log transformed training set and test set.
blood_don_train_no_out_log.trng = blood_don_set.trng
blood_don_train_no_out_log.trng$Months_since_Last_Donation = log(blood_don_set.trng$Months_since_Last_Donation +  1)
blood_don_train_no_out_log.trng$Number_of_Donations = log(blood_don_set.trng$Number_of_Donations)
blood_don_train_no_out_log.trng$Total_Volume_Donated_cc = log(blood_don_set.trng$Total_Volume_Donated_cc)
blood_don_train_no_out_log.trng$Months_since_First_Donation = log(blood_don_set.trng$Months_since_First_Donation)

blood_don_train_no_out_log.test = blood_don_set.test
blood_don_train_no_out_log.test$Months_since_Last_Donation = log(blood_don_set.test$Months_since_Last_Donation +  1)
blood_don_train_no_out_log.test$Number_of_Donations = log(blood_don_set.test$Number_of_Donations)
blood_don_train_no_out_log.test$Total_Volume_Donated_cc = log(blood_don_set.test$Total_Volume_Donated_cc)
blood_don_train_no_out_log.test$Months_since_First_Donation = log(blood_don_set.test$Months_since_First_Donation)

# Build logistic regression model using training set data.
 blood_don_model.12 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_train_no_out_log.trng, family=binomial())

 # Evaluate the model by testing it on the test set.
blood_don_train_no_out_log.test$predictedProbabilities <- predict(blood_don_model.12, newdata = blood_don_train_no_out_log.test, type = "response")

#  Find log loss
# First create a separate column that contains 1s and 0s for Donated and Not_Donated respectively. 
blood_don_train_no_out_log.test$actual = 0
blood_don_train_no_out_log.test$actual[blood_don_train_no_out_log.test$Made_Donation_in_March_2007=="Donated"] = 1

# calculate log loss for each case
blood_don_train_no_out_log.test$logLoss = ( blood_don_train_no_out_log.test$actual*log(blood_don_train_no_out_log.test$predictedProbabilities)) + ( (1-blood_don_train_no_out_log.test$actual)*log(1-blood_don_train_no_out_log.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_train_no_out_log.test$logLoss)*(-1)

#logloss = 0.535
# we see that there is no difference in the log loss as compared to our previous attempt. 

# Attempt No. 10
# Let us calculate residual statistics and indentify influential cases casewise diagnostics. 
# i.e. obtain standardized residuals, studentized residuals, etc to find influential cases.
# We will use the mode that we  built in attempt# 8. i.e. blood_don_model.11 and add new columns to the blood_don_set.trng and blood_don_set.test dataframes.

# create a new dataframe that will contain all the columns from the training data along with the columns for residual and influential statisitcs.
blood_don_set.trng_analsyis=blood_don_set.trng
blood_don_set.trng_analsyis$predictedProbabilities = fitted(blood_don_model.11)
blood_don_set.trng_analsyis$stdResiduals = rstandard(blood_don_model.11)
blood_don_set.trng_analsyis$studentResiduals = rstudent(blood_don_model.11)
blood_don_set.trng_analsyis$dfbeta = dfbeta(blood_don_model.11)
blood_don_set.trng_analsyis$dffit = dffits(blood_don_model.11)
blood_don_set.trng_analsyis$leverage = hatvalues(blood_don_model.11)
blood_don_set.trng_analsyis$cooksDistance = cooks.distance(blood_don_model.11)

averageLeverage = 5/450
# 0.01111111

boxplot(blood_don_set.trng_analsyis$stdResiduals, ylab="Standardized Residuals") 
boxplot(blood_don_set.trng_analsyis$cooksDistance, ylab="Cooks Distance") 
boxplot(blood_don_set.trng_analsyis$leverage, ylab="Leverage") 


#Export the residual statistics data in to csv to better examine them.
write.csv(blood_don_set.trng_analsyis, "ResidualStatistics.csv", row.names=TRUE)

# Some observations about the residual statistics:
# 1. none of the cases have absolute standardized residuals > 3.
# 2. Less than 1% of cases have absolute standardized residuals > 2.5
# 3. Less than 5% of cases have  absolute standardized residuals > 2
# 4. None of the cases have absolute values of dfbeta > 1.
# 5. There are a few cases that have leverage values > 3*average leverage value. 
# We will remove the case with the highest leverage value and create a new model (row id 264)

# 
blood_don_set.trng = blood_don_set.trng[-264,]

# Build logistic regression model using training set data.
 blood_don_model.13 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_set.trng, family=binomial())

# Evaluate the model by testing it on the test set.
blood_don_set.test$predictedProbabilities <- predict(blood_don_model.13, newdata = blood_don_set.test, type = "response")

#  Find log loss
# First create a separate column that contains 1s and 0s for Donated and Not_Donated respectively. 
blood_don_set.test$actual = 0
blood_don_set.test$actual[blood_don_set.test$Made_Donation_in_March_2007=="Donated"] = 1

# calculate log loss for each case
blood_don_set.test$logLoss = ( blood_don_set.test$actual*log(blood_don_set.test$predictedProbabilities)) + ( (1-blood_don_set.test$actual)*log(1-blood_don_set.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_set.test$logLoss)*(-1)
print(logloss)

#logloss = 0.5349

# Apply model on actual test set.
blood_don_test$predictedProbabilities = predict(blood_don_model.13, newdata = blood_don_test, type = "response")

# prepare for submission# 8. 

my_prediction_8 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_8) = colnames(submission_format)
write.csv(my_prediction_8, "my_prediction_8.csv", row.names=FALSE)

# Attempt No. 11
# In our exploratory data analysis, we saw that the predictors "Months_since_Last_Donation" and "Number_of_Donations" were positively skewed.
# Let us apply log transformation to only these two predictors and run logistic regression and build the model. 

# Create a ew training and testing dataframe by applying log transformation only to "Months_since_Last_Donation" and "Number_of_Donations".
newDataFrame.trng = blood_don_set.trng
newDataFrame.trng$Months_since_Last_Donation = log(newDataFrame.trng$Months_since_Last_Donation +  1)
newDataFrame.trng$Number_of_Donations = log(newDataFrame.trng$Number_of_Donations)

newDataFrame.test = blood_don_set.test
newDataFrame.test$Months_since_Last_Donation = log(newDataFrame.test$Months_since_Last_Donation +  1)
newDataFrame.test$Number_of_Donations = log(newDataFrame.test$Number_of_Donations)

# Build logistic regression model using training set data.
 blood_don_model.14 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=newDataFrame.trng, family=binomial())

 # Evaluate the model by testing it on the test set.
newDataFrame.test$predictedProbabilities <- predict(blood_don_model.14, newdata = newDataFrame.test, type = "response")

#  Find log loss
# First create a separate column that contains 1s and 0s for Donated and Not_Donated respectively. 
newDataFrame.test$actual = 0
newDataFrame.test$actual[newDataFrame.test$Made_Donation_in_March_2007=="Donated"] = 1

# calculate log loss for each case
newDataFrame.test$logLoss = ( newDataFrame.test$actual*log(newDataFrame.test$predictedProbabilities)) + ( (1-newDataFrame.test$actual)*log(1-newDataFrame.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(newDataFrame.test$logLoss)*(-1)
print(logloss)

#logloss = 0.531116

# Apply model on actual test set.
# First, apply log transformation on "Months_since_Last_Donation" and "Number_of_Donations" in the actual test set.
blood_don_test$Months_since_Last_Donation = log(blood_don_test$Months_since_Last_Donation +  1)
blood_don_test$Number_of_Donations = log(blood_don_test$Number_of_Donations)

blood_don_test$predictedProbabilities = predict(blood_don_model.14, newdata = blood_don_test, type = "response")

# prepare for submission# 9. 

my_prediction_9 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_9) = colnames(submission_format)
write.csv(my_prediction_9, "my_prediction_9.csv", row.names=FALSE)

# Error: 0.4478

# Attempt No. 12
# Use Linear Discriminant Algorithm  on the entire training dataset.
library(MASS)
blood_don_model.15 = lda(Made_Donation_in_March_2007~Months_since_Last_Donation+Total_Volume_Donated_cc+Months_since_First_Donation, data=blood_don_train, family=binomial())
blood_don_model.15

# Use the model to predict on test set
output = predict(blood_don_model.15, newdata = blood_don_test)
blood_don_test$predictedProbabilities = output$posterior[,"Donated"]


# prepare for submission# 10. 
my_prediction_10 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_10) = colnames(submission_format)
write.csv(my_prediction_10, "my_prediction_10.csv", row.names=FALSE)

# Error: 0.4559

# Attempt No. 13
# We know that Months_since_Last_Donation, Number_of_Donations  have some outliers. Let us remove those outliers and run the logistic regression model. 

blood_don_train[blood_don_train$Months_since_Last_Donation>=60,]
# row number: 386, 576

blood_don_train[blood_don_train$Number_of_Donations>=40,]
# row numbers: 1,9,387,389

# Let us remove those outliers and run logistic regression again. 
blood_don_train_no_out = blood_don_train[-c(1,9,386,387,389,576),]

blood_don_model.16 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_train_no_out, family=binomial())
# Residual Deviance = 549.5

# Use the model to predict on test set
blood_don_test$predictedProbabilities = predict(blood_don_model.16, newdata = blood_don_test, type = "response")


# prepare for submission# 11. 
my_prediction_11 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_11) = colnames(submission_format)
write.csv(my_prediction_11, "my_prediction_11.csv", row.names=FALSE)

# Error: 0.4480

# Attempt No. 14
# Let us remove more records that have outliers in Months_since_Last_Donation, Number_of_Donations. We will run logistic regression again.

blood_don_train[blood_don_train$Months_since_Last_Donation>=35,]
# row number: 385, 386, 575, 576

blood_don_train[blood_don_train$Number_of_Donations>=30,]
# row number: 1, 9, 264, 387, 389, 398

# Let us remove those outliers and run logistic regression again. 
blood_don_train_no_out = blood_don_train[-c(1,9,264,385,386,387,389,398,575,576),]

blood_don_model.17 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_train_no_out, family=binomial())
# Residual Deviance: 546.2. We can see that the residual deviance has reduced when compared to blood_don_model.16

# Use the model to predict on test set
blood_don_test$predictedProbabilities = predict(blood_don_model.17, newdata = blood_don_test, type = "response")

# prepare for submission# 12. 
my_prediction_12 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_12) = colnames(submission_format)
write.csv(my_prediction_12, "my_prediction_12.csv", row.names=FALSE)

# Error: 0.4519

# Attempt No. 15
# From the previous 2 attempts, we learnt that removing records with Months_since_Last_Donation >= 35 and Number_of_Donations>=30 didnt improve our model.
# Hence let us stick to removing records that have  Months_since_Last_Donation>=60 and Number_of_Donations>=40.

# Let us  derive a new column in the training set called avg_vol_donated_month = Total_Volume_Donated_cc / Number_of_Donations
# In the new column, we can see that the average volume of blood donated by each person is 250.
# This is true even for the blind test set. So, the newly derived column is useless. 

# Let us apply linear regression instead of logistic regression


blood_don_train[blood_don_train$Months_since_Last_Donation>=60,]
# row number: 386, 576

blood_don_train[blood_don_train$Number_of_Donations>=40,]
# row numbers: 1,9,387,389

# Let us remove those outliers and run logistic regression again. 
blood_don_train_no_out = blood_don_train[-c(1,9,386,387,389,576),]

set.seed(123)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(blood_don_train_no_out)),size=450,replace=F)
blood_don_set.trng = blood_don_train_no_out[trng_ind,]
blood_don_set.test = blood_don_train_no_out[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
 prop.table(table(blood_don_set.trng$Made_Donation_in_March_2007))
 prop.table(table(blood_don_set.test$Made_Donation_in_March_2007))

 # Build a linear regression model using training set data.
blood_don_model.18 = lm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_set.trng)

# Evaluate the model by testing it on the test set.
output = predict(blood_don_model.18, newdata = blood_don_set.test)

blood_don_set.test$predictedProbabilities  = ifelse(output > 1, 1, output)


#  Find log loss
# First create a separate column that contains 1s and 0s for Donated and Not_Donated respectively. 
blood_don_set.test$actual = 0
blood_don_set.test$actual[blood_don_set.test$Made_Donation_in_March_2007=="Donated"] = 1

# calculate log loss for each case
blood_don_set.test$logLoss = ( blood_don_set.test$actual*log(blood_don_set.test$predictedProbabilities)) + ( (1-blood_don_set.test$actual)*log(1-blood_don_set.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_set.test$logLoss)*(-1)

# logloss = 


# Apply model on actual test set.
output = predict(blood_don_model.18, newdata = blood_don_test)

blood_don_test$predictedProbabilities = ifelse(output > 1, 1, output)

# prepare for submission# 13. 
my_prediction_13 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_13) = colnames(submission_format)
write.csv(my_prediction_13, "my_prediction_13.csv", row.names=FALSE)







library(boot)
library(plyr)

setwd( "/Users/jananee009/Zananee/Projects/Machine_Learning_Algorithms/Blood_Donations")

blood_don_train = read.csv("blood_donation_train.csv",header=TRUE)
blood_don_test = read.csv("blood_donation_test.csv",header=TRUE)

blood_don_train = rename(blood_don_train, c("Months.since.Last.Donation"="Months_since_Last_Donation", "Number.of.Donations"="Number_of_Donations", "Total.Volume.Donated..c.c.."="Total_Volume_Donated_cc", "Months.since.First.Donation"="Months_since_First_Donation", "Made.Donation.in.March.2007"="Made_Donation_in_March_2007"))
blood_don_test = rename(blood_don_test, c("Months.since.Last.Donation"="Months_since_Last_Donation", "Number.of.Donations"="Number_of_Donations", "Total.Volume.Donated..c.c.."="Total_Volume_Donated_cc", "Months.since.First.Donation"="Months_since_First_Donation"))

blood_don_train$Made_Donation_in_March_2007 = factor(blood_don_train$Made_Donation_in_March_2007, levels = c(0, 1), labels = c("Not_Donated", "Donated"))

blood_don_train[blood_don_train$Months_since_Last_Donation>=60,]
# row number: 386, 576
blood_don_train[blood_don_train$Number_of_Donations>=40,]
# row numbers: 1,9,387,389

blood_don_train_no_out = blood_don_train[-c(1,9,264,386,387,389,576),]

# partition given training set in to training set and test set.
set.seed(12345)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(blood_don_train_no_out)),size=460,replace=F)
blood_don_set.trng = blood_don_train_no_out[trng_ind,]
blood_don_set.test = blood_don_train_no_out[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
prop.table(table(blood_don_set.trng$Made_Donation_in_March_2007))
prop.table(table(blood_don_set.test$Made_Donation_in_March_2007))

blood_don_set.test$actual = 0
blood_don_set.test$actual[blood_don_set.test$Made_Donation_in_March_2007=="Donated"] = 1

# Fit the model on the training set
blood_don_model.0 = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=blood_don_set.trng, family=binomial())

# Apply model on test set.
blood_don_set.test$predictedProbabilities  = predict(blood_don_model.0, newdata = blood_don_set.test, type = "response")

# calculate log loss for each case
blood_don_set.test$logLoss = ( blood_don_set.test$actual*log(blood_don_set.test$predictedProbabilities)) + ( (1-blood_don_set.test$actual)*log(1-blood_don_set.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_set.test$logLoss)*(-1)
print(logloss)
#logloss = 0.5142593


# implement non parametric bootstrap
# I will run the bootstrap 10,000 times on my training dataset. i.e. I will draw 10,000 samples from the training dataset with replacement. 
# On each of the 10,000 samples, I will run the logistic regression model. I will capture the values of coefficients, Standard errors, Z values and p values in different dataframes.
# Finally I will take a mean of all the coefficients values to compute the esimates of coefficient values. I will use these estimates as model on my blind test set.

# create empty dataframes to store estimates, std errors, z values and p values for each predictor for each of the 10,000 samples. 
bootstrapCoeff = data.frame(Intercept=numeric(0),Months_since_Last_Donation=numeric(0),Number_of_Donations=numeric(0),Months_since_First_Donation=numeric(0))
bootstrapStdErr= data.frame(Intercept=numeric(0),Months_since_Last_Donation=numeric(0),Number_of_Donations=numeric(0),Months_since_First_Donation=numeric(0))
bootstrapZValue = data.frame(Intercept=numeric(0),Months_since_Last_Donation=numeric(0),Number_of_Donations=numeric(0),Months_since_First_Donation=numeric(0))
bootstrapPValue = data.frame(Intercept=numeric(0),Months_since_Last_Donation=numeric(0),Number_of_Donations=numeric(0),Months_since_First_Donation=numeric(0))

for (i in 1:10000) {
	indices = sample(1:450,replace=T)
	d = blood_don_set.trng[indices,]
	fit = glm(Made_Donation_in_March_2007~Months_since_Last_Donation+Number_of_Donations+Months_since_First_Donation, data=d, family=binomial())
	
	# store all coefficient estimates in dataframe
	bootstrapCoeff[i,"Intercept"] = coef(summary(fit))["(Intercept)","Estimate"]
	bootstrapCoeff[i,"Months_since_Last_Donation"] = coef(summary(fit))["Months_since_Last_Donation","Estimate"]
	bootstrapCoeff[i,"Number_of_Donations"] = coef(summary(fit))["Number_of_Donations","Estimate"]
	bootstrapCoeff[i,"Months_since_First_Donation"] = coef(summary(fit))["Months_since_First_Donation","Estimate"]

	# store all coefficient std errors in dataframe
	bootstrapStdErr[i,"Intercept"] = coef(summary(fit))["(Intercept)","Std. Error"]
	bootstrapStdErr[i,"Months_since_Last_Donation"] = coef(summary(fit))["Months_since_Last_Donation","Std. Error"]
	bootstrapStdErr[i,"Number_of_Donations"] = coef(summary(fit))["Number_of_Donations","Std. Error"]
	bootstrapStdErr[i,"Months_since_First_Donation"] = coef(summary(fit))["Months_since_First_Donation","Std. Error"]

	# store all coefficient z values in dataframe
	bootstrapZValue[i,"Intercept"] = coef(summary(fit))["(Intercept)","z value"]
	bootstrapZValue[i,"Months_since_Last_Donation"] = coef(summary(fit))["Months_since_Last_Donation","z value"]
	bootstrapZValue[i,"Number_of_Donations"] = coef(summary(fit))["Number_of_Donations","z value"]
	bootstrapZValue[i,"Months_since_First_Donation"] = coef(summary(fit))["Months_since_First_Donation","z value"]

	# store all coefficient p values in dataframe
	bootstrapPValue[i,"Intercept"] = coef(summary(fit))["(Intercept)","Pr(>|z|)"]
	bootstrapPValue[i,"Months_since_Last_Donation"] = coef(summary(fit))["Months_since_Last_Donation","Pr(>|z|)"]
	bootstrapPValue[i,"Number_of_Donations"] = coef(summary(fit))["Number_of_Donations","Pr(>|z|)"]
	bootstrapPValue[i,"Months_since_First_Donation"] = coef(summary(fit))["Months_since_First_Donation","Pr(>|z|)"]

}

# Calculate coeffiicent estimates from bootstrap samples.
bootStrapIntercept = mean(bootstrapCoeff[,"Intercept"])
bootStrapMonths_since_Last_Donation = mean(bootstrapCoeff[,"Months_since_Last_Donation"])
bootStrapNumber_of_Donations = mean(bootstrapCoeff[,"Number_of_Donations"])
bootStrapMonths_since_First_Donation = mean(bootstrapCoeff[,"Months_since_First_Donation"])

# Now calculate the predicted probabilities for the test set.
for (j in 1:nrow(blood_don_set.test)) {
	 expValue = exp((bootStrapIntercept + (blood_don_set.test[j,"Months_since_Last_Donation"]*bootStrapMonths_since_Last_Donation) + (blood_don_set.test[j,"Number_of_Donations"]*bootStrapNumber_of_Donations) + (blood_don_set.test[j,"Months_since_First_Donation"]*bootStrapMonths_since_First_Donation))*(-1))
	 blood_don_set.test[j,"predictedProbabilities"] = 1/(1+expValue)

}

# calculate log loss for each case
blood_don_set.test$logLoss = ( blood_don_set.test$actual*log(blood_don_set.test$predictedProbabilities)) + ( (1-blood_don_set.test$actual)*log(1-blood_don_set.test$predictedProbabilities) )

# Average all the log losses.
logloss = mean(blood_don_set.test$logLoss)*(-1)
print(logloss)
#logloss = 0.5151235


# Calculate predicted probabilities on blind test set.
blood_don_test$predictedProbabilities = 0


for (k in 1:nrow(blood_don_test)) {
	 expValue = exp((bootStrapIntercept + (blood_don_test[k,"Months_since_Last_Donation"]*bootStrapMonths_since_Last_Donation) + (blood_don_test[k,"Number_of_Donations"]*bootStrapNumber_of_Donations) + (blood_don_test[k,"Months_since_First_Donation"]*bootStrapMonths_since_First_Donation))*(-1))
	 blood_don_test[k,"predictedProbabilities"] = 1/(1+expValue)

}

# prepare for submission# 11. 
my_prediction_11 = blood_don_test[,c("X","predictedProbabilities")] 
submission_format = read.csv("BloodDonationSubmissionFormat.csv", check.names=FALSE)
colnames(my_prediction_11) = colnames(submission_format)
write.csv(my_prediction_11, "my_prediction_11.csv", row.names=FALSE)
# Score: 0.4546


# Problem Statement: The goal of this project is to build a model to correctly identify  bad loans. 
# Data Source: Data for this project has been taken from the publibly available dataset at https://www.lendingclub.com/info/download-data.action.
# The 2007-2011 loan data has been used for analysis.
# Approach: After cleaning and preparing the data for analysis, the dataset will be divided in to training set and test set. 
# First, a logistic regression model will be built using the training data set.
# Questions / doubts: 1. Loan term is either 36 months or 60 months. Should we treat this as factor variable or numeric variable?
# 2. 

library(stringr)
library(lubridate)
library(car)
library(corrplot)
library(gmodels)
#library(Hmisc)

# Step 1: Get the dataset. Load it in to R.
loanData = read.csv("LoanStats3a.csv",na.strings = c("","NA"), header=TRUE, stringsAsFactors=FALSE, skip=1)


# Step 2: Explore the dataset. 
# Take a look at the first few records in the dataset. 
head(loanData)

# Remove the last 2 rows from the dataframe as they are useless. 
loanData = loanData[1:(nrow(loanData)-2),]

summary(loanData)



# Step 3a: Clean the data. Deal with missing values, incorrect data types, etc

# Remove the first 2 columns (id and member_id)
loanData.clean = loanData[,3:ncol(loanData)]

summary(loanData.clean)

# Remove the columns url , emp_title, desc, title, zipcode, 
loanData.clean$url <- NULL
loanData.clean$emp_title <- NULL
loanData.clean$desc <- NULL
loanData.clean$title <- NULL
loanData.clean$zip_code <- NULL

# Remove the mths_since_last_record,mths_since_last_major_derog,mths_since_last_delinq columns since they are mostly empty.
loanData.clean$mths_since_last_record = NULL
loanData.clean$mths_since_last_major_derog = NULL
loanData.clean$mths_since_last_delinq = NULL


#Remove the policy_code column since it has value 1 for all rows.
loanData.clean$policy_code = NULL

#Remove the initial_list_status column since it has value f for all rows.
loanData.clean$initial_list_status = NULL

#Remove the collections_12_mths_ex_med column since it has value 0 for all rows.
loanData.clean$collections_12_mths_ex_med = NULL 

#Remove purpose column from the dataframe.
loanData.clean$purpose = NULL

# Remove all date fields (issue_d, earliest_cr_line, last_pymnt_d, nxt_pymnt_d, last_credit_pull_d)
loanData.clean$issue_d = NULL
loanData.clean$earliest_cr_line = NULL
loanData.clean$last_pymnt_d = NULL
loanData.clean$next_pymnt_d = NULL
loanData.clean$last_credit_pull_d = NULL

loanData.clean = na.omit(loanData.clean)  # to remove all NAs from from the dataset.


# See whether the following columns have unique values in the dataset. Then you can convert them to categorical variables.
unique(loanData.clean[,"grade"])
unique(loanData.clean[,"sub_grade"])
unique(loanData.clean[,"is_inc_v"])
unique(loanData.clean[,"pymnt_plan"])
unique(loanData.clean[,"loan_status"])

#There are some cases where the loan_status column has ambiguous data like "Does not meet the credit policy.  Status:Current".
# Since it is not clear what these statuses mean, I am going to remove all records with such loan_statuses.
 loanData.clean = loanData.clean[-grep("Does not meet the credit policy",loanData.clean$loan_status),]

 unique(loanData.clean[,"home_ownership"]) # we see that there are cases with home_ownership as "NONE". 
 loanData.clean[loanData.clean$home_ownership=="NONE",]
 # There are 3 cases where the home_ownerhsip is NONE. We will impute this missing value as "RENT".
 loanData.clean[loanData.clean$home_ownership=="NONE","home_ownership"] = "RENT"

unique(loanData.clean[,"emp_length"]) # we see that there are cases with emp_length as "n/a".
nrow(loanData.clean[loanData.clean$emp_length=="n/a",])
# There are 1075 cases with n/a in this field. We will remove these caaes from our dataset.
loanData.clean = loanData.clean[!(loanData.clean$emp_length=="n/a"),]

 

# convert grade, sub-grade, home_ownership,pymnt_plan columns to factors
loanData.clean$grade <- as.factor(loanData.clean$grade)
loanData.clean$sub_grade <- as.factor(loanData.clean$sub_grade)
loanData.clean$home_ownership <- as.factor(loanData.clean$home_ownership)
loanData.clean$is_inc_v <- as.factor(loanData.clean$is_inc_v)
loanData.clean$pymnt_plan <- as.factor(loanData.clean$pymnt_plan)

#convert term,emp_length,addr_state to categorical variable.
loanData.clean$term = as.factor(loanData.clean$term)
loanData.clean$emp_length = as.factor(loanData.clean$emp_length)
loanData.clean$emp_length = relevel(loanData.clean$emp_length,"< 1 year")
loanData.clean$addr_state = as.factor(loanData.clean$addr_state)

summary(loanData.clean)


# The following function will gives the rownumber and the number of missing values in that row.
getMissingValues = function(x){
	incompletecaseNumbers = which(!complete.cases(x)) 
	incompletecaseNumbers =as.vector(incompletecaseNumbers)
	incompletecases = x[incompletecaseNumbers,]
	missingValuesCount = apply(  incompletecases,1,function(y) sum(is.na(y))   )
	missingValuesCount = as.vector(missingValuesCount)
	incompleteCaseCountdf =  data.frame(incompletecaseNumbers,missingValuesCount)
	incompleteCaseCountdfSorted =  incompleteCaseCountdf[order(-missingValuesCount),]
	return (incompleteCaseCountdfSorted) 
}

missingVal = getMissingValues(loanData.clean)

loanData.clean = loanData.clean[-c(missingVal$incompletecaseNumbers),]
# Remove all rows with missing data.



 #Step 3b: Prepare the data. 

 #Since I am doing binary classification, we will transform the different loan_status values in to Good and Bad.
 # I am treating loan with status such as Late, Default and Charged off as bad loans. 
 # Loans with statuses such as Current, Fully Paid and In Grace Period will be treated as Good Loans.
 bad_indicators = c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")
 loanData.clean$is_bad = ifelse(loanData.clean$loan_status %in% bad_indicators,1,0)
 loanData.clean$is_bad = as.factor(loanData.clean$is_bad)
 loanData.clean$is_bad = factor(loanData.clean$is_bad, levels = c(0, 1), labels = c("Good", "Bad"))
 loanData.clean$loan_status = NULL 

loanData.clean$revol_util <- str_replace_all(loanData.clean$revol_util, "[%]", "") 
# Get rid of  the "%" symbol in the revol_util column.

loanData.clean$revol_util <- as.numeric(loanData.clean$revol_util)
# Convert the string data type of revol_util to numeric type.

loanData.clean$int_rate <- str_replace_all(loanData.clean$int_rate, "[%]", "")
loanData.clean$int_rate <- as.numeric(loanData.clean$int_rate) 


# STEP 3B: Plot the data. Check for linearity, multicollinearity, etc 

# Find the correlation between all continuous predictor variables.
corrMatrix1 = cor(loanData.clean[,c("loan_amnt","funded_amnt","funded_amnt_inv","int_rate","installment","annual_inc","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util",
"total_acc","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries", "collection_recovery_fee","last_pymnt_amnt" )])

# To visualize the correlation matrix better:
corrplot(corrMatrix1,method = "circle",title="Correlation Matrix For All Continuous Predictors",tl.cex=0.5)

# From the correlation matrix, we see that loan_amnt, funded_amnt, funded_amnt_inv are highly correlated with each other. 
# Similarly, total_pymnt, total_pymnt_inv and total_rec_prncp are highly correlated with each other. out_prncp and out_prncp_inv are also highly correlated.
# But, how do we deal with multicollinearity problem? 




# Remove funded_amnt, funded_amnt_inv, total_pymnt_inv  total_rec_prncp and out_prncp_inv and generate correlation matrix again. 
corrMatrix2 = cor(loanData.clean[,c("loan_amnt","int_rate","installment","annual_inc","dti","delinq_2yrs","inq_last_6mths","open_acc","pub_rec","revol_bal","revol_util",
"total_acc","out_prncp","total_pymnt","total_rec_int","total_rec_late_fee","recoveries", "collection_recovery_fee","last_pymnt_amnt" )])

# To visualize the correlation matrix better:
corrplot(corrMatrix2,method = "circle",title="Correlation Matrix For Loan Default Predictors",tl.cex=0.5)

# Now, how do I deal with correlated varoa
# Remove the highly correlated variables from the dataset.
#loanData.clean$funded_amnt = NULL
#loanData.clean$funded_amnt_inv = NULL
#loanData.clean$total_pymnt_inv = NULL
#loanData.clean$total_rec_prncp = NULL
#loanData.clean$out_prncp_inv = NULL



# Step 3C: Prepare training dataset and test dataset. 
# We have 39,730 records. Let us use 30,000 records for training and the remaining for testing. 
# We will randomly pick the cases for training set.
set.seed(1234)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(loanData.clean)),size=30000,replace=F)
loanData.trng = loanData.clean[trng_ind,]
loanData.test = loanData.clean[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
 prop.table(table(loanData.trng$is_bad))
 prop.table(table(loanData.test$is_bad))

# STEP 4: Build the model.
loanData.logModel.1 = glm(is_bad~.,data=loanData.trng,family=binomial(),control = list(maxit = 50))
# output: 
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
summary(loanData.logModel.1)
# Residual deviance = 1258.6, AIC = 1510.6

#Calculate model chi square statistic:
modelChi.logModel.1 = loanData.logModel.1$null.deviance - loanData.logModel.1$deviance
chidf.logModel.1 = loanData.logModel.1$df.null - loanData.logModel.1$df.residual
chisq.prob.logModel.1 = 1-pchisq(modelChi.logModel.1,chidf.logModel.1)
modelChi.logModel.1 ; chidf.logModel.1; chisq.prob.logModel.1;
#[1] 22335.51
#[1] 125
#[1] 0


# To get rid of the above warning message, Manually perform stepwise forward selection and note the deviances.
# We find that the algorithm does not converge if we include annual_inc, total_pymnt, recoveries, collection_recovery_fee, last_pymnt_amnt in the
# set of predictors. We run the model after remving these predictors.
#loanData.trng$annual_inc = loanData.trng$total_pymnt = loanData.trng$recoveries = loanData.trng$collection_recovery_fee = loanData.trng$last_pymnt_amnt = NULL
loanData.logModel.2 = glm(is_bad~loan_amnt+funded_amnt+funded_amnt_inv+term+int_rate+installment+grade+sub_grade+emp_length+home_ownership+is_inc_v+pymnt_plan+addr_state+dti+delinq_2yrs+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+out_prncp+out_prncp_inv+total_pymnt_inv+total_rec_prncp+total_rec_int+total_rec_late_fee,data=loanData.trng,family=binomial())
summary(loanData.logModel.2)
# Residual deviance = 1313.7, AIC = 1555.7

#Calculate model chi square statistic:
modelChi.logModel.2 = loanData.logModel.2$null.deviance - loanData.logModel.2$deviance
chidf.logModel.2 = loanData.logModel.2$df.null - loanData.logModel.2$df.residual
chisq.prob.logModel.2 = 1-pchisq(modelChi.logModel.2,chidf.logModel.2)



#Now compare model1 and model2.
modelChi.1and2 = loanData.logModel.2$deviance - loanData.logModel.1$deviance
chidf.1and2 = loanData.logModel.2$df.residual - loanData.logModel.1$df.residual
chisq.prob.1and2 = 1-pchisq(modelChi.1and2,chidf.1and2)
modelChi.1and2;chidf.1and2;chisq.prob.1and2
#[1] 55.11286
#[1] 5
#[1] 1.237404e-10
# Since the p-value is 1, we cannot consider model 1 as a significant improvement over model 2.



#Check for multicollinearity
vif(loanData.logModel.1)
# Output:
#Error in vif.default(loanData.logModel) : there are aliased coefficients in the model


# This means there is perfect multicollinearity in the data.
alias(loanData.logModel.1)

# How to interpret the output of this command?


# Next, I want to make use of R's step() function to perform forward selection. I am doing this just out of curiosity! 
#nullModel = glm(is_bad~1,data=loanData.trng,family=binomial())
#fullModel = glm(is_bad~loan_amnt+term+int_rate+installment+grade+sub_grade+emp_length+home_ownership+is_inc_v+pymnt_plan+addr_state+dti+delinq_2yrs+inq_last_6mths+open_acc+pub_rec+revol_bal+revol_util+total_acc+out_prncp+total_rec_int+total_rec_late_fee,data=loanData.trng,family=binomial())
#step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")



# Now, test the model by predicting the output on the test set. 
loanData.test$predictedProbabilities <- predict(loanData.logModel.1, newdata = loanData.test, type = "response")
loanData.test$predictedProbabilities <- predict(loanData.logModel.1, newdata = loanData.test[,-which(names(loanData.test) == "is_bad")] , type = "response")
#Warning message:
#In predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type ==  : prediction from a rank-deficient fit may be misleading
	

# If the predicted probability is >= 0.5, classify it as bad loan, else good loan.
loanData.test$prediction = ifelse(loanData.test$predictedProbabilities>=0.5,1,0)
loanData.test$prediction = factor(loanData.test$prediction, levels = c(0, 1), labels = c("Good", "Bad"))

#Create a confusion matrix.
CrossTable(loanData.test$is_bad, loanData.test$prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))

#Total Observations in Table:  8661 

 
#             | predicted 
#      actual |      Good |       Bad | Row Total | 
#-------------|-----------|-----------|-----------|
#        Good |      7507 |         2 |      7509 | 
#             |     0.867 |     0.000 |           | 
#-------------|-----------|-----------|-----------|
#         Bad |        39 |      1113 |      1152 | 
#             |     0.005 |     0.129 |           | 
#-------------|-----------|-----------|-----------|
#Column Total |      7546 |      1115 |      8661 | 
#-------------|-----------|-----------|-----------|

# We see that over all our model has correctly identified almost all the good loan (7507/7509).
# For the bad loans, the accuracy is 1113/1152 = 96.61%












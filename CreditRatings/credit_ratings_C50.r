
# The goal of this project is to identify factors that make an loan applicant at higher risk of default using C5.0 decision tree algorithm.
# We will use the data on a large number of past bank loans and whether the loan went into default, as well as information about the applicant.
# The data set has been taken from the UCI Machine Learning Data Repository, http://archive.ics.uci.edu/ml. 
# It represents loans obtained from a credit agency in Germany. 



# Step1: Collecting the data. Download the dataset from the link provided above. 
# The credit dataset includes 1,000 examples of loans, plus a combination of numeric and nominal features indicating characteristics of the loan 
# and the loan applicant. A class variable indicates whether the loan went into default.

# Step 2: exploring and preparing the data.
credit = read.csv("credit.csv")
head(credit_rand)
str(credit)
credit$default = factor(credit$default)
credit$default = factor(credit$default, levels = c(1, 2), labels = c("No", "Yes"))

#Create training sets and test sets. We will use 90% of data for training and 10% of data for testing. These record will be randomly chosen.
# we will use random number generation to randomly create training and test set data. 

set.seed(12345)
credit_rand = credit[order(runif(1000)), ]


# Check whether we have created a random data set correctly. The results of the first 2 commands below should be identical.
 summary(credit$amount)
 summary(credit_rand$amount)

 head(credit$amount)
 head(credit_rand$amount)

 # Now, create training and test data.
 credit_train = credit_rand[1:900, ]
 credit_test  = credit_rand[901:1000, ]

 #check the proportion of loan defaults in both cases.
 prop.table(table(credit_train$default))
 prop.table(table(credit_test$default))

# Step3: Training a model on the data.
# C5.0 decisoion tree algorithm will be used to model the data. 
# signature of the C5.0(): C5.0(train, class, trial=1,costs=NULL)
library(C50)
credit_model = C5.0(credit_train[,1:20], credit_train$default)
credit_model
summary(credit_model)

#Step4: Evaluating model performance.
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred, prop.chisq=FALSE, prop.c=FALSE, dnn=c('actual default','predicted default'))
# Results:
#Cell Contents
#|-------------------------|
#|                       N |
#|           N / Row Total |
#|         N / Table Total |
#|-------------------------|

 
#Total Observations in Table:  100 

 
#              | predicted default 
#actual default |        No |       Yes | Row Total | 
#---------------|-----------|-----------|-----------|
#            No |        54 |        14 |        68 | 
#               |     0.794 |     0.206 |     0.680 | 
#               |     0.540 |     0.140 |           | 
#---------------|-----------|-----------|-----------|
#           Yes |        11 |        21 |        32 | 
#               |     0.344 |     0.656 |     0.320 | 
#               |     0.110 |     0.210 |           | 
#---------------|-----------|-----------|-----------|
# Column Total |        65 |        35 |       100 | 
#---------------|-----------|-----------|-----------|

# We can see that the model predicted 73 records correctly out of a total 100. i.e. the model has an overall accuracy rate of 73% and an error rate of 23%.
# But out of 32 defaults, our model has predicted only 21 correctly. i.e. the accuracy for defaulters is only 66%. Let us see if we can improve this accuracy. 



# Step5: Improving model performance.
# add boosting to the C5.0 decision tree. add trials parameter and set the value to 10. This parameter sets an upperlimit and the algorithm will stop adding 
# trees if it recognizes that additional trials do not seem to be improving the accuracy.
credit_boost10 = C5.0(credit_train[,1:20], credit_train$default, trials=10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 = predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
# Results:
#Cell Contents
#|-------------------------|
#|                       N |
#|         N / Table Total |
#|-------------------------|

 
#Total Observations in Table:  100 

 
#               | predicted default 
#actual default |        No |       Yes | Row Total | 
#---------------|-----------|-----------|-----------|
#            No |        63 |         5 |        68 | 
#               |     0.630 |     0.050 |           | 
#---------------|-----------|-----------|-----------|
#           Yes |        16 |        16 |        32 | 
#               |     0.160 |     0.160 |           | 
#---------------|-----------|-----------|-----------|
#  Column Total |        79 |        21 |       100 | 
#---------------|-----------|-----------|-----------|

# We can see that the model predicted 79 records correctly out of a total 100. 
# i.e. the model has an overall accuracy rate of 79% which is an improvement over the previous 73%.
# But out of 32 defaults, our model has predicted only 16 correctly. i.e. the accuracy for defaulters is only 50%, which is worse than what we got before.

# In case of loan applicants, giving loan to an applicant who is likely to default is a costly mistake. i.e. False negatives are more costly than False positives, 
# which in turn are more costly than true positives and true negatives. 
# One way to reduce false negatives is to reject a large number of border line loan applicants. 
# The few years' worth of interest that the bank would earn from a risky loan is far outweighed by the massive loss it would take if the money 
# was never paid back at all.
# We can make the C5.0 algorithm assign penalties to false negatives and false positives to discourage the decision tree from making more costly mistakes. 

error_cost = matrix(c(0, 1, 4, 0), nrow = 2)
# False negative has a cost of 4, where as a false positive has a cost of 1.
credit_cost = C5.0(credit_train[,1:20], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# Results:
#   Cell Contents
#|-------------------------|
#|                       N |
#|         N / Table Total |
#|-------------------------|

 
#Total Observations in Table:  100 

 
#               | predicted default 
#actual default |        No |       Yes | Row Total | 
#---------------|-----------|-----------|-----------|
#            No |        38 |        30 |        68 | 
#               |     0.380 |     0.300 |           | 
#---------------|-----------|-----------|-----------|
#           Yes |         5 |        27 |        32 | 
#              |     0.050 |     0.270 |           | 
#---------------|-----------|-----------|-----------|
#  Column Total |        43 |        57 |       100 | 
#---------------|-----------|-----------|-----------|

# Compared to the previous results, the overall accuracy has come down to 65%. But the total number of false negatives has gone down to 15%, compared to 50% in the 
# previous run. 















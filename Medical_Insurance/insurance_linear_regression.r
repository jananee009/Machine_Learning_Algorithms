# Goal: The goal of this analysis is to use patient data to estimate the average medical care expenses for  population segments. 
# Overview: In order for an insurance company to make money, it needs to collect more in yearly premiums than it spends on medical care to its beneficiaries. 
# Hence, models that accurately forecast medical expenses are highly valued by insurers.
# Medical expenses are difficult to estimate because the most costly conditions are rare and seemingly random. 
# Still, some conditions are more prevalent for certain segments of the population. 
# For instance, lung cancer is more likely among smokers than non-smokers, and heart disease may be more likely among the obese.
# These estimates could be used to create actuarial tables which set the price of yearly premiums higher or lower depending on the expected treatment costs.

# Step 1: Collecting the data. Use the insurance.csv file. It contains 1,338 examples of beneficiaries currently enrolled in the insurance plan, 
# with features indicating characteristics of the patient as well as the total medical expenses charged to the plan for the calendar year.
#• age: This is an integer indicating the age of the primary beneficiary (excluding those above 64 years, since they are generally covered by the government).
#• sex: This is the policy holder's gender, either male or female.
#• bmi: This is the body mass index (BMI), which provides a sense of how over or under-weight a person is relative to their height. BMI is equal to weight (in kilograms) divided by height (in meters) squared. An ideal BMI is within the range of 18.5 to 24.9.
#• children: This is an integer indicating the number of children / dependents covered by the insurance plan.
#• smoker: This is yes or no depending on whether the insured regularly smokes tobacco.
#• region: This is the beneficiary's place of residence in the U.S., divided into four geographic regions: northeast, southeast, southwest, or northwest.

# Step 2: Exploring and preparing the data. 
insurance = read.csv("insurance.csv",stringsAsFactors=TRUE)
str(insurance)
summary(insurance$charges)
# mean = 13270, median = 9382
# Since the mean > median, the distribution of insurances is right skewed.

hist(insurance$charges)
# From the histogram, we can see that the majority of the people in the data have yearly medical expenses between $0 and $15,000.
# Since linear regression assumes the distribution of the dependent variable as normal, this distribution is not ideal.

table(insurance$region)

# before training the regression model on the data, see the correlation between the independent variables.
cor(insurance[c("age","bmi","children","charges")])

#Visualize relationship among features.
pairs(insurance[c("age", "bmi", "children", "charges")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# Step 3: Training the model.
ins_model = lm(charges ~ ., data = insurance)
ins_model

# Step4: Evaluating model performance.
summary(ins_model)
# Adjusted R-squared:  0.7494 

# Step 5: Improving model performance.
# Regression typically leaves feature selection and model specification to the user. 
# Consequently, if we have subject matter knowledge about how a feature is related to the outcome, 
# we can use this information to inform the model specification and potentially improve the model's performance.
# For e.g. the effect of age on health expenses may not be consistent across all ages. i.e. age may not be linearly related to charges. 
# Add the nonlinear age to the model.
insurance$age2 <- insurance$age^2

# Conside the impact of BMI. If a person has normal weight(BMI), it may have zero impact on medical expenses. 
# But, it may be strongly related to expenses for the obese.
# Assume that the normal BMI is 30 or less. Anything above 30 is risky. 
# We model this relationship by creating a binary indicator variable that is 1 if the BMI is at least 30 and 0 otherwise.
# The estimated beta for this binary feature would then indicate the average net impact on medical expenses for individuals 
# with BMI of 30 or above, relative to those with BMI less than 30.
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# Consider interaction effect on medical expenses. Smoking and obesity may be harmful separately. 
# But, we can say that the combined effects of both smoking and obesity may be more harmful than the sum of each one alone.
charges ~ bmi30*smoker

# Incorporate the above 3 relationships in to the model.
ins_model2 = lm(charges ~ age+age2+children+bmi+sex+bmi30*smoker+region, data=insurance)
ins_model2
summary(ins_model2)
# Adjusted R-squared:  0.8653 
# We can see that the R squared value has increased to 87% from 75% in the previous model. 
# i.e. 87% of the variance in the dependent variable is explained by the model.
# Also, all the new variables that we added to the model are statistically significant. 







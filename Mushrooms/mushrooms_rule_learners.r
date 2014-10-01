# Goal: This is a classification project. The goal is, based on the given data, identify which mushrooms are poisonous and which are edible, based on the features. 

# Step1: Collect data. Gather information about different species of wild mushrooms along with labels that identify them as "edible" or "poisonous" in nature.  
# The information about different species of mushrooms and their nature is in the CSV file "mushroom.csv". This csv file contains 8,214 samples of mushroom data. 
# In this project, we will assume that this data set is an exhaustive set of all possible wild mushrooms. 
# Hence we will not create a separate training set and test set.  We will try to find rules that accurately classify a mushroom as poisonous or edible.
# So, we will build the model and test it on the same data.

#Step2: Explore and prepare the data.
# Load the data from the csv file in to R.
mushrooms = read.csv("mushroom.csv",header=TRUE, stringsAsFactors=TRUE)

# remove the first column since it contains serial number denoting the observation number. 
mushrooms = mushrooms[-1]

#The dependent variable is in the first column names "EorP". Change the name of this column to "type".
colnames(mushrooms)[1] = "type" 
mushrooms$type = factor(mushrooms$type, levels = c("e", "p"), labels = c("edible", "poisonous"))

str(mushrooms)
# The output shows that the feature veilType is a Factor with only 1 level. It means all the observations in the dataset have level "p". 
# Since this feature is not giving any additional information, we will drop it. 
mushrooms$veil_type <- NULL
table(mushrooms$type)
# edible 4208, poisonous 3916

#Step 3: Training a model on the data.
# We will start with the one rule 1R implementation available in the RWeka package.
library(RWeka)
mushroom_1R = OneR(type~.,data=mushrooms)

#examine the rule created:
mushroom_1R

# Step4: Evaluating model performance.
summary(mushroom_1R)

#=== Confusion Matrix ===
#    Actual
#    a    b   <-- classified as
# 4208    0 |    a = edible
#  120 3796 |    b = poisonous

# Step 5: Improving model performance.
# For a more sophisticated rule learner, we will use JRip(), a Java-based implementation of the RIPPER rule learning algorithm. 
mushroom_JRip = JRip(type~., data=mushrooms) 

#examine the rules created:
mushroom_JRip

#=== Confusion Matrix ===
#    Actual
#    a    b   <-- classified as
# 4208    0 |    a = edible
#    0 3916 |    b = poisonous

# From the above matrix, we can see that the number of False Positives and False negatives is 0. The prediction accuracy is 100%.

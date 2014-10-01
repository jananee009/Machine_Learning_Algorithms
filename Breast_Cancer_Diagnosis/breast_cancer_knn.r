# Goal: To classify cancerous cells as malignant or benign by applying KNN algorithm to biposied cells from women with abnormal breast masses.
# The "Breast Cancer Wisconsin Diagnostic" dataset from the UCI Machine Learning Repository, which is available at http://archive.ics.uci.edu/ml, 
# has been used for analysis in this problem.
# The breast cancer data includes 569 examples of cancer biopsies, each with 32 features. 
# One feature is an identification number, another is the cancer diagnosis, and 30 are numeric-valued laboratory measurements. 
# The diagnosis is coded as M to indicate malignant or B to indicate benign.
# The 30 numeric measurements comprise the mean, standard error, and worst (that is, largest) value for 10 different characteristics of the digitized cell nuclei. These include:
#• Radius
#• Texture
#• Perimeter
#• Area
#• Smoothness
#• Compactness
#• Concavity
#• Concave points
#• Symmetry
#• Fractal dimension
# Based on their names, all of the features seem to relate to the shape and size of the cell nuclei.


# Step1 : Download the dataset from the link provided above. 569 examples of cancer biopsies,each with 32 features is available in "wisc_bc_data.csv"  file.  

# Step2: Exploring and preparing the data. Read the data in to R from csv file.
wbcd_original = read.csv("wisc_bc_data.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
str(wbcd_original)

#the first column in the dataset is  id . Drop the id feature.
wbcd =  wbcd_original[-1]

# how many Benign and Malignant cases are there altogether in the dataset.
table(wbcd$diagnosis)
# B 357   M 212

# convert the diagnosis column values in to factor and change the labels.
wbcd$diagnosis = factor(wbcd$diagnosis, levels=c("B","M"),labels = c("Benign","Malignant"))

# how many Benign and Malignant cases are there altogether in the dataset, expressed as %ages. 
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd)
# we can see that some features have values ranging between 0 and 1, while others have values ranging from 5 to 500.
# Since we are using KNN, we need to rescale the values to a standard range of values.
# define a function to normalize a set of values.
normalize = function(x) 
{ 
	return ((x-min(x)) / (max(x) - min(x)))
}	

# apply the normalize function to all the features that are numeric in nature. 
wbcd_n = as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n)

# Use the first 469 records for training the classifier and the last 100 for testing. 
wbcd_train = wbcd_n[1:469,]
wbcd_test = wbcd_n[470:569,]

# storing the class labels in a separate vector.
wbcd_train_labels = wbcd[1:469,1]
wbcd_test_labels = wbcd[470:569,1]

# Step 3: Training the model on the data. use the implementation of the knn algorithm available in the class package.
library(class)
# call the knn function. It returns a vector of predicted variables for each example in the test set. 
# choose a value of k. As our training data includes 469 instances, we pick k = 21, an odd number roughly equal to the square root of 469.
wbcd_test_pred = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=21)

# Step 4: Evaluating model performance. To evaluate the performance of the algorithm, generate a confusion matrix.
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

# Step 5: Improving model performance:
# Approach 1: To standardize the values of all features in the input data, use z scores.
wbcd_z = as.data.frame(scale(wbcd[-1]))
#check if z scores have been computed correctly. All the features must have mean as 0.
summary(wbcd_z)

# create training set and test set data, run the algorithm and create the confusion matrix:
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE
# In this approach, the accuracy declined as compared to our original solution. Hence we try another approach.

# Aproach 2: Use different values of K.
# Use the normalized data set and setting as k = 1,5,11,15,21,27. Run the algorithm for the different values of K and create the confusion matrix. 
# Get the number of false positives and false negatives for each value of k and compare to decide value of k gives the highest accuracy.
wbcd_test_pred_1 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_1, prop.chisq=FALSE)
wbcd_test_pred_5 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_5, prop.chisq=FALSE)
wbcd_test_pred_11 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_11, prop.chisq=FALSE)
wbcd_test_pred_15 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_15, prop.chisq=FALSE)
wbcd_test_pred_21 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_21, prop.chisq=FALSE)
wbcd_test_pred_27 = knn(train=wbcd_train, test=wbcd_test,cl=wbcd_train_labels,k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_27, prop.chisq=FALSE)

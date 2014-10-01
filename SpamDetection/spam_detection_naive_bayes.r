# Goal: The goal of this project is to filter mobile phone spam with naive bayes algorithm.

# Step1: Collecting the data. For analysis, we will use data from the SMS Spam Collection at http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/.
# The dataset for this problem is a csv file containing a list of 5500+ text messages. Each text message has been classified as ham or spam.

# Step 2: Exploring and preparing the data. REad the data from the csv file  and store it in to a dataframe in R.
sms_raw = read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
# from the output of above command, we can see that the csv file has 2 columns, "type" and "text". R is treating the type variable as a character vector. 
# But, we want type to be a factor variable. 
 sms_raw$type = factor(sms_raw$type)
 table(sms_raw$type)
 # output
 # ham   spam
 # 4812   747

 # Use textmining package called "tm" to analyze the text messages.
 library(tm)

 #build a corpus i.e. a collection of text documents. 1 text document = 1 sms
 sms_corpus = Corpus(VectorSource(sms_raw$text))
 # this corpus contains 5559 text documents. 

 print(sms_corpus)
 
 # view the first 3 sms messages.
 inspect(sms_corpus[1:3])

 # Clean the corpus. i.e. convert all upper case letters to lower case, remove numbers, remove punctuations, etc
# convert all upper case letters to lower case.
 corpus_clean = tm_map(sms_corpus, tolower)
# remove all numbers
 corpus_clean = tm_map(corpus_clean, removeNumbers)
# remove all stop words
 corpus_clean = tm_map(corpus_clean, removeWords, stopwords())
# remove punctuation
 corpus_clean = tm_map(corpus_clean, removePunctuation)
 # remove all extra white spaces around the words
 corpus_clean = tm_map(corpus_clean, stripWhitespace)

 # split each text message in to individual components (words) called as tokens and create a sparse matrix.
 sms_dtm <- DocumentTermMatrix(corpus_clean)

 # split the dataset in to training and testing.
 # raw data
 sms_raw_train = sms_raw[1:4169,]
 sms_raw_test = sms_raw[4170:5559,]

 # dtm data
 sms_dtm_train = sms_dtm[1:4169, ]
 sms_dtm_test  = sms_dtm[4170:5559, ]

 # corpus data
 sms_corpus_train = corpus_clean[1:4169]
 sms_corpus_test  = corpus_clean[4170:5559]

 #check whether the training and test data set are representative of the complete set of data.
 prop.table(table(sms_raw_train$type))
 prop.table(table(sms_raw_test$type))

 #visualize the frequency of words appearing in the data.
 library(wordcloud)
 wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)
 # since random.order = FALSE, the cloud will be arranged in non-random order, with the higher-frequency words placed closer to the center. 
 # If we do not specify random.order, the cloud would be arranged randomly by default.
 # A general rule is to begin by setting min.freq to a number roughly 10 percent of the number of documents in the corpus; 
 # In this case 10 percent is about 40. Therefore, words in the cloud must appear in at least 40 SMS messages.

 # visualize the frequency of the words appearing in spam messages and ham messages separately.
 spam = subset(sms_raw_train, type == "spam")
 ham = subset(sms_raw_train, type == "ham")
 wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
 wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

 # create a vector of frequently occurring words. 
 sms_dict = findFreqTerms(sms_dtm_train, 5)
 # command will display a character vector of the words appearing at least 5 times in the sms_dtm_train matrix:
 
 sms_train = DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
 sms_test  = DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

 convert_counts <- function(x) {
    x <- ifelse(x > 0, 1, 0)
    x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
    return(x)
￼}

sms_train = apply(sms_train, MARGIN = 2, convert_counts)
sms_test  = apply(sms_test, MARGIN = 2, convert_counts)


# Step 3: Train the model on the data.
# Install and load the "e1071" package that contains the naive bayes implementation.
install.packages("e1071")
library(e1071)

# To build our model on the sms_train matrix, we'll use the following command:
sms_classifier = naiveBayes(sms_train, sms_raw_train$type)

# Step4: Evaluating model performance.
sms_test_pred = predict(sms_classifier, sms_test)
CrossTable(sms_raw_test$type, sms_test_pred, prop.chisq = FALSE, prop.t = FALSE,dnn = c('actual', 'predicted'))

# Step 5: Improving model performance:
# Use laplace = 1.
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_raw_test$type, sms_test_pred2, prop.chisq = FALSE, prop.t = FALSE,dnn = c('actual', 'predicted'))














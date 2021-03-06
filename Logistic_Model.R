# Logistic regression

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[,3:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])
#Fitting logistic regression into training dataset

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)
#Predict test results
#response type gives probabilities listed in a single vector
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred >0.5, 1, 0)

#confusion matrix to count number of correct and incorrect predictions
cm = table(test_set[, 3], y_pred)

#Visualizing training set results
install.packages('ElemStatLearn')
library(ElemStatLearn)


############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#    Linear kernel
#    RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a digit from 0-9
#The dataset contain description about 28×28 pixel image which would represent 784 variables
#according to business understanding the pixel values would be between 0 to 255 and target variable would be 0-9 digit

#####################################################################################

# 2. Data Understanding: 

# Number of Instances: 59.999
# Number of Attributes: 785 

#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Loading Data

Data <- read_csv("mnist_train.csv")

#Data is quite huge hence taking 5% of the data as sample and creating the training data set for further building the model.
set.seed(1)
trainindices= sample(1:nrow(Data), 0.05*nrow(Data))
train =Data[trainindices,]


#Understanding Dimensions

dim(train)

#Structure of the dataset

str(train)

#printing first few rows

head(train)

#Exploring the data

summary(train)



#checking missing value

sapply(train, function(x) sum(is.na(x)))
#no missing values present in data

#Renaming the  target column as digit and converting the same to factor.
colnames (train)[1]<- "digit"
train$digit<-factor(train$digit)

#Checking the summary of digit column to validate the sample contains all the digits.
summary(train$digit)


#Similarly preparing the test data from the source as done for training data.

#test_Data <- read_csv("mnist_test.csv")
test <- read_csv("mnist_test.csv")



#Renaming the  target column as digit and converting the same to factor.
colnames (test)[1]<- "digit"
test$digit<-factor(test$digit)

#Renaming the column of testing dataset same as training data set as allaising has to be same for model evalution.
colnames(test)<-colnames(train)



#Removing the columns which are having more then 95% 0 or near value as they won't be
#important in preparation of the model.

#combining the dataset to remove columns as both datasets needs to considered for removal of zero variance columns
total <- rbind(train, test)
totalind = nearZeroVar(total, freqCut = 95/5, uniqueCut = 10)

#Removing the columns having zero vairance.
train =train[,-totalind]
test =test[,-totalind]

#Vaildating the structures of dataset after removal of columns
str(test)
str(train)


#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)
# accuracy 88%

#Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)
# accuracy 94%

#We can conclude that the accuracy of using RBF kernel is more, hence the model
#is complex and we will use RBF kernel for model evalution.


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.01, 0.02), .C=seq(2, 5, by=1))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

#sigma= 0.01
#C=2,3,4,5

plot(fit.svm)

#We get the model is more accurate with sigma as 0.01 but its same for
#all the values of C

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm, test)
confusionMatrix(evaluate_non_linear, test$digit)

# Accuracy    - 0.9471 hence it is not overfitting the accuracy value is similar to what is expected


######################################################################


#Building the model again using RBF Kernel with proper value for C=2 and sigma as 0.01
Model_RBF2 <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot", C = 2, sigma=0.01)
Eval_RBF2<- predict(Model_RBF2, test)

#confusion matrix - RBF Kernel for C=2 and sigma as 0.01
confusionMatrix(Eval_RBF2,test$digit)

# accuracy 94.4%



#Building the model again using RBF Kernel with proper value for C=5 and sigma as 0.01
Model_RBF5 <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot", C = 5, sigma=0.01)
Eval_RBF5<- predict(Model_RBF5, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF5,test$digit)

# accuracy 94.6%


#We can conclude that it will be built by RBF model and the accuracy of the model will be 94.6% with the optimum value of C as 5
#and value of sigma as 0.01.

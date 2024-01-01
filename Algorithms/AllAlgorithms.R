# Author: Ali Eren Kayhan - Emre Yildirim
# Date: 2023
# Title: Heart Attack Prediction Machine Learning Project - KNN/Decision Tree/Naive Bayes

##### Reading data from a CSV file into the 'dataset' variable
file_path <- file.path("Data", "HeartAttack.csv")
dataset <- read.table(file_path, header = TRUE, sep = ",")

##### DATA PRE-PROCESSING

# Converting the 'class' column in the dataset to a factor (categorical variable)
dataset$class <- as.factor(dataset$class)

# Getting information about the data
# Top Five Observations
head(dataset)

#Getting a summary of the data
summary(dataset)

# Size Information
dim(dataset)

# Creating a frequency table for the 'class' variable in the dataset
table(dataset$class)

# Missing Value Check
sum(is.na(dataset))
missing_values <- sum(is.na(dataset))
cat("Total Number of Missing Values:", missing_values, "\n")

# Repeated Rows
duplicated_rows <- dataset[duplicated(dataset), ]
cat("The number of repeated Rows:", nrow(duplicated_rows), "\n")

# Checking Data Types
str(dataset)

# Checking for logical inconsistencies in the data
logical_inconsistencies <- subset(dataset, pressurehight < pressurelow)
cat("Rows with illogical pressure values:", nrow(logical_inconsistencies), "\n")

# Data Validation
# Removing logical inconsistencies from the data in which pressurehight < pressurelow
dataset <- dataset[-c(151,152,153,210,516,901,1204,1272),]

##### DATA VISUALIZATON

# install.packages(ggplot2)
library(ggplot2)

# Example: Box plot of Age by Class
ggplot(dataset, aes(x = class, y = age)) + 
  geom_boxplot() +
  labs(title = "Box plot of Age by Class", x = "Class", y = "Age")

# Example: Pie chart for the distribution of 'class'
class_counts <- table(dataset$class)
colors <- c("red", "green")

pie(class_counts, labels = names(class_counts), col = colors,
    main = "Distribution of Class")

# Example: Histogram for the 'age' variable
hist(dataset$age, col = "skyblue", main = "Histogram of Age",
     xlab = "Age", ylab = "Frequency")

# Example: Scatterplot for 'pressurehight' vs 'pressurelow'
plot(dataset$pressurehight, dataset$pressurelow, 
     col = ifelse(dataset$class == "negative", "blue", "red"),
     main = "Scatterplot of Pressure (High vs Low)",
     xlab = "Pressure High", ylab = "Pressure Low",
     pch = 16)
legend("topright", legend = c("negative", "positive"), col = c("blue", "red"), pch = 16)


##### ALGORITHMS
# KNN Algorithm
# Stratified Three-way Split
# Creating training, validation, and testing datasets
# install.packages("caret")

# Creating a stratified 70-30 split for training and the rest of the data
set.seed(1)
my_indexes <- caret::createDataPartition(y = dataset$class, times = 1, p = .70, list = FALSE)
training <- as.data.frame(dataset[my_indexes,])
the_rest <- as.data.frame(dataset[-my_indexes,])

# Further splitting the rest into 50-50 for validation and test sets
set.seed(1)
my_indexes <- caret::createDataPartition(y = the_rest$class, times = 1, p = .50, list = FALSE)
validation <- as.data.frame(the_rest[my_indexes,])
test <- as.data.frame(the_rest[-my_indexes,])

# Displaying frequency tables for 'class' variable in different datasets
table(dataset$class)
table(training$class)
table(validation$class)
table(test$class)

# Initializing empty vectors for accuracy (acc) and precision (prec)
acc <- NULL
prec <- NULL

# Looping through k values from 1 to 20 for kNN modeling
for(i in 1:20){
  ##### MODELING
  # Applying kNN algorithm
  
  # Loading the 'class' library for kNN algorithm
  # install.packages("class")
  library(class)
  set.seed(1)
  
  # Applying kNN algorithm on training and validation sets
  (knn_predictions <- knn(train = training[, -9], test = validation[, -9], cl = training[[9]], k = i))
  
  # Evaluating the performance using confusion matrix
  myConf <- confusionMatrix(data = knn_predictions, reference = validation[[9]], dnn = c("Predictions", "Actual/Reference"), mode = "everything")
  acc[i] <- myConf$overall["Accuracy"]
  prec[i] <- myConf$byClass["Pos Pred Value"]
}

# Plotting k-values against accuracy
plot(x=1:length(prec), y = acc, type = "o", col = "blue", xlab = "k value", ylab = "Accuracy", main = "Performance Evaluation of kNN Classifier", ylim = c(0.5,1))
text(x=1:length(acc), y = acc,  round(acc,2), cex=1, pos=3, col="red") 
axis(side=2, at=c(seq(from = 0.5, to=1, by=0.1)))

# Adding horizontal grid lines
abline(h = c(seq(from = 0.5, to=1, by=0.1)), lty = 2, col = "grey")

# Adding vertical grid lines
abline(v = 1:20,  lty = 2, col = "grey")

# Testing the kNN model performance on the test set with k=12
set.seed(1)
(knn_predictions <- knn(train = training[, -9], test = test[, -9], cl = training[[9]], k = 12))

# Evaluating the performance on the test set using confusion matrix
myConf <- confusionMatrix(data = knn_predictions, reference = test[[9]], dnn = c("Predictions", "Actual/Reference"), mode = "everything", positive = "positive")
myConf

######################################################################

# Decision Tree (C5.0) Algorithm
# Creating training and testing datasets
# install.packages("caret")
# Loading the 'caret' package for data partitioning
library(caret)

# Creating a 70-30 split for training and test datasets
set.seed(1)
my_indexes <- createDataPartition(y = dataset$class, p = .70, list = FALSE)
training <- as.data.frame(dataset[my_indexes,])
test <- as.data.frame(dataset[-my_indexes,])

##### MODELING
# Applying C5.0 Algorithm
# install.packages("C50")

# Loading the 'C50' package for decision tree modeling
library(C50)

# Building a C5.0 decision tree model using the training data
C50_model <- C5.0(x = training[,-9], y = training$class)

# Printing the details of the C5.0 decision tree model
print(C50_model)

# Displaying a summary of the C5.0 decision tree model
summary(C50_model)

# install.packages("party")
# install.packages("partykit")
# Loading 'party' and 'partykit' packages for plotting
library(party)
library(partykit)

# Plotting the C5.0 decision tree model
plot(C50_model)

# Assessing variable importance using C5imp function
C5imp(C50_model, metric = "usage")

# Making predictions on the test set using the C5.0 model
C50_predictions <- predict(C50_model, newdata = test[,-9])

# Evaluating the performance of the C5.0 model using confusion matrix
confusionMatrix(data = C50_predictions, reference = test[[9]], dnn = c("Predictions", "Actual/Reference"), mode = "everything", positive = "positive")

######################################################################

# Naive Bayes Algorithm
# Creating training and testing datasets
#install.packages("caret")

# Loading the 'caret' package for data partitioning
library(caret)

# Creating a 70-30 split for training and test datasets
set.seed(1)
my_indexes <- createDataPartition(y = dataset$class, p = 0.70, list = FALSE)
training <- as.data.frame(dataset[my_indexes,])
test <- as.data.frame(dataset[-my_indexes,])

# Displaying class distribution in the original, training, and test sets
table(dataset$class)
table(training$class)
table(test$class)

# MODELING
# Applying Naive Bayes algorithm
# install.packages("e1071")
library(e1071)
naiveB_model <- naiveBayes(training[,1:8], training[[9]])
naiveB_model

# Finding Predictions of The Model
(nb_predictions <- predict(naiveB_model, test[,1:8]))
(nb_probs <- predict(naiveB_model, test[,1:8], "raw"))

# Create a results data frame
results <- data.frame(test[[9]], nb_predictions, nb_probs)

# Finding Predictions of The Model
(my_table <- table(nb_predictions, test[[9]], dnn = c("Predictions", "Actual/Reference")))
print(my_table)

# Evaluating the performance of the C5.0 model using confusion matrix
confusionMatrix(data = nb_predictions, reference = test$class, 
                dnn = c("Predictions", "Actual/Reference"), 
                mode = "everything",
                positive = "positive")

# REFERENCES:
# The dataset is obtained from https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data on December 27, 2023.
# Sozan S. Maghdid , Tarik A. Rashid. (2023).Heart Disease Classification Dataset (V2 ed.). https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data
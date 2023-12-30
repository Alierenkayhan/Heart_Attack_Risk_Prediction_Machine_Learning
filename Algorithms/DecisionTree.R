# Author: Ali Eren Kayhan - Emre Yildirim
# Date: 2023
# Title: HeartAttack Machine Learning - KNN/Decision Tree/Naive Bayes
# Decision Tree

##### Reading data from a CSV file into the 'dataset' variable
file_path <- file.path("Data", "HeartAttack.csv")
dataset <- read.table(file_path, header = T, sep = ",")

##### DATA PRE-PROCESSING

# Converting the 'class' column in the dataset to a factor (categorical variable)
dataset$class <- as.factor(dataset$class)

# Displaying a summary of the dataset
summary(dataset)

# Creating a frequency table for the 'class' variable in the dataset
table(dataset$class)

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

# Modeli RDS format??nda kaydet
saveRDS(C50_model, "Models/decisionTree_model.RDS")

# Evaluating the performance of the C5.0 model using confusion matrix
confusionMatrix(data = C50_predictions, reference = test[[9]], dnn = c("Predictions", "Actual/Reference"), mode = "everything", positive = "positive")

# REFERENCES:
# The dataset is obtained from https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data on December 27, 2023.
# Sozan S. Maghdid , Tarik A. Rashid. (2023).Heart Disease Classification Dataset (V2 ed.). https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data
# Author: Ali Eren Kayhan - Emre Yildirim
# Date: 2023
# Title: HeartAttack Machine Learning - KNN/Decision Tree/Naive Bayes
# KNN

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

# Stratified Three-way Split
# Creating training, validation, and testing datasets
# install.packages("caret")

# Creating a stratified 60-40 split for training and the rest of the data
set.seed(1)
my_indexes <- caret::createDataPartition(y = dataset$class, times = 1, p = .60, list = FALSE)
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

# Testing the kNN model performance on the test set with k=13
set.seed(1)
(knn_predictions <- knn(train = training[, -9], test = test[, -9], cl = training[[9]], k = 13))

# Modeli RDS format??nda kaydet
saveRDS(knn_predictions, "Models/knn_model.RDS")

# Evaluating the performance on the test set using confusion matrix
myConf <- confusionMatrix(data = knn_predictions, reference = test[[9]], dnn = c("Predictions", "Actual/Reference"), mode = "everything", positive = "positive")
myConf

# REFERENCES:
# The dataset is obtained from https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data on December 27, 2023.
# Sozan S. Maghdid , Tarik A. Rashid. (2023).Heart Disease Classification Dataset (V2 ed.). https://www.kaggle.com/datasets/bharath011/heart-disease-classification-dataset/data
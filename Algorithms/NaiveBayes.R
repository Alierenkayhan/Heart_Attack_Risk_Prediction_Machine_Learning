# Author: Ali Eren Kayhan - Emre Yildirim
# Date: 2023
# Title: HeartAttack Machine Learning - KNN/Decision Tree/Naive Bayes
# Naive Bayess

#Read data from csv
file_path <- file.path("Data", "HeartAttack.csv")
dataOfHeartAttack <- read.table(file_path, header = TRUE, sep = ",")


#Get information about the data
    # Top Five Observations
    head(dataOfHeartAttack)
    
    #Get a summary of the data
    summary(dataOfHeartAttack)
    
    # Size Information
    dim(dataOfHeartAttack)
    
    # Missing Value Check
    sum(is.na(dataOfHeartAttack))
    missing_values <- sum(is.na(dataOfHeartAttack))
    cat("Total Number of Missing Values:", missing_values, "\n")
    
    # Repeated Rows
    duplicated_rows <- dataOfHeartAttack[duplicated(dataOfHeartAttack), ]
    cat("The number of repeated Rows:", nrow(duplicated_rows), "\n")
    
    # Check Data Types:
    str(dataOfHeartAttack)
    
    # Select only numeric columns
    numeric_columns <- sapply(dataOfHeartAttack, is.numeric)
    numeric_data <- dataOfHeartAttack[, numeric_columns]
    
    # Min-Max Normalization
    scaled_numeric_data <- as.data.frame(scale(numeric_data))
    
    # Combine the scaled numeric data with the non-numeric column
    scaled_data <- cbind(scaled_numeric_data, dataOfHeartAttack$class)
    
    # Select only columns of character type
    character_columns <- sapply(dataOfHeartAttack, is.character)
    character_data <- dataOfHeartAttack[, character_columns]
    
    # Convert each categorical column to factor type
    factor_data <- as.data.frame(lapply(character_data, as.factor))
    
    # Determining Discrete Variables
    discrete_variables <- sapply(dataOfHeartAttack, function(x) is.factor(x) || is.integer(x))
    discrete_data <- dataOfHeartAttack[, discrete_variables]
    
#Data Visualization
    # install.packages(ggplot2)
    library(ggplot2)
    
    # Example: Box plot of Age by Class
      ggplot(dataOfHeartAttack, aes(x = class, y = age)) + 
        geom_boxplot() +
        labs(title = "Box plot of Age by Class", x = "Class", y = "Age")
      
    # Example: Pie chart for the distribution of 'class'
      class_counts <- table(dataOfHeartAttack$class)
      colors <- c("red", "green")  # You can customize the colors
      
      pie(class_counts, labels = names(class_counts), col = colors,
          main = "Distribution of Class")
      
    # Example: Histogram for the 'age' variable
      hist(dataOfHeartAttack$age, col = "skyblue", main = "Histogram of Age",
           xlab = "Age", ylab = "Frequency")
      
    # Example: Scatterplot for 'pressurehight' vs 'pressurelow'
      plot(dataOfHeartAttack$pressurehight, dataOfHeartAttack$pressurelow, 
           col = ifelse(dataOfHeartAttack$class == "negative", "blue", "red"),
           main = "Scatterplot of Pressure (High vs Low)",
           xlab = "Pressure High", ylab = "Pressure Low",
           pch = 16)
      legend("topright", legend = c("negative", "positive"), col = c("blue", "red"), pch = 16)
      
      
#Program and algorithm
      # Naive Bayes
        # Creating training and testing datasets
          #install.packages("caret")
          library(caret)
          set.seed(1)
          my_indexes <- createDataPartition(y = dataOfHeartAttack$DecisionLabel, p = .90, list = FALSE)
          training <- as.data.frame(dataOfHeartAttack[my_indexes,])
          test <- as.data.frame(dataOfHeartAttack[-my_indexes,])
          
          
          table(dataOfHeartAttack$DecisionLabel)
          table(training$DecisionLabel)
          table(test$DecisionLabel)
      
      

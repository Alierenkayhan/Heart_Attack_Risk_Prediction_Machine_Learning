#Read data from csv
data <- read.csv("heart_attack_prediction_dataset.csv", sep = ",", header = T, stringsAsFactors = T)

#Get information about the data
    # Top Five Observations
    head(data)
    
    #Get a summary of the data
    summary(data)
    
    # Size Information
    dim(data)
    
    # Missing Value Check
    sum(is.na(data))
    missing_values <- sum(is.na(data))
    cat("Total Number of Missing Values:", missing_values, "\n")
    
    # Repeated Rows
    duplicated_rows <- data[duplicated(data), ]
    cat("The number of repeated Rows:", nrow(duplicated_rows), "\n")
    
    # Min-Max Normalization
    normalized_data <- as.data.frame(scale(data))
    
    # Select only numeric columns
    numeric_columns <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_columns]
    
    # Normalizasyon
    normalized_data <- as.data.frame(scale(numeric_data))
    
    # Select only columns of character type
    
    # Convert each categorical column to factor type

    # Dummy Coding  
    
    # Determining Discrete Variables
 
#Data Visualization
 
#Program and algorithm

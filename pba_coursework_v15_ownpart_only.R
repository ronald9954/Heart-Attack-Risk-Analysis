# *************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# *************************
# PRACTICAL BUSINESS ANALYTICS COURSEWORK
# *************************
# R Script For HEART ATTACK RISK DATA BASE ANALYSIS
# *************************
# This is where R starts execution
# *************************
# clears all objects in "global environment"
rm(list=ls())
# clears the console area
cat("\014")
# Starts random numbers at the same sequence
set.seed(14) #For reproducibility, to be removed if randomness is preferred
# prints the current working directory
# remember to set directory to your folder using Session>Set Working Directory
print(paste("WORKING DIRECTORY: ",getwd()))
# Make sure you have heart.csv in current working directory
# Define and then load the libraries used in this project

OUTPUT_FIELD      <- "HeartAttackRisk"     

DISCRETE_BINS     <- 6  
OUTLIER_CONF      <-.95                 # Confidence p-value for outlier detection

TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded


#Please call libraries here------
library(caret)
library(pacman)
library(ggplot2)
library(caret)
library(randomForest)
library(kernlab)
library(nnet)
library(naivebayes)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#Load additional R script files provide for this lab
print("START COURSEWORK CODE")
Heart<-read.csv("heart_attack_prediction_dataset.csv",encoding="UTF-8",stringsAsFactors = FALSE)
print("Dataset is in a frame called Heart")

#****************************
#DATA EXPLORATION------
#****************************

#Take a look at how the dataset looks
summary(Heart)
str(Heart)

#Result:There are 26 columns and 8763 rows



# Separate Blood.Pressure into Systolic and Diastolic columns in the Heart dataframe
# Early data preparation since we need to visualize them.
Heart <- transform(Heart,
                   Systolic = as.numeric(substring(Blood.Pressure, 1, regexpr("/", Blood.Pressure) - 1)),
                   Diastolic = as.numeric(substring(Blood.Pressure, regexpr("/", Blood.Pressure) + 1)))

# Remove the original Blood.Pressure column
Heart <- subset(Heart, select = -c(Blood.Pressure))
#Dropping Patient ID as well since it is useless to analysis
Heart <- subset(Heart, select = -c(Patient.ID))

#Remove all the " . " dots from column names
#FROM COMM053 PRACTICAL BUSINESS ANALYTICS LAB CODES
NPREPROCESSING_removePunctuation <- function(fieldName) {
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# Applying the function directly to column names of 'Heart'
names(Heart) <- sapply(names(Heart), NPREPROCESSING_removePunctuation)

# Display the modified column names
print(names(Heart))


#Checking if there are missing values in the data frame
missing_values <- is.na(Heart)
any_missing <- any(missing_values)
print(any_missing)
#Returns FALSE as no missing value or na

manualTypes <- data.frame()

#FROM COMM053 PRACTICAL BUSINESS ANALYTICS LAB CODES
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      b
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}
field_types<-NPREPROCESSING_initialFieldType(Heart)
print(table(field_types))

#******************************************************************************************************************************************
#Seperating the Numerical and Symbolic columns for EDA, Interpretability and to apply different Data Processing Techniques:
#*******************************************************************************************************************************************

# Assigning the the names of the columns that as numeric to numeric_fields
#Transferring the numeric columns from the actual dataset Heart to Numericals

numeric_fields<-names(Heart)[field_types=="NUMERIC"]
Numericals<-Heart[field_types=="NUMERIC"]
print("NUMERIC FIELDS INCLUDE:")
print(numeric_fields)

symbolic_fields<-names(Heart)[field_types=="SYMBOLIC"]
symbolic<-Heart[field_types=="SYMBOLIC"]
print("SYMBOLIC FIELDS INCLUDE:")
print(symbolic_fields)


#FROM COMM053 PRACTICAL BUSINESS ANALYTICS LAB CODES
NPREPROCESSING_discreteNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #191020NRT use R hist() function to create 10 bins
      histogramAnalysis<-hist(dataset[,field], breaks = 10, plot=FALSE)
      bins<-histogramAnalysis$counts/length(dataset[,field])*100  # Convert to %
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discrete value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCRETE
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Type of field is the chart name
      hist(dataset[,field], breaks = 10, plot=TRUE,
           main=paste(graphTitle,field_types[field]),
           xlab=names(dataset[field]),ylab="Number of Records",
           yaxs="i",xaxs="i",border = NA)
      
    } #endif numeric types
  } #endof for
  return(field_types)
}
field_types1<-NPREPROCESSING_discreteNumeric(dataset=Heart,
                                             field_types=field_types,
                                             cutoff=DISCRETE_BINS)
print(field_types1)
#converting to dataframe 
results<-data.frame(field=names(Heart),initial=field_types,types1=field_types1)
#Here we can see that the Age is ordinal as age has many unique values,there are very few spaces between the bins and then noofbins with vales less than 1% is less that the cuttoff value
print(formattable::formattable(results))


discrete<-Heart[,which(field_types1==TYPE_DISCRETE)]
discrete
length(discrete)

ordinals<-Heart[,which(field_types1==TYPE_ORDINAL)]
ordinals
length(ordinals)

zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
zscaled

cor_matrix<-cor(zscaled)
cor_matrix<-round(cor_matrix,digits=4)
library(reshape2)

melted_cor_matrix <- melt(cor_matrix)

heatmap_plot <- ggplot(melted_cor_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-0.2, 0.2), space = "Lab",  # Adjust these limits based on your data
                       name="Correlation", breaks = seq(-0.2, 0.2, by = 0.05)) +  # Adjust breaks for color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_blank()) +
  labs(fill = "Correlation")

# Display the heatmap
print(heatmap_plot)

# Result:There is no strong correlation among input ordinal columns. 

#Checking for outliers
#FROM COMM053 PRACTICAL BUSINESS ANALYTICS LAB CODES
NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

check_outliers <- function(data, confidence) {
  outlier_list <- list()
  
  for (field in names(data)) {
    # Calculate the Chi-squared outlier scores for each column
    scores <- outliers::scores(data[[field]], type = "chisq", prob = abs(confidence))
    outliers <- which(scores > 0) # Identify the outliers
    
    # Add to list if outliers are found
    if (length(outliers) > 0) {
      outlier_list[[field]] <- length(outliers)
      print(paste("Outliers detected in field:", field, "Count:", length(outliers)))
    }
  }
  
  if (length(outlier_list) == 0) {
    print("No outliers detected based on the given confidence level.")
  } else {
    return(outlier_list)
  }
}

outlier_check_results <- check_outliers(ordinals, OUTLIER_CONF)
#No outliers detected based on the given confidence level.

#check for duplicate rows ** the result is FALSE, so there are no duplicate rows
rowduplicates<-any(duplicated(Heart))
print(rowduplicates)


#DATA VISUALISATION -------
# **************************************************************
# **************************************************************
# *************************
# Visualise the dataset---
# Visulization function. It shows in each group, how many percent has heart attack risk, say, 0.6, 60% in the group has Heart attack risk
investigateAttributeByGroup <- function(data , attributeName , startIndex , endIndex, byRange){
  groupName <-paste(attribute_name, "_Group", sep = "") 
  data[[groupName]] <- cut(data[[attribute_name]], breaks = seq(startIndex, endIndex, by = byRange), include.lowest = TRUE, labels = FALSE)
  
  
  
  if (byRange >= 1) {
    data[[groupName]] <- paste(data[[groupName]] ,":" , data[[groupName]]*byRange-(byRange-1) , "to" , data[[groupName]]*byRange )
  }else{
    data[[groupName]] <- data[[groupName]]  - 1
  }
  print(data[, c(attributeName, groupName )])
  # Counting occurrences by attribute group and HeartAttackRisk
  counts <- with(data, table(get(groupName), HeartAttackRisk))
  # Converting counts to a data frame
  counts_df <- as.data.frame(counts)
  #counts_df$Var1 <- paste(counts_df$Var1 ,":" , counts_df$Var1*byRange-(byRange-1) , " to " , counts_df$Var1*byRange )
  print(counts_df)
  # Calculate the count ratio of red to black
  counts_df$Ratio <- counts_df$Freq[which(counts_df$HeartAttackRisk == 1)] / (counts_df$Freq[which(counts_df$HeartAttackRisk == 1)] + counts_df$Freq[which(counts_df$HeartAttackRisk == 0)])
  counts_df <- subset(counts_df, HeartAttackRisk==1)
  
  # Identify the maximum ratio value
  max_ratio <- max(counts_df$Ratio)
  # Plotting the bar chart with count ratio on y-axis
  ggplot(counts_df, aes(x = Var1, y = Ratio, fill = Ratio)) +
    geom_col() +
    scale_fill_gradient(low = "gray", high = "black") +
    labs(x = groupName, y = "Count Ratio (1/0+1)", fill = attributeName) +
    theme_minimal() +
    guides(fill = FALSE) +
    geom_text(aes(label = ifelse(Ratio == max_ratio, "Max", "")), vjust = -0.5, color = "white")
}



#======================================================================================
#We will go over 5 of the attributes in this code.
# Visualisation Part 1

#Investigating the cholesterol groups relations
attribute_name <- "Cholesterol"
startIndex <- 0
endIndex <- 400
byRange <- 50

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)

#Description: In Cholesterol, the chance of having HeartAttackRisk is linear with the cholesterol group
#We can keep the attribute linear and be normalized.


#======================================================================================

#Investigating the age group relations
attribute_name <- "Triglycerides"
startIndex <- 0
endIndex <- 800
byRange <-100

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)

#Description: The chance distribution forms a U-shape, with higher chance when number is large, and number is small.
#It would be more suitable to use a encoding for these features.


#======================================================================================

# Investigating the Age relations

attribute_name <- "Age"
startIndex <- 0
endIndex <- 120
byRange <-10

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)
#Description: Younger group has a slightly higher risk, and so does the higher aged group
#We can separate them into age groups


#======================================================================================

#Investigating the systolic blood pressure groups relations

attribute_name <- "Systolic"
startIndex <- 0
endIndex <- 200
byRange <-20

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)
#Description: The chance goes up linearly with the systolic group
# We can keep it as linear as it is an normalised

#The rest of the columns have a slight linear trend / trendless, without the 
# need of one-hot encoding/grouping.
#======================================================================================
#======================================================================================
# Extra data columns - Derivation and External database

#Extra data column 1 - Health care index

#Currently, the countries are:
print(unique(Heart$Country))
print(length(unique(Heart$Country))) #20 countries on the list

#It becomes more challenging to train our model if we have too many columns.
#Also, if we have patients from other countries out of the list, 
#we cannot apply their data to the model.
#Or else we need to re-train the model.

#Therefore we adopted data from GLOBAL HEALTH SECURITY INDEX
# From: https://www.ghsindex.org/report-model/
#We want to know if there is extra risk if the patient comes from a country of 
#worse health care condition.

#The report gives a ranking and overall rating for a country's health care system.

#Country Health Careoverall index Creation 
Heart$CountryHealthCare <- NA

Heart$CountryHealthCare[Heart$Country == "Argentina"] <- 54.4
Heart$CountryHealthCare[Heart$Country == "Canada"] <- 69.8
Heart$CountryHealthCare[Heart$Country == "France"] <- 61.9
Heart$CountryHealthCare[Heart$Country == "Thailand"] <- 68.2

Heart$CountryHealthCare[Heart$Country == "Germany"] <- 65.5 
Heart$CountryHealthCare[Heart$Country == "Japan"] <- 60.5
Heart$CountryHealthCare[Heart$Country == "Brazil"] <- 51.2
Heart$CountryHealthCare[Heart$Country == "South Africa"] <- 45.8

Heart$CountryHealthCare[Heart$Country == "United States"] <- 75.9
Heart$CountryHealthCare[Heart$Country == "Vietnam"] <- 42.9
Heart$CountryHealthCare[Heart$Country == "China"] <- 47.5
Heart$CountryHealthCare[Heart$Country == "Italy"] <- 51.9


Heart$CountryHealthCare[Heart$Country == "Spain"] <- 60.9
Heart$CountryHealthCare[Heart$Country == "India"] <- 42.8
Heart$CountryHealthCare[Heart$Country == "Nigeria"] <- 38
Heart$CountryHealthCare[Heart$Country == "New Zealand"] <- 62.5

Heart$CountryHealthCare[Heart$Country == "South Korea"] <- 65.4
Heart$CountryHealthCare[Heart$Country == "Australia"] <- 71.1
Heart$CountryHealthCare[Heart$Country == "Colombia"] <- 53.2
Heart$CountryHealthCare[Heart$Country == "United Kingdom"] <- 67.2


#Investigating the Health Care Index

attribute_name <- "CountryHealthCare"
startIndex <- 0
endIndex <- 100
byRange <-10

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)
#It appears countries with lowest index and highest index have a higher risk.
#It can be done by encoding the top and bottom classes.


#======================================================================================



# Stress alone is very common, but we can see what if a person is stressed and relying on alcohol
Heart$StressDrink<- 0
Heart$StressDrink <- (Heart$StressLevel > 7) & (Heart$AlcoholConsumption == 1)
Heart$StressDrink <- as.numeric(Heart$StressDrink)


#Investigating the StressDrink
attribute_name <- "StressDrink"
startIndex <- 0
endIndex <- 1
byRange <-0.5

investigateAttributeByGroup(Heart, attribute_name, startIndex, endIndex, byRange)

#Unexpectedly people who has a high stress level with alcohol consumption has 
# 3% lower proportion of people having HeartAttackRisk.


#======================================================================================
#======================================================================================
#Data Preparation
#======================================================================================

#=========================================================================================
# Function to do one hot encoding, and to cut values into different groups
# It separates the column in numeric based on the value, then cut into different column groups
cut_one_hot_encode_columns <- function(data , columnName , cuttingList ) {
  
  #Cut the values to different groups according to your cutting list
  newColumnName = paste0(columnName, "_Group")
  data[[newColumnName]] <- cut(data[[columnName]], breaks = cuttingList, labels = FALSE, right = FALSE)
  
  #create a for loop
  group_values <- 1:(length(cuttingList)-1)
  
  #making new groups
  for (i in group_values) {
    column_name <- paste0(columnName,"_Group_", i)
    data[[column_name]] <- as.numeric(data[[newColumnName]] == i)
  }
  
  
  columnsToDrop <- c(columnName, newColumnName)
  
  data <- data[, setdiff(names(data), columnsToDrop)]
  print(paste("Completed one-hot encoding for", columnName, "and turned it into", newColumnName, "group 1 to", (length(cuttingList)-1)))
  return(data)
}

#==========================================================================================

summary(Heart)
#DATA PREPARATION STARTS HERE------
# Separate Blood.Pressure into Systolic and Diastolic columns in the Heart dataframe

# Put Heart Attack risk to the back
Heart <- Heart[, c(setdiff(names(Heart), "HeartAttackRisk"), "HeartAttackRisk")]

#Print the column names to double check
column_names <- colnames(Heart)
print(column_names)
#==========================================================================================

# Function to normalize numeric columns by minmax
normalize_columns <- function(data , col_list) {
  normalized_data <- data
  for (col in col_list) {
    # Min-Max normalization
    min_val <- min(data[[col]], na.rm = TRUE)
    max_val <- max(data[[col]], na.rm = TRUE)
    normalized_data[[col]] <- (data[[col]] - min_val) / (max_val - min_val)
  }
  return(normalized_data)
}


# Function to decimal scale normalize numeric columns
decimal_scale_normalize_columns <- function(data , col_list) {
  normalized_data <- data
  for (col in col_list) {
    # Decimal scaling normalization
    max_abs_val <- max(abs(data[[col]]), na.rm = TRUE)
    normalized_data[[col]] <- data[[col]] / (10 ^ (ceiling(log10(max_abs_val))))
  }
  return(normalized_data)
}


# Function to z-score normalize numeric columns
z_score_normalize_columns <- function(data , col_list) {
  normalized_data <- data
  for (col in col_list) {
    # Z-score normalization
    mean_val <- mean(data[[col]], na.rm = TRUE)
    sd_val <- sd(data[[col]], na.rm = TRUE)
    normalized_data[[col]] <- (data[[col]] - mean_val) / sd_val
  }
  return(normalized_data)
}




#==========================================================================================

#ORDINAL ENCODING------

# Define the levels and their corresponding numeric values
levels_mapping <- c(Unhealthy = 0, Average = 1, Healthy = 2)

# Apply the mapping to the 'Diet' column
Heart$Diet <- levels_mapping[Heart$Diet]

#Check if Male, else its Female in the database's scope
Heart$Sex <- as.numeric(Heart$Sex == 'Male')

#Encoding Triglycerides and Age as mentioned above in data preparation

#One hot encoding on Triglycerides
Heart<- cut_one_hot_encode_columns(Heart , "Triglycerides" , c(0, 200, 500, 1000) )

#One hot encoding on Age
Heart<- cut_one_hot_encode_columns(Heart , "Age" , c(0, 20, 50, 70, 100) )

#One hot encoding on Country Health Care Index
Heart<- cut_one_hot_encode_columns(Heart , "CountryHealthCare" , c(0, 40, 70, 100) )

#Since we have countries index to cover, we can drop "Country", "Continent", "Hemisphere"
Heart <- subset(Heart, select = -c(Country,Continent,Hemisphere))

#The rest of the columns can use normalisation
#But since we will be using different kinds of normalisation
#We will define Heart0

#List of columns with 1 and 0 only
columns_with_2_unique_values <- names(Heart)[sapply(Heart, function(x) length(unique(x))) == 2]
#Exclude these columns during normalisation
norm_cols <- setdiff(names(Heart), columns_with_2_unique_values)

Heart0 <- normalize_columns(Heart,norm_cols)
#After that, we want to figure out the importance of each columns
#Then first build a baseline logistic regression model as comparison
#==========================================================================================
#==========================================================================================

#Before we separate to training_set, testing_set, and building model
#We want to figure out if there is any data imbalance


#Databalancing ---
# Counting how unbalanced is the target variable
zero_count <- sum(Heart$HeartAttackRisk == 0)
one_count <- sum(Heart$HeartAttackRisk == 1)

cat("Number of occurrences of 0 in HeartAttackRisk column:", zero_count, "\n")
cat("Number of occurrences of 1 in HeartAttackRisk column:", one_count, "\n")
#Number of occurrences of 0 in HeartAttackRisk column: 5624 
#Number of occurrences of 1 in HeartAttackRisk column: 3139

#We will carry out oversample on training_set to cope with the data imbalance
#No oversampling will be done on testing_set

#Since we will prepare multiple dataset, we want to write a function for separation to training_set and testing_set

#==========================================================================================

#The function where we separate to training set and testing set
#It is used for models with built in validation set creation
#We use 60 train 20 validation and 20 test
#By calling this function, the user always get the same set of train valid test
data_split_w_valid <- function(data) {
  # Split sizes
  train_size <- floor(0.6 * nrow(data))
  valid_size <- floor(0.2 * nrow(data))
  set.seed(49)
  # Randomly sample indices for the training set
  train_indices <- sample(seq_len(nrow(data)), train_size)
  
  # Get the indices for the validation and testing sets
  valid_indices <- sample(setdiff(seq_len(nrow(data)), train_indices), valid_size)
  test_indices <- setdiff(seq_len(nrow(data)), c(train_indices, valid_indices))
  
  # Create the training, validation, and testing sets
  training_set <- data[train_indices, ]
  validating_set <- data[valid_indices, ]
  testing_set <- data[test_indices, ]
  
  #Balance the training set
  training_set_Heart_risk1 <- training_set[training_set$HeartAttackRisk == 1, ]
  training_set <- rbind(training_set, training_set_Heart_risk1)
  
  # Return all sets as a list
  return(list(training_set = training_set, validating_set = validating_set, testing_set = testing_set))
}



#The function where we separate to training set and testing set
#It is used for models with built in validation set creation
#By calling this function, the user always get the same set of train and test set
#Test set is same as the above function
#This function is used, because some of our members use library where it can 
#autmatically create validation set
data_split <- function(data, split_ratio) {
  
  #split_ratio unused, but remain to prevent bugs in version control
  
  new_data <- data_split_w_valid(data)
  value_set_risk_1 <- new_data$validating_set[new_data$validating_set$HeartAttackRisk == 1, ]
  new_data$validating_set <- rbind(new_data$validating_set, value_set_risk_1)
  new_data$training_set <- rbind(new_data$training_set, new_data$validating_set)
  
  
  # Return both sets as a list
  return(list(training_set = new_data$training_set, testing_set = new_data$testing_set))
}




#==========================================================================================
#Split Heart0 to training set and testing set
split_Heart0 <- data_split_w_valid(Heart0 )
Heart0_training <- split_Heart0$training_set
Heart0_validating <- split_Heart0$validating_set
Heart0_testing <- split_Heart0$testing_set

#Checking training set balance
zero_count <- sum(Heart0_training$HeartAttackRisk == 0)
one_count <- sum(Heart0_training$HeartAttackRisk == 1)

cat("Number of occurrences of 0 in HeartAttackRisk column:", zero_count, "\n")
cat("Number of occurrences of 1 in HeartAttackRisk column:", one_count, "\n")
#3365 occurance of 0 , vs 3784 occurance of 1, where it is much more balanced now.

#==========================================================================================

library(Boruta)
#We use this to seek unimportant attributes

training_data <- Heart0_training

#THIS CODE TAKES 2 MINUTES TO RUN
#ctrl shift c to mass comment/uncomment
###UNCOMMENT FOR NORMAL OPERATION=============================================================

boruta_output <- Boruta(HeartAttackRisk ~ ., data = training_data, doTrace = 0)

# Print the results
print(boruta_output)
#31 attributes confirmed important: Age_Group_1, Age_Group_2, Age_Group_3,
#Age_Group_4, AlcoholConsumption and 26 more;
#No attributes deemed unimportant.

# Plotting the results can also help in visualizing the importance
plot(boruta_output, cex.axis = .7, las = 2, xlab = "", main = "Variable Importance")

# Get names of important features
# Assuming 'boruta_output' is your Boruta model object
importance_scores <- attStats(boruta_output)$meanImp
print(importance_scores)
print(boruta_output)
print((attStats(boruta_output)))


# Sort the importance scores in descending order
sorted_indices <- order(importance_scores, decreasing = TRUE)
#Get the column names in order of importance
important_columns <- names(Heart0)[sorted_indices]

###UNCOMMENT FOR NORMAL OPERATION=============================================================

#To quickly obtain the variable
important_columns <- c("BMI", "Income", "ExerciseHoursPerWeek", "Diastolic", "Systolic", "SedentaryHoursPerDay", "HeartRate", "Cholesterol", "SleepHoursPerDay", "PhysicalActivityDaysPerWeek", "StressLevel", "Triglycerides_Group_1", "Triglycerides_Group_2", "Diet", "AlcoholConsumption", "CountryHealthCare_Group_1", "Age_Group_2", "StressDrink", "Obesity", "Diabetes", "Age_Group_4", "PreviousHeartProblems", "Age_Group_3", "MedicationUse", "FamilyHistory", "Sex", "Age_Group_1", "HeartAttackRisk", "CountryHealthCare_Group_2", "Triglycerides_Group_3")

# Print the columns in sequence of importance
print(important_columns)


important_columns <- c("HeartAttackRisk" , important_columns)

# Get the top 5 columns
top_5_columns <- important_columns[1:5]
print(top_5_columns)

# Get the top 10 columns
top_10_columns <- important_columns[1:10]
print(top_10_columns)

# Get the top 20 columns
top_20_columns <- important_columns[1:20]
print(top_20_columns)

#Now we know "BMI", "Income", "ExerciseHoursPerWeek", "Diastolic", "Systolic"... etc
#Are the most important columns
#We will be building new dataframes for analysis using these important columns
#But we will continue to build a baseline logistic regression for now.
#==========================================================================================
#==========================================================================================

#For formula building
myModelFormula<-function(dataset,fieldNameOutput){
  
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  
  output<-paste(fieldNameOutput,"~")
  
  formular=as.formula(paste(output,inputs))
  
  return(formular)
  
} 

#==========================================================================================
#Function for model evaluation
get_confusion_matrix<-function(predicted,actual){
  
  predicted <- factor(predicted)
  actual <- factor(actual)
  
  # Determine the unique levels in both columns
  levels <- union(levels(predicted), levels(actual))
  
  # Set the levels for the predicted and actual variables
  predicted <- factor(predicted, levels = levels)
  actual <- factor(actual, levels = levels)
  
  # Create the confusion matrix
  cm <- confusionMatrix(data = predicted, reference = actual)
  
  cm$TN <- cm$table[1]
  cm$FP <- cm$table[2]
  cm$FN <- cm$table[3]
  cm$TP <- cm$table[4]
  cm$Precision <- cm$TP / (cm$TP + cm$FP )
  cm$Recall <- cm$TP / (cm$TP + cm$FN )
  cm$Specificity <- cm$TN / (cm$TN + cm$FP)
  cm$FScore <- 2*cm$Recall*cm$Precision/(cm$Recall + cm$Precision)
  
  precision_str <- sprintf("%.2f", cm$Precision)
  recall_str <- sprintf("%.2f", cm$Recall)
  fscore_str <- sprintf("%.2f", cm$FScore)
  
  #cat("Precision:", precision_str, "\n")
  #cat("Recall:", recall_str, "\n")
  #cat("F-score:", fscore_str, "\n")
  
  
  # Print the confusion matrix
  # print(cm)
  return (cm)
} 

print_performance <-function(cm){
  ### Result confusion matrix : Heart0_cm
  accuracy <- cm$overall['Accuracy']
  precision <- cm$Precision
  recall <- cm$Recall
  f1_score <- cm$FScore
  
  confMatrix <- as.matrix(cm$table)
  
  #printing confusion matrix
  print(cm$table)
  print("")
  
  # Printing the results
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall (Sensitivity):", recall, "\n")
  cat("Specificity:",cm$Specificity,"\n")
  cat("F1 Score:", f1_score, "\n")
  
}




#==========================================================================================
### Model 0 training
### Algorithm: Logistic regression
### Columns: All columns 
### Normalisation: Min max normalisation
# Done by: Group Effort, for comparison

#Recall we had : Heart0_training , Heart0_testing 

Heart0_formula<-myModelFormula(dataset=Heart0_training,fieldNameOutput=OUTPUT_FIELD)
Heart0_LogModel<-glm(Heart0_formula,data=Heart0_training, family = "binomial")

#We develop our model with validation set
validate_predictions <- predict(Heart0_LogModel, newdata = Heart0_validating, type = "response")
predicted_classes <- ifelse(validate_predictions >= 0.5, 1, 0)
actual_classes <- Heart0_validating$HeartAttackRisk

Heart0_cm_validate <- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart0_cm_validate)

# Baseline logistic regression on Validation set for finetuning
# Accuracy: 0.4269406 
# Precision: 0.3497689 
# Recall (Sensitivity): 0.7394137 
# Specificity: 0.258348 
# F1 Score: 0.4748954

#We then carry out our final test on testing_set
test_predictions <- predict(Heart0_LogModel, newdata = Heart0_testing, type = "response")
predicted_classes <- ifelse(test_predictions >= 0.5, 1, 0)
actual_classes <- Heart0_testing$HeartAttackRisk

Heart0_cm_test <- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart0_cm_test)

# Accuracy: 0.4606613 
# Precision: 0.3794992 
# Recall (Sensitivity): 0.7601881 
# Specificity: 0.2894265 
# F1 Score: 0.506263 

#==========================================================================================
#And hence the baseline model Model 0 has been created.
#We will then make 5 datasets
#They will be created based on Heart (not normalised yet)
# 1. Heart1_top20 , Heart database with top 20 important columns , and minmax normalisation
# 2. Heart2_top10 , Heart database with top 10 important columns , and minmax normalisation
# 3. Heart3_top5 , Heart database with top 5 important columns , and minmax normalisation
# 4. Heart4_decimal, Heart database with top 10 important columns , and decimal scale normalisation
# 5. Heart5_z_scale , Heart database with top 10 important columns , and z score normalisation

Heart_norm<- normalize_columns(Heart,norm_cols)

Heart1<- Heart_norm #minmax normalise  (and similar below)
Heart1_top20 <- Heart1[top_20_columns] #Get the 20 most important columns only (and similar below)

Heart2<- Heart_norm
Heart2_top10 <- Heart2[top_10_columns]

Heart3<- Heart_norm
Heart3_top5<- Heart3[top_5_columns]

Heart4<- decimal_scale_normalize_columns(Heart,norm_cols) #decimal scale normalise
Heart4_decimal<- Heart4[top_20_columns]

Heart5<- z_score_normalize_columns(Heart,norm_cols) #z score scale normalise
Heart5_z_scale<- Heart5[top_20_columns]


#==========================================================================================
# Model 2 , Heart2_top10
# Heart2_top10 , Heart database with top 10 important columns , and minmax normalisation
# Done by: Ronald L

split_Heart2 <- data_split_w_valid(Heart2_top10 )
Heart2_training <- split_Heart2$training_set
Heart2_validating <- split_Heart2$validating_set
Heart2_testing <- split_Heart2$testing_set

Heart2_formula<-myModelFormula(dataset=Heart2_training,fieldNameOutput=OUTPUT_FIELD)
Heart2_LogModel<-glm(Heart2_formula,data=Heart2_training, family = "binomial")


#We develop our model with validation set
validate_predictions <- predict(Heart2_LogModel, newdata = Heart2_validating, type = "response")
predicted_classes <- ifelse(validate_predictions >= 0.52, 1, 0)
actual_classes <- Heart2_validating$HeartAttackRisk

Heart2_cm_validate <- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart2_cm_validate)
# Accuracy: 0.4777397 
# Precision: 0.3615455 
# Recall (Sensitivity): 0.6400651 
# Specificity: 0.3901582 
# F1 Score: 0.4620811 
# Baseline logistic regression on Validation set for finetuning


#The threshold 0.52 gives the best F1 score and Precision under validation set
#So we adopt it and use it again on the testing set.

#We then carry out our final test on testing_set
test_predictions <- predict(Heart2_LogModel, newdata = Heart2_testing, type = "response")
predicted_classes <- ifelse(test_predictions >= 0.52, 1, 0)
actual_classes <- Heart2_testing$HeartAttackRisk

Heart2_cm_test <- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart2_cm_test)

# Baseline logistic regression on Testing set
# Accuracy: 0.4675029 
# Precision: 0.3649635 
# Recall (Sensitivity): 0.6269592 
# Specificity: 0.3763441 
# F1 Score: 0.461361 

#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#START OF OUR INDIVIDUAL MODELS
#==========================================================================================
# Model choice : Naive Bayes & Naive Bayes ensemble
# Done by: Ronald L
# Applied to : Heart1_top20 , Heart2_top10 , Heart3_top5 Heart database with top 10 important columns , and minmax normalisation




NB_threshold <- 0.55


split_Heart1 <- data_split_w_valid(Heart1_top20  )
Heart1_training <- split_Heart1$training_set
Heart1_validating <- split_Heart1$validating_set
Heart1_testing <- split_Heart1$testing_set

Heart1_training$HeartAttackRisk <- as.factor(Heart1_training$HeartAttackRisk)
Heart1_validating$HeartAttackRisk <- as.factor(Heart1_validating$HeartAttackRisk)
Heart1_testing$HeartAttackRisk <- as.factor(Heart1_testing$HeartAttackRisk)

NB_model1 <- naive_bayes(HeartAttackRisk ~ ., data = Heart1_training, usekernel = TRUE  , usepoisson=TRUE ) 

rm(predicted_classes)

#We write a function for getting the predicted classes

NB_predicted_classes <-function(model , data){
  
  
  if (exists("predicted_classes")) {
    rm(predicted_classes)
  }
  
  predicted_classes <- predict(model, newdata = data , type = "prob" )
  predicted_classes <- predicted_classes>NB_threshold
  predicted_classes <- predicted_classes[, '1']
  predicted_classes <- as.integer(predicted_classes)
  return (predicted_classes)
}

predicted_classes <- NB_predicted_classes(NB_model1 , Heart1_validating )
actual_classes <- Heart1_validating$HeartAttackRisk
Heart_NB_cm<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB_cm)
# Accuracy: 0.4811644 
# Precision: 0.3620206 
# Recall (Sensitivity): 0.6302932 
# Specificity: 0.400703 
# F1 Score: 0.459893 

#Then we run it on test set
predicted_classes <- NB_predicted_classes(NB_model1 , Heart1_testing)
actual_classes <- Heart1_testing$HeartAttackRisk
Heart_NB2_cm_testing<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB_cm)
# Accuracy: 0.4811644 
# Precision: 0.3620206 
# Recall (Sensitivity): 0.6302932 
# Specificity: 0.400703 
# F1 Score: 0.459893 
#Which gives a similar result

#==========================================================================================

#Now we try to build an ensemble model to see if it can improve the performance, where we aim to improve the recall and overall accuracy.
#In next model, we use the following to build the ensemble model
# 1. Heart1_top20 , Heart database with top 20 important columns , and minmax normalisation (built above)
# 2. Heart0 , all columns with minmax normalisation
# 3. Heart3_top5 , Heart database with top 5 important columns , and minmax normalisation
# Note that we cannot use the data with different nomalisation technique.
#==========================================================================================
#Heart2_top10

split_Heart2 <- data_split_w_valid(Heart0)
Heart2_training <- split_Heart2$training_set
Heart2_validating <- split_Heart2$validating_set
Heart2_testing <- split_Heart2$testing_set

Heart2_training$HeartAttackRisk <- as.factor(Heart2_training$HeartAttackRisk)
Heart2_validating$HeartAttackRisk <- as.factor(Heart2_validating$HeartAttackRisk)
Heart2_testing$HeartAttackRisk <- as.factor(Heart2_testing$HeartAttackRisk)

NB_model2 <- naive_bayes(HeartAttackRisk ~ ., data = Heart2_training, usekernel = TRUE  , usepoisson=FALSE ) 

#On the validating set
predicted_classes <- NB_predicted_classes(NB_model2 , Heart2_validating )
actual_classes <- Heart2_validating$HeartAttackRisk
Heart_NB2_cm<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB2_cm)
# Accuracy: 0.3892694 
# Precision: 0.3534704 
# Recall (Sensitivity): 0.8957655 
# Specificity: 0.115993 
# F1 Score: 0.5069124  

#Then we run it on test set
predicted_classes <- NB_predicted_classes(NB_model2 , Heart2_testing)
actual_classes <- Heart2_testing$HeartAttackRisk
Heart_NB2_cm_testing<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB2_cm_testing)
# Accuracy: 0.4019384 
# Precision: 0.3686901 
# Recall (Sensitivity): 0.9043887 
# Specificity: 0.1146953 
# F1 Score: 0.5238311 
#Which gives a similar result, but it shows an imrovement from previous model.

#==========================================================================================

#Heart3_top5
split_Heart3 <- data_split_w_valid(Heart3_top5 )
Heart3_training <- split_Heart3$training_set
Heart3_validating <- split_Heart3$validating_set
Heart3_testing <- split_Heart3$testing_set

Heart3_training$HeartAttackRisk <- as.factor(Heart3_training$HeartAttackRisk)
Heart3_validating$HeartAttackRisk <- as.factor(Heart3_validating$HeartAttackRisk)
Heart3_testing$HeartAttackRisk <- as.factor(Heart3_testing$HeartAttackRisk)

NB_model3 <- naive_bayes(HeartAttackRisk ~ ., data = Heart3_training, usekernel = TRUE  , usepoisson=TRUE ) 

#Validation
predicted_classes <- NB_predicted_classes(NB_model3 , Heart3_validating )
actual_classes <- Heart3_validating$HeartAttackRisk
Heart_NB3_cm<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB3_cm)
# Accuracy: 0.5627854 
# Precision: 0.3389831 
# Recall (Sensitivity): 0.2605863 
# Specificity: 0.7258348 
# F1 Score: 0.2946593 

#Testing
predicted_classes <- NB_predicted_classes(NB_model3 , Heart3_testing)
actual_classes <- Heart3_testing$HeartAttackRisk
Heart_NB3_cm_testing<- get_confusion_matrix(predicted_classes , actual_classes)
print_performance(Heart_NB3_cm_testing)
# Accuracy: 0.5644242 
# Precision: 0.3793103 
# Recall (Sensitivity): 0.3103448 
# Specificity: 0.7096774 
# F1 Score: 0.3413793 

#==========================================================================================

#NB_model1 NB_model2 NB_model3 are built
#We will use Heart1 as it contains all the features they need
#As a reminder that Heart2_top10 and Heart3_top5 are subsets of Heart1_top20
#Calculation is performed based on features to be found in the tables.


NB_emsembled_predict<-function(testing_data ){
  
  if (exists("predicted_classes1")) {
    rm(predicted_classes1)
  }
  
  if (exists("predicted_classes2")) {
    rm(predicted_classes2)
  }
  
  if (exists("predicted_classes3")) {
    rm(predicted_classes3)
  }
  
  predicted_classes1 <- NB_predicted_classes(NB_model1 , testing_data)
  predicted_classes2 <- NB_predicted_classes(NB_model2 , testing_data)
  predicted_classes3 <- NB_predicted_classes(NB_model3 , testing_data)
  combined_NB_predicted <- as.data.frame(cbind(predicted_classes1, predicted_classes2, predicted_classes3))
  combined_NB_predicted$FinalPrediction <- predicted_classes1 + predicted_classes2 + predicted_classes3
  #print(combined_NB_predicted)
  NB_threshold <- 3 
  
  combined_NB_predicted$FinalPrediction <- ifelse(combined_NB_predicted$FinalPrediction >= NB_threshold, 1, 0)
  
  return (combined_NB_predicted$FinalPrediction )
}

#Validation, tuning the threshold to 0.55
final_result_NB <- NB_emsembled_predict(Heart1_validating)
final_result_NB <- as.factor(final_result_NB)
actual_classes <- as.factor(Heart1_validating$HeartAttackRisk)
ensembled_NB <- get_confusion_matrix(final_result_NB , actual_classes)
print_performance(ensembled_NB)
# Accuracy: 0.5793379 
# Precision: 0.3443038 
# Recall (Sensitivity): 0.2214984 
# Specificity: 0.7724077 
# F1 Score: 0.2695738 

#Testing
final_result_NB <- NB_emsembled_predict(Heart1_testing)
final_result_NB <- as.factor(final_result_NB)
actual_classes <- as.factor(Heart1_testing$HeartAttackRisk)
ensembled_NB <- get_confusion_matrix(final_result_NB , actual_classes)
print_performance(ensembled_NB)
# Accuracy: 0.5803877 
# Precision: 0.3896396 
# Recall (Sensitivity): 0.2711599 
# Specificity: 0.7571685 
# F1 Score: 0.3197782 
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================
#==========================================================================================

#==========================================================================================
# Comparing models #

# Logistic regression Model based on different data sets

# Model Precision bar chart for Logistic Regression Models
precisions <- c(37.95,37.33,37.45,36.84,36.51,36.51)
modelNames <- c('All columns','top20 minmax','top10 minmax','top5 minmax','top10 dec','top10 z score')
# bar_colors <- c("red", "green", "blue", "yellow", "orange", "purple", "pink", "cyan", "gray", "brown")
# Precision bar plot
barplot(precisions,
        names.arg = modelNames,
        xlab = "Data set",
        ylab = "Precision %",
        main = "Comparing Logistic Regression Precision by data set",
        col = "#89b3a7",
        ylim = c(20, 50),
        beside = TRUE,
        xpd = FALSE)

# For logistic models

F1Scores <- c(50.63, 50.50, 46.1, 49.69, 44.86, 44.86)

orange_index <- which(modelNames %in% 'All columns')

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_index] <- "orange"

# F1 score bar plot
barplot(F1Scores,
        names.arg = modelNames,
        xlab = "Data set",
        ylab = "F1-Score %",
        main = "Comparing Logistic Regression F1-Score by data set",
        col = bar_colors,
        ylim = c(30, 55),
        beside = TRUE,
        xpd = FALSE)



# For individual models
modelNames <- c('LogReg', 'NB', 'RF(Chris)',
                'NN(Chris)', 'RF(Aqib)', 'SVM(Aqib)', 'DT(Aditya)',
                'KNN')
F1Scores <- c(0.506263, 0.3413793, 0.2234763, 0.4274194, 0.2742616, 0.1282051, 0.4889142, 0.4091559)*100

# Find the index of the desired model name
orange_index <- which(modelNames %in% 'LogReg')

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_index] <- "orange"

# F1 score bar plot
barplot(F1Scores,
        names.arg = modelNames,
        xlab = "Model",
        ylab = "F1-Score %",
        main = "Comparing F1-Score by Model",
        col = bar_colors,
        ylim = c(10, 60),
        beside = TRUE,
        xpd = FALSE)



# For ensemble models

modelNames <- c('NB ensm', 'RF+SVM', 'KNN+RF')
F1Scores <- c(0.3197782, 0.2083333, 0.1983071)*100

# Find the index of the desired model name
orange_index <- which(modelNames %in% 'NB ensm')

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_index] <- "orange"

# F1 score bar plot
barplot(F1Scores,
        names.arg = modelNames,
        xlab = "Model",
        ylab = "F1-Score %",
        main = "Comparing F1-Score by Ensemble Model",
        col = bar_colors,
        ylim = c(10, 50),
        beside = TRUE,
        xpd = FALSE)



######################### visualization of our model comparison starts here #########################

#The code used for comparing the precision of individual models
precisions <- c(37.93, 38.9, 39.912, 41.9, 39.8, 35.21, 37.49, 34.72)
modelNames <- c('NB', 'NB Ens', 'RF(Chris)', 'RF(Aqib)', 'SV+RF', 'SVM', 'DT', 'KNN')

# Find the indices of the desired model names
orange_indices <- which(modelNames %in% c('RF(Chris)', 'RF(Aqib)', 'SV+RF'))

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_indices] <- "orange"

# Precision bar plot
barplot(precisions,
        names.arg = modelNames,
        xlab = "Model names",
        ylab = "Precision %",
        main = "Comparing Precision of each model",
        col = bar_colors,
        ylim = c(20, 45),
        beside = TRUE,
        xpd = FALSE)

abline(h = 39.5, col = "red")



###################################################################################################################

#The code used for comparing the Recall of individual models
Recall <- c(0.31 , 0.27 , 0.498, 0.76 , 0.67 , 0.46, 0.203, 0.155)*100
modelNames <- c('NB', 'NB Ens', 'NN', 'LogReg', 'DeciTree', 'KNN', 'RF(Aqib)', 'Rf(Chris)')

# Find the indices of the desired model names
orange_indices <- which(modelNames %in% c('NN', 'LogReg', 'DeciTree'))

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_indices] <- "orange"

# Precision bar plot
barplot(Recall,
        names.arg = modelNames,
        xlab = "Model names",
        ylab = "Recall",
        main = "Comparing Recall of each model",
        col = bar_colors,
        ylim = c(10, 80),
        beside = TRUE,
        xpd = FALSE)

abline(h = 48.8, col = "red")




###################################################################################################################

#The code used for comparing the F-Score of individual models
F1Scores <- c(34.1, 31.9, 42.7, 50.6, 48.9, 40.9, 27.4, 22.3)
modelNames <- c('NB', 'NB Ens', 'NN', 'LogReg', 'DeciTree', 'KNN', 'RF(Aqib)', 'Rf(Chris)')

# Find the indices of the desired model names
orange_indices <- which(modelNames %in% c( 'LogReg', 'DeciTree'))

# Create a color vector with the same length as modelNames
bar_colors <- rep("#89b3a7", length(modelNames))
bar_colors[orange_indices] <- "orange"

# Precision bar plot
barplot(F1Scores,
        names.arg = modelNames,
        xlab = "Model names",
        ylab = "F1-Score %",
        main = "Comparing F1-Score of each model",
        col = bar_colors,
        ylim = c(10, 55),
        beside = TRUE,
        xpd = FALSE)

abline(h = 47, col = "red")


###################################################################################################################



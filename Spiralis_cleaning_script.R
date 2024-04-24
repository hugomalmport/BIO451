#Load the library
library(readxl)
library(dplyr)

#Clear the workspace
rm(list=ls())

#Read the excel file from its path
##Make sure to change path depending on where you saved it!
###Change sheet depending on which sheet is going to be used!!
data_spiralis <- read_excel("C:\\Users\\hugom\\OneDrive\\Skrivbord\\Skolrelaterat\\BIO451\\Spiralis_data.xlsx", sheet = "Data")

#Inspect the file
summary(data_spiralis)
head(data_spiralis)
str(data_spiralis)

#Remove rows with NA values
##Now it'll remove everything cause we have NA values in all rows
data_spiralis <- na.omit(data_spiralis)

#Inspect the file again
summary(data_spiralis)
head(data_spiralis)

#Separate data depending on column
##Use 'ungroup()' if not necessary
grouped_data <- data_spiralis %>%
  group_by(chlorophyll)

#Check the grouped data
summary(grouped_data)

#Remove columns with NA values
data_spiralis <- data_spiralis[, colSums(is.na(data_spiralis)) == 0]

#Now we removed columns with NA values
##Check the data frame
print(data_spiralis)


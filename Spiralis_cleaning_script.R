#Load the library
library(readxl)
library(dplyr)

#Clear the workspace
rm(list=ls())

#Read the excel file from its path
##Make sure to change path depending on where you saved it!
###Change sheet depending on which sheet is going to be used!! - so either "Data" or "IC" (Initial chlorophyll)!!
data_spiralis <- read_excel("C:\\Users\\hugom\\OneDrive\\Skrivbord\\Skolrelaterat\\BIO451\\Spiralis_data.xlsx", sheet = "Data")

#Inspect the file
summary(data_spiralis)
head(data_spiralis)
str(data_spiralis)

#Remove columns with NA values
#Now we remove columns with NA values
data_spiralis <- data_spiralis[, colSums(is.na(data_spiralis)) == 0]

#Remove rows with NA values
##Now it'll remove everything cause we have NA values in all rows
data_spiralis <- na.omit(data_spiralis)

#Inspect the file again
summary(data_spiralis)
head(data_spiralis)

#Separate and organize data depending on column
    ##Here I organize after the tank_id - 1-10
###Use 'ungroup()' if not necessary after - or remove
organized_data <- data_spiralis %>%
  group_by(tank_id) %>%
  arrange(tank_id)

#Check the grouped and organized data
summary(organized_data)

#Now we removed columns with NA values
##Check the data frame
print(data_spiralis)

#Decide the file name and path
file_path <- "C:\\Users\\hugom\\OneDrive\\Skrivbord\\Skolrelaterat\\BIO451\\cleaned_data.csv"

#Write the dataframe to a CSV file 
write.csv(data_spiralis, file = file_path, row.names = FALSE)


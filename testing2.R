# Load necessary libraries
library(dplyr)
library(tidyr)
library(mice)

data <- tbc
# Simplify the dataset by filtering for the first visit for each patient
# Assuming 'data' is your original dataset
data <- data %>%
  filter(first == TRUE)

data <- data %>%
  mutate(sex = if_else(sex == 1, 0, 1))

# Retain only the relevant features
data<- subset(data, select = -c(id,occ,nocc,first,typ,age,ao))

# Generate X_complete, x_incomplete,y_complete and y _incomplete
x_complete <- data %>% filter(!is.na(bmi.z) & !is.na(hgt.z))
x_incomplete <- data %>% filter(is.na(hgt.z) & is.na(bmi.z))
y_complete <- x_complete$sex
y_incomplete<-x_incomplete$sex
x_complete<-subset(x_complete,select=-c(sex))
x_incomplete<-subset(x_incomplete,select=-c(sex))
#turn df into vectors
x_complete <- as.vector(x_complete)
x_incomplete <- as.vector(x_incomplete)
y_complete <- as.vector(y_complete)
y_incomplete <- as.vector(y_incomplete)





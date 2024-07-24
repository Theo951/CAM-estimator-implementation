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
### Divide x and y
y_complete <- x_complete$sex
y_incomplete<-x_incomplete$sex
####### set x without y
x_complete<-subset(x_complete,select=-c(sex))
x_incomplete<-subset(x_incomplete,select=-c(sex))
######

####turn df into vectors
x_complete_v <- as.vector(x_complete)
x_incomplete_v <- as.vector(x_incomplete)
y_complete_v <- as.vector(y_complete)
y_incomplete_v <- as.vector(y_incomplete)
###

theta_0 <- mean(x_complete$bmi.z) ## average of the bmi, the complete case estimator
cam_u_simple <- x_incomplete$wgt.z
model <- lm(bmi.z ~ wgt.z * sex, data = x_complete)
summary(model)
fitted_values <-fitted(model)
phi_0_m<- mean((fitted_values))
phi_m <-predict(model, newdata = x_incomplete )
phi_m_avg <- mean(phi_m)


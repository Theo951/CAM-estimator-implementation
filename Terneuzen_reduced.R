library(dplyr)
library(mice)
data <- tbc
# Simplify the dataset by filtering for the first visit for each patient
data <- data %>%
  filter(first == TRUE)

# Retain only the relevant features
df_subset <- subset(data, select = -c(id,occ,nocc,first,typ,age,ao))

# Check the number of rows with missing values for height_z and bmi_z
#####
#105 cases in Am for m = (1, 0, 1)T , where only Y (sex) and X(2) (weight) are observed.
#####
incomplete_case <- df_subset %>%
  filter(is.na(hgt.z) & is.na(bmi.z))

#We have 201 complete cases in A0

complete_case<- df_subset [complete.cases(df_subset),]

#Statistics of the 2 datasets
sum(is.na(complete_case))
summary(complete_case)
sum(is.na(incomplete_case))
summary((incomplete_case))

u_statistics<- function(x){
  #mean(df_subset$wgt.z,na.rm = TRUE)
  mean(x$bmi.z,na.rm = TRUE) ############ hgt
}

cam_estimator <- function(complete, incomplete) {
  # Estimate from complete cases
  theta_0 <- u_statistics(complete)
  
  # Define phi functions
  phi_m_simple <- incomplete_case$wgt.z
  lm_model <- lm(bmi.z ~ wgt.z * sex, data = complete )
  # Get fitted values for complete cases
  fitted_values <- fitted(lm_model)
  phi_0_m <- mean(fitted_values)  # This is phi_0,m
  print(summary(lm_model))
  phi_m_complex <- predict(lm_model, newdata = incomplete)
  print("phi_m_complex")
  print(phi_m_complex)
  
  # Average predictions on incomplete cases
  
  phi_hat_simple <- mean(phi_m_simple, na.rm=TRUE)
  phi_hat_complex <- mean(phi_m_complex,na.rm=TRUE)
  print("phi_hat_complex")
  print( phi_hat_complex)
  
  # Combine estimates
  #cam_simple <- theta_0 - phi_hat_simple #mean(phi_hat_simple)
  #cam_complex <- theta_0 - (phi_hat_complex) #mean(phi_hat_complex)
  # Combine estimates using phi_0_m
  cam_simple <- theta_0 + (phi_0_m - phi_hat_simple)
  cam_complex <- theta_0 + (phi_0_m - phi_hat_complex)
  print("cam_simple")
  print(cam_simple)
  print("cam_complex")
  print(cam_complex)
  
  return(list(cam_simple = cam_simple, cam_complex = cam_complex))
}
# Estimate from complete cases
theta_0 <- u_statistics(complete_case)
cam_results <- cam_estimator(complete_case, incomplete_case)
# Print results
cat("Complete-case estimate: ", u_statistics(complete_case), "\n")
cat("CAM estimate (simple): ", cam_results$cam_simple, "\n")
cat("CAM estimate (complex): ", cam_results$cam_complex, "\n")

# intervals
conf_interval <- function(estimator, data) {
  n <- nrow(data)
  se <- sd(data$bmi.z, na.rm = TRUE) / sqrt(n)
  error <- qnorm(0.975) * se
  lower <- estimator - error
  upper <- estimator + error
  print(c(lower, upper))
  
  return(c(lower, upper))
}

# Calculate confidence intervals
ci_complete <- conf_interval(theta_0, complete_case)
ci_cam_simple <- conf_interval(cam_results$cam_simple, complete_case)
ci_cam_complex <- conf_interval(cam_results$cam_complex, complete_case)

# Calculate interval widths
width_complete <- ci_complete[2] - ci_complete[1]
print(width_complete)
width_cam_simple <- ci_cam_simple[2] - ci_cam_simple[1]
print(width_cam_simple)
width_cam_complex <- ci_cam_complex[2] - ci_cam_complex[1]






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
conf_interval <- function(estimator, data) { ###HOW TO CALCULATE THE CONFIDENCE INTERVAL OF A PREDICTION?
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


################################################################
estimate_variance_components_subsample <- function(data, B) {
  n <- nrow(data)
  subsample_size <- floor(n / B)
  
  psi_U_list <- numeric(B)
  print("psi_u_list")
  print(psi_U_list)
  Omega_U_list <- matrix(0, nrow = B, ncol = 2)
  Lambda_U_list <- matrix(0, nrow = B, ncol = 2)
  
  for (b in 1:B) {
    subsample <- data[sample(1:n, subsample_size), ]
    
    
    psi_U_list[b] <- var(subsample$bmi.z, na.rm = TRUE)
    
    Omega_U_list[b, 1] <- var(subsample$bmi.z, na.rm = TRUE)
    Omega_U_list[b, 2] <- var(subsample$hgt.z, na.rm = TRUE)
    Lambda_U_list[b, 1] <- (cov(subsample$bmi.z, subsample$hgt.z)) #na.rm=TRUE
    Lambda_U_list[b, 2] <- Lambda_U_list[b, 1]
  }
  print("Omega_U_list")
  print(Omega_U_list)
  print("Lambda_U_list")
  print(Lambda_U_list)
  print(paste("Subsample", b))
  print(subsample)
  psi_U <- mean(psi_U_list)
  Omega_U <- apply(Omega_U_list, 2, mean)
  Lambda_U <- apply(Lambda_U_list, 2, mean)
  
  return(list(psi_U = psi_U, Omega_U = Omega_U, Lambda_U = Lambda_U))
}

conf_interval_subsample <- function(estimator, data, var_components) {
  n <- nrow(data)
  psi_U <- var_components$psi_U
  Omega_U <- var_components$Omega_U
  Lambda_U <- var_components$Lambda_U
  
  # Estimate the variance
  var_est <- psi_U - Omega_U %*% Lambda_U %*% t(Omega_U)
  
  # Standard error
  se <- sqrt(var_est / n)
  
  # Margin of error
  error <- qnorm(0.975) * se
  
  # Confidence interval
  lower <- estimator - error
  upper <- estimator + error
  
  return(c(lower, upper))
}

# Example data
#data <- complete_case

# Number of subsamples
B <- 105

# Estimate variance components using subsampling
var_components <- estimate_variance_components_subsample(complete_case, B)

# Example estimator (mean of bmi.z)
estimator <- mean(complete_case$bmi.z, na.rm = TRUE)

# Calculate the confidence interval using subsamples
ci <- conf_interval_subsample(estimator, complete_case, var_components)

# Print the confidence interval
print(ci)

###################################################
# Function to calculate the confidence interval
conf_interval_theorem3 <- function(estimator, data, M, psi_U, Omega_U, Lambda_U) {
  n <- nrow(data)
  # Estimate the variance
  var_est <- psi_U - Omega_U %*% Lambda_U %*% t(Omega_U)
  
  # Standard error
  se <- sqrt(var_est / n)
  
  # Margin of error
  error <- qnorm(0.975) * se
  
  # Confidence interval
  lower <- estimator - error
  upper <- estimator + error
  
  return(c(lower, upper))
}

psi_U <- var(complete_case$bmi.z, na.rm = TRUE)  # This is a placeholder, calculate appropriately
Omega_U <- matrix(1, nrow=1, ncol=1)  # Placeholder, calculate appropriately
Lambda_U <- matrix(1, nrow=1, ncol=1)  # Placeholder, calculate appropriately



n <- nrow(data)
subsample_size <- floor(n / B)
for (n in 1:n){                                             #### SUMBSAMPLE ONLY RETURNS 1 ######
  for (b in 1:B) {
  subsample <- data[sample(1:n, subsample_size), ]}
}



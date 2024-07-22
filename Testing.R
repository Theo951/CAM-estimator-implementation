# Load necessary libraries
library(dplyr)
library(tidyr)
library(mice)
# Define the function to calculate confidence intervals using variance components
conf_interval <- function(estimator, variance_components, n) {
  psi_U <- variance_components$psi_U
  Omega_U <- matrix(variance_components$Omega_U, nrow = 2)  # Ensure Omega_U is a matrix
  Lambda_U <- matrix(variance_components$Lambda_U, nrow = 2)  # Ensure Lambda_U is a matrix
  
  asymptotic_variance <- psi_U - t(Omega_U) %*% solve(Lambda_U) %*% Omega_U
  se <- sqrt(asymptotic_variance) / sqrt(n)
  error <- qnorm(0.975) * se
  lower <- estimator - error
  upper <- estimator + error
  return(c(lower, upper))
}

# Function to calculate U-statistic variance components
# Function to calculate U-statistic variance components
estimate_variance_components_subsample <- function(data, B) {
  print("here")
  n <- nrow(data)
  subsample_size <- floor(n / B)
  
  
  psi_U_list <- numeric(B)
  Omega_U_list <- matrix(0, nrow = B, ncol = 2)
  Lambda_U_list <- matrix(0, nrow = B, ncol = 2)
 
  
  for (b in 1:B) {
    subsample <- data[sample(1:n, subsample_size), ]
    psi_U_list[b] <- var(subsample$bmi.z, na.rm = TRUE)
    
    Omega_U_list[b, 1] <- var(subsample$bmi.z, na.rm = TRUE)
    print(Omega_U_list)
    Omega_U_list[b, 2] <- var(subsample$hgt.z, na.rm = TRUE)
    
    Lambda_U_list[b, 1] <- (cov(subsample$bmi.z, subsample$hgt.z, na.rm = TRUE))
    Lambda_U_list[b, 2] <- Lambda_U_list[b, 1]
  }
  
  
  psi_U <- mean(psi_U_list)
  Omega_U <- apply(Omega_U_list, 2, mean)
  Lambda_U <- apply(Lambda_U_list, 2, mean)
  # Print the results to debug
  print(list(psi_U = psi_U, Omega_U = Omega_U, Lambda_U = Lambda_U))
  return(list(psi_U = psi_U, Omega_U = Omega_U, Lambda_U = Lambda_U))
}

# Load your data

data <- tbc
# Simplify the dataset by filtering for the first visit for each patient
# Assuming 'data' is your original dataset
data <- data %>%
  filter(first == TRUE)

data <- data %>%
  mutate(sex = if_else(sex == 1, 0, 1))

# Retain only the relevant features
data<- subset(data, select = -c(id,occ,nocc,first,typ,age,ao))


# Ensure the data is in the correct format
# For demonstration, let's assume the data has been pre-processed

# Calculate theta_0 using the complete cases
complete_case <- data %>% filter(!is.na(bmi.z) & !is.na(hgt.z))
incomplete_case <- data %>%
  filter(is.na(hgt.z) & is.na(bmi.z))

theta_0 <- mean(complete_case$bmi.z, na.rm = TRUE)

# Calculate phi_m_simple and phi_m_complex
phi_m_simple <- complete_case$wgt.z
lm_model <- lm(bmi.z ~ wgt.z * sex, data = complete_case )
fitted_values <- fitted(lm_model)
phi_0_m <- mean(fitted_values)
phi_m_complex <- predict(lm_model, newdata = incomplete_case)


# Calculate confidence intervals
ci_complete <- conf_interval(theta_0, complete_case)
ci_cam_simple <- conf_interval(mean(phi_m_simple, na.rm = TRUE), complete_case)
ci_cam_complex <- conf_interval(mean(phi_m_complex, na.rm = TRUE), complete_case)

# Calculate interval widths
width_complete <- ci_complete[2] - ci_complete[1]
width_cam_simple <- ci_cam_simple[2] - ci_cam_simple[1]
width_cam_complex <- ci_cam_complex[2] - ci_cam_complex[1]

# Summarize results in a table
results <- data.frame(
  Method = c("Complete-case", "CAM: φm(Zm) = X(2)", "CAM: φm(Zm) linear"),
  `Point est.` = c(theta_0, mean(phi_m_simple, na.rm = TRUE), mean(phi_m_complex, na.rm = TRUE)),
  `95% CI` = c(paste0("(", round(ci_complete[1], 2), ", ", round(ci_complete[2], 2), ")"),
               paste0("(", round(ci_cam_simple[1], 2), ", ", round(ci_cam_simple[2], 2), ")"),
               paste0("(", round(ci_cam_complex[1], 2), ", ", round(ci_cam_complex[2], 2), ")")),
  `CI width` = c(round(width_complete, 2), round(width_cam_simple, 2), round(width_cam_complex, 2))
)

# Print the results
print(results)


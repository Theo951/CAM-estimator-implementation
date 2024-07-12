# Sample data
set.seed(123)
n <- 1000
data <- data.frame(
  height = rnorm(n, 170, 10),
  weight = rnorm(n, 65, 15),
  bmi = rnorm(n, 22, 3),
  sex = sample(c("M", "F"), n, replace = TRUE)
)

# Introduce missing values
data$weight[sample(1:n, n/2)] <- NA

# Complete cases
complete_cases <- data[complete.cases(data),]

# Incomplete cases
incomplete_cases <- data[!complete.cases(data),]

# Define U-Statistics functions
u_statistic <- function(data) {
  n <- nrow(data)
  mean(data$bmi)
}

cam_estimator <- function(complete, incomplete) {
  # Estimate from complete cases
  theta_0 <- u_statistic(complete)
  
  # Define phi functions
  phi_m_simple <- incomplete$weight
  phi_m_complex <- lm(bmi ~ height * sex, data = complete)$fitted.values
  
  # Average predictions on incomplete cases
  phi_hat_simple <- mean(phi_m_simple, na.rm = TRUE)
  phi_hat_complex <- mean(predict(lm(bmi ~ height * sex, 
                                     data = complete), newdata = incomplete), na.rm = TRUE)
  
  # Combine estimates
  cam_simple <- theta_0 - mean(phi_hat_simple)
  cam_complex <- theta_0 - mean(phi_hat_complex)
  
  return(list(cam_simple = cam_simple, cam_complex = cam_complex))
}

# Apply CAM estimator
cam_results <- cam_estimator(complete_cases, incomplete_cases)
cam_results
# Complete-case estimator
complete_case_estimate <- u_statistic(complete_cases)

# Define U-Statistics functions
u_statistic <- function(data) {
  mean(data$bmi, na.rm = TRUE)  # Ensure to handle NAs properly
}

cam_estimator <- function(complete, incomplete) {
  # Estimate from complete cases
  theta_0 <- u_statistic(complete)
  
  # Define phi functions
  phi_m_simple <- incomplete$weight
  
  # Check for NA values in phi_m_simple
  if (all(is.na(phi_m_simple))) {
    cat("All values in phi_m_simple are NA.\n")
    return(list(cam_simple = NaN, cam_complex = NaN))
  }
  
  # Calculate mean of phi_m_simple
  phi_hat_simple <- mean(phi_m_simple, na.rm = TRUE)
  
  # If phi_hat_simple is NaN, handle the situation
  if (is.nan(phi_hat_simple)) {
    cat("phi_hat_simple is NaN. Possibly due to all values being NA.\n")
    return(list(cam_simple = NaN, cam_complex = NaN))
  }
  
  # Fit linear model for phi_m_complex
  lm_model <- lm(bmi ~ height * sex, data = complete)
  phi_m_complex <- predict(lm_model, newdata = incomplete)
  
  # Calculate mean of phi_m_complex
  phi_hat_complex <- mean(phi_m_complex, na.rm = TRUE)
  
  # If phi_hat_complex is NaN, handle the situation
  if (is.nan(phi_hat_complex)) {
    cat("phi_hat_complex is NaN.\n")
    return(list(cam_simple = theta_0 - phi_hat_simple, cam_complex = NaN))
  }
  
  # Combine estimates
  cam_simple <- theta_0 - phi_hat_simple
  cam_complex <- theta_0 - phi_hat_complex
  
  return(list(cam_simple = cam_simple, cam_complex = cam_complex))
}

# Apply CAM estimator
cam_results <- cam_estimator(complete_cases, incomplete_cases)

# Print results
cat("Complete-case estimate: ", u_statistic(complete_cases), "\n")
cat("CAM estimate (simple): ", cam_results$cam_simple, "\n")
cat("CAM estimate (complex): ", cam_results$cam_complex, "\n")


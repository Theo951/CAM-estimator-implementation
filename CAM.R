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

# Print results
cat("Complete-case estimate: ", complete_case_estimate, "\n")
cat("CAM estimate (simple): ", cam_results$cam_simple, "\n")
cat("CAM estimate (complex): ", cam_results$cam_complex, "\n")

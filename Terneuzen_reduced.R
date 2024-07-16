library(dplyr)
library(mice)
data <- tbc
# subselect data Using subset function, authors decided to only use 4 columns
#in order to simplify, I've decided to use their same attributes
df_subset <- subset(data, select = -c(id,occ,nocc,first,typ,age,ao))
print("After dropping columns with subset function:")
print(df_subset)


#Generate a complete case scenario as per paper defined
complete_case <- df_subset [complete.cases(df_subset),]
#Generate an incomplete case dataset
incomplete_case <- df_subset[!complete.cases(df_subset),] 

#####
#105 cases in Am for m = (1, 0, 1)T , where only Y (sex) and X(2) (weight) are observed.
#####
subset_data <- df_subset %>%
  #filter(is.na(bmi.z)&!is.na(hgt.z))
  filter(is.na(hgt.z) & is.na(bmi.z))
  

set.seed(123)
# Number of observations to extract
num_samples <- 105
# Randomly sample row indices
sample_indices_incomplete <- sample(1:nrow(subset_data), num_samples)
# Extract the subset
incomplete_case <- subset_data[sample_indices_incomplete, ]

#We have 201 complete cases in A0
n_samples <- 201
sample_indices_complete <- sample(1:nrow(complete_case),n_samples)
complete_case <- complete_case[sample_indices_complete,]



#Statistics of the 2 datasets
sum(is.na(complete_case))
summary(complete_case)
sum(is.na(incomplete_case))
summary((incomplete_case))

u_statistics<- function(x){
  #mean(df_subset$wgt.z,na.rm = TRUE)
  mean(x$bmi.z,na.rm = TRUE)
}

cam_estimator <- function(complete, incomplete) {
  # Estimate from complete cases
  theta_0 <- u_statistics(complete)
  
  # Define phi functions
  phi_m_simple <- incomplete_case$wgt.z
  lm_model <- lm(bmi.z ~ wgt.z * sex, data = complete )
  print(summary(lm_model))
  phi_m_complex <- predict(lm_model, newdata = incomplete)
  print("phi_m_complex")
  print(phi_m_complex)
  
  # Average predictions on incomplete cases
  #phi_hat_simple <- mean(phi_m_simple, na.rm = TRUE)
  phi_hat_simple <- mean(phi_m_simple, na.rm=TRUE)
  #phi_hat_complex <- mean(predict(lm(bmi ~ height * sex, 
  #                                 data = complete), newdata = incomplete), na.rm = TRUE)
  phi_hat_complex <- mean(phi_m_complex,na.rm=TRUE)
  print("phi_hat_complex")
  print( phi_hat_complex)
  
  # Combine estimates
  cam_simple <- theta_0 - phi_hat_simple #mean(phi_hat_simple)
  cam_complex <- theta_0 - (phi_hat_complex) #mean(phi_hat_complex)
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
  return(c(lower, upper))
}

# Calculate confidence intervals
ci_complete <- conf_interval(theta_0, complete_case)
ci_cam_simple <- conf_interval(cam_results$cam_simple, complete_case)
ci_cam_complex <- conf_interval(cam_results$cam_complex, complete_case)

# Calculate interval widths
width_complete <- ci_complete[2] - ci_complete[1]
width_cam_simple <- ci_cam_simple[2] - ci_cam_simple[1]
width_cam_complex <- ci_cam_complex[2] - ci_cam_complex[1]







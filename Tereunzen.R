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

#Statistics of the 2 datasets
sum(is.na(complete_case))
summary(complete_case)
sum(is.na(incomplete_case))
summary((incomplete_case))

Y_complete<- complete_case$sex
X_complete <- complete_case %>% select(hgt.z, wgt.z,bmi.z)
#X_complete_mat<-as.matrix(X_complete)
X_complete <- simplify2array(X_complete)
#------------#
#X_complete <- array(c(X_complete), dim = c(4,2,3))
#------------#

# build estimators

## CAM ESTIMATOR ##
comp_est <- lapply(X_complete,mean) 
comp_est <- mean(X_complete)
##--------##

## PHI VALUES + AVERAGE PREDICTIONS ##
phi_m_simple<- incomplete_case$wgt.z
#phi_hat_simple <- lapply(phi_m_simple,mean) # o questo
phi_hat_simple <- mean(phi_m_simple, na.rm=TRUE) # o questo
lm_model <- lm(bmi.z ~ wgt.z * sex, data = complete_case )
summary(lm_model)
phi_m_complex <- predict(lm_model, newdata = incomplete_case)
phi_hat_complex <- lapply(phi_m_complex,mean)
##------##

cam_simple <- theta - phi_hat_simple
#TODO REVIEW THE DATASET IN ORDER TO FIND PATTERNS OF (1,0,1) WHERE ONLY SEX 
#AND WEIGHT ARE OBSERVED --> fixed the NA issue
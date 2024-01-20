# Loading the lavaan package for SEM analysis
library(lavaan)

# Our variables for clarity:
# EC - Empathic Concern, measures the emotional component of empathy
# PT - Perspective Taking, represents the cognitive aspect of empathy
# COS - Cosmopolitan Orientation, indicating a global and inclusive outlook
# Additional variables: Political Orientation (Pol_orien), Age, Gender, and Education Level (Edu_level)


# Specifying the SEM model:
# The model aims to explore how empathic concern and perspective taking (EC and PT)
# influence attitudes towards different groups, mediated by cosmopolitan orientation (COS).
model <- '
  # Direct effects: Predicting COS based on EC, PT, and demographic factors
  COS ~ a1*EC + a2*PT + Pol_orien + Age + Gender + Edu_level

  # Predicting attitudes towards Roma, Jews, and Muslims based on COS and demographics
  Roma ~ b1*COS + Pol_orien + Age + Gender + Edu_level
  Jews ~ b2*COS + Pol_orien + Age + Gender + Edu_level
  Muslim ~ b3*COS + Pol_orien + Age + Gender + Edu_level

  # Adding a direct path from Gender to EC based on modification index suggestions
  EC ~ c1*Gender

  # Indirect effects: Understanding how EC and PT impact attitudes via COS
  indirect_EC_Roma := a1 * b1
  indirect_PT_Roma := a2 * b1
  indirect_EC_Jews := a1 * b2
  indirect_PT_Jews := a2 * b2
  indirect_EC_Muslim := a1 * b3
  indirect_PT_Muslim := a2 * b3
  
  # Covariances: Exploring correlations between empathy dimensions and among attitudes
  EC ~~ PT
  Roma ~~ Muslim
  Roma ~~ Jews
  Muslim ~~ Jews
'

# Fitting the SEM model to the data
# Here we use Maximum Likelihood estimation with Full Information Maximum Likelihood for missing data
fit <- sem(model, data = mydata, estimator = "ML", missing = "FIML", fixed.x = FALSE)

# Bootstrapping for robustness of estimates
# 5000 bootstrap samples to ensure confidence in our parameter estimates
fit_bootstrap <- bootstrapLavaan(fit, R = 5000)

# Summarizing the results
# Including fit measures and standardized results for easier interpretation
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Detailed parameter estimates
# Bootstrapped confidence intervals (BCa) provide a more accurate estimate range
parameterEstimates(fit, boot.ci.type = "bca")

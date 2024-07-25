data <- c(231, 81, 318, 68, 161, 175, 112, 435, 283, 87, 360, 143, 54, 311, 170, 168, 176, 202, 176, 250)

#==============================================================
# Constructing Confidence Interval for the Mean (Mu) 
#==============================================================

mean_y = 44    # mean(data)   # Sample mean
sd_y = 6  # sd(data)    # Sample standard deviation
n_y = 12  # length(data) # Sample size
conf = 0.95          # Confidence level
alpha = 1 - conf;
alpha               # Significance level
margin_error_mean = qt((alpha / 2), n_y - 1, lower.tail = FALSE) * (sd_y / sqrt(n_y));
margin_error_mean              # Margin of error
CI_mean = cbind(mean_y - margin_error_mean, mean_y + margin_error_mean);
CI_mean              # Confidence interval

#==============================================================
# Constructing Confidence Interval for the Proportion (pi) 
#==============================================================

n = 400       # sample size
np = 240      # number of successes in n
p_hat = np / n;
p_hat     # sample proportion
conf = 0.99  # confidence level
alpha = 1 - conf;
alpha  # significance level
margin_error_prop = qnorm(1 - alpha / 2) * (sqrt(p_hat * (1 - p_hat)) / sqrt(n))
margin_error_prop  # calculating the margin of error
CI_prop = cbind(p_hat - margin_error_prop, p_hat + margin_error_prop);
CI_prop

#==============================================================
# Sample Size Calculation for Margin of Error in Proportion (PI)
#==============================================================

pilot_prop = 0.6      # proportion in pilot sample
desired_error_margin = 0.02   # desired margin of error, must always be in DECIMAL
alpha = 0.05 # significance level
new_sample_size_prop = ((qnorm(alpha / 2, lower.tail = FALSE) * (sqrt(pilot_prop * (1 - pilot_prop)) / desired_error_margin)))^2;
new_sample_size_prop

#==============================================================
# Sample Size Calculation with Margin of Error for Mu
#==============================================================

pilot_sd = 6 # sd(data)      # Standard deviation of the pilot sample
pilot_n = 12 # length(data)   # Size of the pilot sample
desired_error_margin = 2             # Desired margin of error, same unit as mean
alpha = 0.05
new_sample_size_mean = (qt((alpha / 2), pilot_n - 1, lower.tail = FALSE) * pilot_sd / desired_error_margin)^2;
new_sample_size_mean             # New sample size

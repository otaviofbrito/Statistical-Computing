############################################################
# Performing the test MANUALLY for population MEAN (Mu)
#############################################################

#===========================================
#  STATEMENT
#==========================================
# testing Mu=5, TWO-SIDED test #

sample_sd = 10    # sample standard deviation #
sample_mean = 134     # sample mean #
n = 40              # sample size #

test_value = 130       # tested value Mu0 #

tcalc = ((sample_mean - test_value) / (sample_sd / (n^0.5)))
tcalc # calculated T quantile #

# p-value of the test, CHOICE BASED ON H1

# A) Two-sided test
p_value_t = 2 * pt(abs(tcalc), df = n - 1, lower.tail = FALSE); p_value_t

# B) Left-sided test 
p_value_t = pt(tcalc, df = n - 1, lower.tail = TRUE); p_value_t

# C) Right-sided test
p_value_t = pt(tcalc, df = n - 1, lower.tail = FALSE); p_value_t

#==========================================================
# USING the t.test() FUNCTION for population MEAN
# Performs the t-Student test for one or two samples.
#==========================================================
sample = c(6.0, 7.3, 6.8, 7, 6.9, 7.3, 7.2, 7, 6.7, 6.6, 7.8, 7, 6.3, 7.3, 7.1)
t.test(sample, mu = 7.3, alternative = "less")

# use alternative="greater" when H1 has a greater sign (>)
# use alternative="less" when H1 has a less sign (<)
# use alternative="two.sided" when H1 has a different sign (!=)

################################################################################
#               Testing the PROPORTION (PI) of a population
#                Performing the test MANUALLY
################################################################################

n = 50
ns = 42
p0 <- 0.95 # Value to be tested Pi0 #

pe = ns / n; pe # sample proportion
zc = (pe - p0) / sqrt((pe * (1 - pe)) / n); zc

# p-value of the test, CHOICE BASED ON H1

# A) Two-sided test
p_value_z = 2 * pnorm(abs(zc), lower.tail = FALSE); p_value_z

# B) Left-sided test
p_value_z = pnorm(zc, lower.tail = TRUE); p_value_z

# C) Right-sided test
p_value_z = pnorm(zc, lower.tail = FALSE); p_value_z

#==========================================================
#               USING THE FUNCTION binom.test
#              Exact test for PROPORTION (PI)                            
#==========================================================
# input with number of successes (ns) and sample size (n)

n = 80
ns = 68
p0 <- 0.92 # Value to be tested (assumed) for Pi#

binom.test(ns, n, p = p0, alternative = "less")

# use alternative="greater" when H1 has a greater sign (>)
# use alternative="less" when H1 has a less sign (<)
# use alternative="two.sided" when H1 has a different sign (!=)

#=====================================================================

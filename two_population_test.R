#==================================================================
# var.test() Performs the F-test for variance ratio
#==================================================================

##Can we state that A and B have equal variances?

#H0: sig2A/sig2B = 1
#H1: sig2A/sig2B != 1

sampleA = c(120, 119, 89, 103, 122, 108, 105, 102, 122, 132)
sampleB = c(115, 110, 90, 110, 105, 113, 98, 104, 125, 111)
var(sampleA)  #calculating sample variance
var(sampleB) 

var.test(sampleA, sampleB, alternative="two.sided")

## Pay attention to H1.
# use alternative="two.sided" when H1 has a different sign (!=)
# use alternative="greater" when H1 has a greater sign (>)
# use alternative="less" when H1 has a less sign (<)

###################################################################
#==================================================================
#              T-TEST TO COMPARE TWO MEANS (Mu1 - Mu2)
#==================================================================        
# t.test() Performs the t-Student test for two INDEPENDENT samples
## t-test for comparing two means with equal variances
######################################################################

#  H0 : MuA - MuB = 0
#  H1 : MuA - MuB != 0

sampleA = c(3, 5, 7, 9, 8, 9, 7, 4, 9, 9)
sampleB = c(8, 5, 9, 3, 2, 6, 4, 5, 2, 5)

# since variances are equal, use "var.equal = TRUE" ##

t.test(sampleA, sampleB, var.equal = TRUE, alternative="two.sided")

#NOTE: When the variances of the two populations are considered different,
#use: var.equal = FALSE, as an argument in the t.test function.

# t.test(sampleA, sampleB, var.equal = FALSE, alternative="two.sided")

###################################################################
# t.test() Performs the t-Student test for two DEPENDENT samples
#### PAIRED t-test (when the observations are on the same sample element)
######################################################################
# H0 : µDif = 0
# H1 : µDif != 0

a = c(170, 154, 162, 188, 155, 145, 167, 154, 177, 148, 160, 150)
b = c(165, 152, 164, 167, 154, 142, 162, 152, 165, 151, 153, 152)

t.test(a, b, paired=TRUE, alternative="less")  #paired=TRUE, indicates that the samples are dependent.

# Pay attention to options: alternative="greater", "less", "two.sided"
#
#==================================================================
#  Test to compare two proportions (Pi1-Pi2)
#==================================================================
n1 = 160
ns1 = 150 # number of successes in the sample from pop_1

n2 = 140
ns2 = 118 # number of successes in the sample from pop_2

#Ha: Pi2 < Pi1

prop.test(c(ns2, ns1), c(n2, n1), alternative="less")

#Ha: Pi1 > Pi2
prop.test(c(ns1, ns2), c(n1, n2), alternative="greater")

## Pay attention to H1.
# use alternative="two.sided" when H1 has a different sign (!=)
# use alternative="greater" when H1 has a greater sign (>)
# use alternative="less" when H1 has a less sign (<)

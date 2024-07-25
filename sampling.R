library(RcmdrMisc)

# Path to the data file
file_path <- "./datasets/sampling.xlsx";
Dataset <- readXL(file_path,
                  rownames=FALSE, header=TRUE,
                  na="", sheet="Plan1", stringsAsFactors=TRUE)

# Extracting relevant columns as factors or numeric vectors
learned <- factor(Dataset$Learn)
age <- Dataset$Age
id <- Dataset$ID
performance <- Dataset$Area

# Summary of the dataset
summary(Dataset)

n <- 100 # Sample size
N <- length(learned) # Population size

#=============SIMPLE RANDOM SAMPLE
randomSample <- sample(age, n)
mean(randomSample) #MEAN OF THE SIMPLE RANDOM SAMPLE

#============SYSTEMATIC SAMPLE
jump <- round(N/n)
first_element <- sample(1:jump, 1)
systematicSample <- c()
index <- first_element 
for(i in 1:n){
  systematicSample[i] <- age[index]
  index <- index + jump
}
mean(systematicSample) #MEAN OF THE SYSTEMATIC SAMPLE

#===========STRATIFIED SAMPLE

subsetA <- subset(Dataset, Area == 'A') #STRATUM A
subsetP <- subset(Dataset, Area == 'P') #STRATUM P
subsetO <- subset(Dataset, Area == 'O') #STRATUM O

#Proportional sampling

strata <- summary(performance)
proportionalSample <- round((strata/N)*n)

sampleA <- sample(subsetA$Age, proportionalSample[1])
sampleO <- sample(subsetO$Age, proportionalSample[2])
sampleP <- sample(subsetP$Age, proportionalSample[3])

stratifiedSample <- c(sampleA, sampleP, sampleO)

meanEST <- mean(stratifiedSample) #MEAN OF THE STRATIFIED SAMPLE

#ESTIMATION ERROR
err <- abs(meanEST - mean(age))

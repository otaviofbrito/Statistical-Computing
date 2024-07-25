library(RcmdrMisc)

file_path <- "./datasets/data_histogram.xlsx"
Dataset <- readXL(file_path,
                  rownames=FALSE, header=TRUE,
                  na="", sheet="Plan1", stringsAsFactors=TRUE)

data <- Dataset$glucose_level # STUDIED VARIABLE

table <- function(data, decimal_places){
  A <- max(data) - min(data)
  n <- length(data) 
  if(n <= 100){
    k <- ceiling(sqrt(n))
  } else {
    k <- ceiling(5 * log10(n))
  }
  C <- round(A / (k - 1), decimal_places)
  LI <- c(rep(0, (k + 1)))
  mean_val <- c(NA)
  LI[1] <- round(min(data) - (C) / 2, decimal_places)
  for(i in 2:(k + 1)){
    LI[i] <- round(LI[i - 1] + C, decimal_places)
    mean_val[i - 1] <- mean(c(LI[i], LI[i - 1]))
  }
  limits <- LI
  TDF <- hist(data, breaks = limits, plot = FALSE, right = FALSE)
  table <- matrix(c(rep(6 * k)), k, 6)

  for(i in 1:k){
    table[i, 1] <- round(LI[i], decimal_places)
    table[i, 2] <- round(LI[i + 1], decimal_places)
    table[i, 3] <- round(mean_val[i], decimal_places)
    table[i, 4] <- (TDF$counts[i])
    table[i, 5] <- round(((TDF$counts[i]) / n), 5)
    table[i, 6] <- round((100 * TDF$counts[i]) / n, 3)
  }

  colnames(table) <- c("LI", "LS", "X", "Fa", "Fr", "Fp")

  return(table)
}

# Histogram

breaks <- function(data, decimal_places){
  A <- max(data) - min(data)
  n <- length(data) 
  if(n <= 100){
    k <- ceiling(sqrt(n))
  } else {
    k <- ceiling(5 * log10(n))
  }
  C <- round(A / (k - 1), decimal_places)

  LI <- c(rep(0, (k + 1)))
  LI[1] <- round(min(data) - (C) / 2, decimal_places)
  for(i in 2:(k + 1)){
    LI[i] <- round(LI[i - 1] + C, decimal_places)
  }
  return(LI)
}

# BUILDING FREQUENCY DISTRIBUTION TABLE
table(data, 2)
limits <- breaks(data, 3)

# Building the Frequency Distribution Table information

TDF <- hist(data, breaks = limits, plot = FALSE, right = FALSE)
print(TDF)

hist(data, label = FALSE, main = "",
     xlab = expression(paste("Concentration (", mu*g/m^3 ,")")),  # CHANGE DESCRIPTION AND UNIT
     ylab = "Absolute frequency", 
     ylim = c(0, (max(TDF$counts) + 1)),
     breaks = limits, axes = FALSE, right = FALSE)
axis(1, at = limits, pos = c(0, 0))
axis(2, at = c(seq(0:(max(TDF$counts) + 1)) - 1))

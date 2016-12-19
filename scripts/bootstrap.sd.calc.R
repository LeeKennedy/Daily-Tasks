## READ ME ##
#Takes a cut & paste set of data and calculates the standard deviation and 95% confidence interval for the standard deviation


# Clean environment ---------
rm(list=ls())

# Packages ------------------
library(dplyr)
library(ggplot2)


# Data In -------------------
if((Sys.info()[1] == "Darwin") == TRUE){
data1 <- read.delim(pipe('pbpaste'), header=FALSE)
} else {
data1 <- read.delim('clipboard', header = FALSE, as.is = TRUE)
}

bs <- rep(0,10000)

N <- nrow(data1)
     
# Run Bootstrap routine -----

for(i in 1:10000) {
                y <- sample(data1$V1, N, replace=T)
                bs[i] = sd(y)
}

# Summary data --------------
n <- 3
mean_bs <- mean(bs)
sd_bs <- sd(bs)
Upper_bs <- mean_bs + 2*sd_bs
Lower_bs <- mean_bs - 2*sd_bs
bootstrap <- data.frame(round(Lower_bs,n), round(mean_bs,n),  round(Upper_bs, n))
colnames(bootstrap) <- c("Lower 95% sd", "Std Dev", "Upper 95% sd")

# Display -------------------
bootstrap

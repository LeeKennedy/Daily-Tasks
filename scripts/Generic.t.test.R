# Clear Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(dplyr)

# Input data (Two columns, no row names) ---------------------------------
data.in <- read.csv("Book1.csv", header=TRUE)

row_1 <- colnames(data.in)[1]
colnames(data.in)[1] <- "A"
row_2 <- colnames(data.in)[2]
colnames(data.in)[2] <- "B"

# Functions --------------------------------------------------------------
cohens_d <- function(x, y) {
        lx <- length(x)- 1
        ly <- length(y)- 1
        md  <- abs(mean(x) - mean(y))
        csd <- lx * var(x) + ly * var(y)
        csd <- csd/(lx + ly)
        csd <- sqrt(csd)
        
        cd  <- md/csd   ## cohen's d
}


# Extract data into individual sets --------------------------------------
t1 <- data.in$A
t2 <- data.in$B

# Boxplot to see spread --------------------------------------------------

par(mfrow=c(1,1))

boxplot(t1, t2)
summary(data.in)

# Perform t-test - Nominate PAIRED or UNPAIRED data ----------------------
t.test(t2, t1, paired = FALSE)

# Cohen's d Test ---------------------------------------------------------
aa <- cohens_d(data.in$A, data.in$B)
round(aa,2)

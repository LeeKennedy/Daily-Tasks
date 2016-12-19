# TOST Equivalence Test
#
# This function calculates whether two sets of data are practically equivalent.
# A     - First data set.
# B     - Second data set.
# An acceptable difference, epsilon, must be estimated.

# Clean Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(equivalence)
library(readr)

# Function ---------------------------------------------------------------
TOST <- function(A, B, E, b = FALSE){

if(b == TRUE){
        boxplot(data.in$A, data.in$B)
}

summary(data.in)

tost_xy <- tost(A, B, epsilon=E, conf.level = 0.95, var.equal = TRUE)
tost_xy
}

# Input data -------------------------------------------------------------
data.in <- read_csv("Alcohol.csv")
row_1 <- colnames(data.in)[1]
colnames(data.in)[1] <- "A"
row_2 <- colnames(data.in)[2]
colnames(data.in)[2] <- "B"

# Set tolerable difference, epsilon --------------------------------------
epsilon <- 0.002

# Include TRUE if boxplot required ---------------------------------------
box_flag <- TRUE

# Test TOST --------------------------------------------------------------

tost_run <- TOST(data.in$A, data.in$B, epsilon, box_flag)

tost_run
# The Ho of the test is that the data sets ARE different by more than epsilon.
# A result of "Not Rejected" means that the data sets are different.
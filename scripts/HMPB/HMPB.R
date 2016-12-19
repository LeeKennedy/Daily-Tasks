# Clean environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(dplyr)
library(readr)

# Data input and cleaning ------------------------------------------------
data.in <- read_csv("8910296.csv")
colnames(data.in)[1] <- "SAMPLE_NUMBER"

data.in_2 <- select(data.in, REPORTED_NAME, ENTRY)
                   
data.in_2$ENTRY <- as.numeric(data.in_2$ENTRY)
ten <- c("Arsenic", "Antimony", "Bismuth", "Cadmium", "Copper", "Lead", "Mercury", "Molybdenum", "Silver", "Tin")

data_set <- data.in_2 %>%
        filter (REPORTED_NAME %in% ten)

# Convert negative values to zero ----------------------------------------
data_set$ENTRY[data_set$ENTRY<0] <- 0

data_set

# Heavy metals summary ---------------------------------------------------

Limit_Test_Sum <- sum(data_set$ENTRY, na.rm = TRUE)
Limit_Test_Sum
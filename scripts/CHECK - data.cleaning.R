# Clean environment ------------------------------------------------------
rm(list=ls())

# Packages ----------------------------------------------------------------
library (dplyr)
library (tidyr)
library (ggplot2)
library (readxl)
library (lubridate)
library (readr)

# Input data -------------------------------------------------------------
data.in <- read_csv("SOLI03.csv")
#data.in <- read_excel("NIST data_summary.xlsx", sheet = 1, skip = 1)

# Rescue column 1 name ---------------------------------------------------
colnames(data.in)[1] <- "SAMPLE_NUMBER"

# structure of data ------------------------------------------------------
nrow(data.in)
ncol(data.in)
head (data.in, 10)
tail (data.in, 10)
str(data.in)
summary(data.in)

# Force numeric ENTRY ----------------------------------------------------
data.in$ENTRY <- as.numeric(data.in$ENTRY)

data.in <- na.omit(data.in)

hist(data.in$ENTRY, breaks = 30)


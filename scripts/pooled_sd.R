# Copy the data from Excel; 
#first column = no of data points, 
#second column = sd associated with those data points.

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
x <- read.delim('clipboard', as.is=TRUE, header = FALSE) 
} else { 
x <- read.delim(pipe('pbpaste'), header = FALSE) 
}

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(here)

#### Functions -----------------------------

#### Data Cleaning -----------------------------
colnames(x)[1] <- "n"
colnames(x)[2] <- "sd"

k <- nrow(x)
x$n2 <- x$n-1
x$sd2 <- x$sd^2
x$nsd <- x$n2*x$sd2

sumx <- sum(x$nsd)/(sum(x$n-k))
pooled_sd <- sqrt(sumx)
pooled_sd

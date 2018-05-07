# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(here)

# Data Input -------------------------------------------------------------
setwd(here())
data.in <- read_excel("~/Desktop/AFLAx.xlsx", 
                      col_types = c("numeric", "date", "numeric", 
                                    "text", "text", "text", "text", "text", 
                                    "numeric", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "numeric", "text"))
# Data Cleaning ----------------------------------------------------------

data.in <- data.in[,c(1,5,6,7,8,9)]
data.in$ENTRY2 <- as.numeric(data.in$ENTRY)

data.in <- data.in %>% 
  filter(SAMPLE_NUMBER > 9754391)

# Save sample names ------------------------------------------------------
samples <- unique(data.in$SAMPLE_NUMBER)

# Identify replicates, if present ----------------------------------------
#data.in$SAMPLE_NUMBER <- paste(data.in$SAMPLE_NUMBER,"_",data.in$REPLICATE_COUNT, sep="")

data.in_02 <- data.in %>% 
  filter(ANALYSIS == "AFLA020493")

data.in_10 <- data.in %>% 
  filter(ANALYSIS == "AFLA100716")

data.in_both <- data.in_02 %>% 
  filter(SAMPLE_NUMBER %in% data.in_10$SAMPLE_NUMBER)

data.in_10x <- data.in_10 %>% 
  filter(SAMPLE_NUMBER %in% data.in_both$SAMPLE_NUMBER)


data.in_both2 <- rbind(data.in_both, data.in_10x)

data.in_both2 <- data.in_both[,c(1,2,5)]

data.in_wide <- spread(data.in_both2,ANALYSIS, ENTRY)
  
write.csv(data.in_both2, "AFLA_2.csv")  


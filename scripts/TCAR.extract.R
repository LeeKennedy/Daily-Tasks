# Clean Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Data imput -------------------------------------------------------------
data <- read_excel("~/Desktop/11932704.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "numeric", "numeric", "text", 
                                 "text", "text", "text", "numeric"))

# Extract sample number --------------------------------------------------
Sample <- data$SAMPLE_NUMBER[1]

# Pull together the required components  (note: 'or' operator) -----------
data2 <- data %>%
        filter(grepl("ASH", ANALYSIS) | 
                        grepl("PROT", ANALYSIS) | 
                        grepl("MOIS", ANALYSIS) | 
                        grepl("FATS", ANALYSIS) | 
                        grepl("DIET", ANALYSIS) |
                        grepl("TCAR", ANALYSIS) |
                        grepl("ENER", ANALYSIS) |
                        grepl("Sugars", REPORTED_NAME) |
                        grepl("Sodium", REPORTED_NAME)) %>%
        arrange(REPORTED_NAME)

data2 <- data2[, c(3,6)]
data2$ENTRY <- as.numeric(data2$ENTRY)

# Turn off scientific notation -------------------------------------------
options(scipen=999)

# Extract CHO & Sugar data -----------------------------------------------
cho <- data2 %>% 
        filter(REPORTED_NAME == "Total Carbohydrate [m/m]") %>% 
        select(ENTRY)
sugar <- data2 %>% 
        filter(REPORTED_NAME == "Total Sugars") %>% 
        select(ENTRY)

# Calculate difference ---------------------------------------------------
newrow = as.data.frame(c("Difference",cho-sugar))
colnames(newrow)[1] <- "REPORTED_NAME"
data2 = rbind(data2,newrow)
 data2

# Calculate salt content (ash reality check) -----------------------------
salt <- data2 %>% 
        filter(REPORTED_NAME == "Sodium") %>% 
        select(ENTRY)

newrow1 = as.data.frame(c("Theoretical Salt",salt*2.54/1000))
colnames(newrow1)[1] <- "REPORTED_NAME"
data2 = rbind(data2,newrow1)

# Assemble table in desired order ----------------------------------------
n <- nrow(data2)
m <- n-1
data2 <- data2[c(1,n,2:m),]

data2$ENTRY <- round(data2$ENTRY,2)
data2

# Original CHO calculation ----------------------------------------------
Original_CHO <- 100 - sum(data2[c(1,3,5,6,7),2], na.rm=TRUE)
Original_CHO

# Export results ---------------------------------------------------------
write.csv(data2, paste(Sample, "_summary.csv", sep=""), row.names=TRUE)

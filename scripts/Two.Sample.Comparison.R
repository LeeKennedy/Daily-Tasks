# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Input -------------------------------------------------------------
data.in <- read_excel("C:/Users/leekennedy/Desktop/Nestle.xlsx")

# Data Cleaning ----------------------------------------------------------

analytes <- c("Calcium", "Copper", "Iron", "Magnesium", "Manganese", "Potassium", "Sodium", "Zinc", "Ash (@ 550?C)")
#analytes <- unique(data.in$REPORTED_NAME)

data.in$SAMPLE_NUMBER <- paste(data.in$SAMPLE_NUMBER,"_",data.in$REPLICATE_COUNT, sep="")

data.2 <- data.in[,c(1,3,5,6)]

data.2$ENTRY <- round(data.2$ENTRY,3)

data.2 <- data.2 %>%
  filter(REPORTED_NAME %in% analytes)

data.3 <- spread(data.2, SAMPLE_NUMBER, ENTRY)

colnames(data.3)[3] <- "Test"
colnames(data.3)[4] <- "Retest"

data.3 <- data.3 %>% 
  mutate(Ratio = round(Retest/Test,2))

print(tbl_df(data.3), n=40)

write_csv(data.3, "C:/Users/leekennedy/Desktop/Nestle_summary.csv")

# Visualising Data ----------------------------------------------------


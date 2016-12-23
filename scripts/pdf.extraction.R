# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tabulizer)

# Data Input -------------------------------------------------------------

location <- 'C:\\Users\\leekennedy\\Desktop\\Pesticides Lists\\FreshTest-Test-Codes-and-LORs 2016.pdf'

# Extract the table
out <- extract_tables(location)

out2 <- as.data.frame(out)

write.csv(out2, "freshtest.csv")

# Data Cleaning ----------------------------------------------------------
# Joining multipage tables
#final <- do.call(rbind, out[-length(out)])

# table headers get extracted as rows with bad formatting. Dump them.
#final <- as.data.frame(final[3:nrow(final), ])

# Column names
#headers <- c('Notice.Date', 'Effective.Date', 'Received.Date', 'Company', 'City', 
             'No.of.Employees', 'Layoff/Closure')

# Apply custom column names
#names(final) <- headers
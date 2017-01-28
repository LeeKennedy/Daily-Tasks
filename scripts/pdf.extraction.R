# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tabulizer)

# Data Input -------------------------------------------------------------

location <- '~/Desktop/DTSQ17000252.pdf'

# Extract the table
out <- extract_tables(location)

out <- out[c(-1,-2,-7)]

final <- do.call(rbind, out[-length(out)])

final2 <- as.data.frame(final[1:nrow(final), ])

final2 <- final2 %>% 
  filter(V3 == "mg/kg ") %>% 
  arrange(V1)

final2 <- final2[,c(1,3,4)]

colnames(final2) <- c("Pesticide", "Units", "LOR")

write.csv(final2, "~/Desktop/pests.csv")

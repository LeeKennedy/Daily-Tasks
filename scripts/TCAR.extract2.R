# Clear Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(dplyr)
library(readxl)

# Data input -------------------------------------------------------------
#data <- read.csv("9330783.csv", as.is=TRUE)
data <- read_excel("S:/Chemistry/Technical Support Projects/TCAR - Reporting Carbohydrates/10241173/Test.xlsx")
View(Test)
colnames(data)[1] <- "SAMPLE_NUMBER"

# Extracting Protein Factor ----------------------------------------------
ftemp <- data %>%
  filter(grepl("Protein",REPORTED_NAME ))

bits <- unlist(strsplit(as.character(ftemp[1,3]), ' '))
bits2 <- substr(bits[4], 1, nchar(bits[4])-1)

nf1 <- as.numeric(bits2)

# Nominate new protein factor --------------------------------------------
nf2 <- 6.38

Sample <- data$SAMPLE_NUMBER[1]

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

# Calculating components -------------------------------------------------
cho <- data2 %>% 
        filter(REPORTED_NAME == "Total Carbohydrate") %>% 
        select(ENTRY)
sugar <- data2 %>% 
        filter(REPORTED_NAME == "Total Sugars") %>% 
        select(ENTRY)


salt <- data2 %>% 
        filter(REPORTED_NAME == "Sodium") %>% 
        select(ENTRY)

newrow1 = as.data.frame(c("Theoretical Salt",salt*2.54/1000))
colnames(newrow1)[1] <- "REPORTED_NAME"
data2 = rbind(data2,newrow1)

# Assemple table of results ----------------------------------------------

n <- nrow(data2)
m <- n-1
data2 <- data2[c(1,n,2:m),]

data2$ENTRY <- round(data2$ENTRY,2)

data2$ENTRY_2 <- data2$ENTRY

data2$ENTRY_2[7] <- data2$ENTRY[7]*nf2/nf1
data2$ENTRY_2[9] <- 100 - sum(data2$ENTRY_2[c(1,3,5,6,7)], na.rm = TRUE)

data2$REPORTED_NAME[7] <- "Protein (N x factor)"

newrow = c(1:3)
data2 = rbind(data2,newrow)
data2[11,1] <- "Factor used"
data2[11,2] <- nf1
data2[11,3] <- nf2

data2 <- data2[c(1,2,3,4,5,6,7,11,8,9,10),]
data2$ENTRY_2 <- round(data2$ENTRY_2,2)

data2

# Export table -----------------------------------------------------------
write.csv(data2, paste(Sample, "_summary.csv", sep=""), row.names=TRUE)

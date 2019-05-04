# Clear Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(LK.Toolbox)

# Function -------------------------------------------------------------


# Data input -------------------------------------------------------------

df1 <- read_excel("~/Downloads/12438067.xlsx")


dummy <- df1[1,]
dummy$ANALYSIS <- "DIET02"
dummy$REPORTED_NAME <- "Dietary Fibre"
dummy$ENTRY <- 0

if (any(grepl("DIET", df1$ANALYSIS) == FALSE) == TRUE){
  df1 <- rbind(df1, dummy)
}

# Extracting Protein Factor ----------------------------------------------
ftemp <- df1 %>%
  filter(grepl("Protein",REPORTED_NAME ))

bits <- unlist(strsplit(as.character(ftemp[1,3]), ' '))
bits2 <- substr(bits[4], 1, nchar(bits[4])-1)

nf1 <- as.numeric(bits2)

#### Nominate new protein factor --------------------------------------------
nf2 <- 5.55

Sample <- df1$SAMPLE_NUMBER[1]

df2 <- df1 %>%
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

df2 <- df2[, c(3,6)]
df2$ENTRY <- as.numeric(df2$ENTRY)


# Turn off scientific notation -------------------------------------------
options(scipen=999)

# Calculating components -------------------------------------------------
cho <- df2 %>%
  filter(REPORTED_NAME == "Total Carbohydrate") %>%
  select(ENTRY)
sugar <- df2 %>%
  filter(REPORTED_NAME == "Total Sugars") %>%
  select(ENTRY)


salt <- df2 %>%
  filter(REPORTED_NAME == "Sodium") %>%
  select(ENTRY)

newrow1 = as.data.frame(c("Theoretical Salt",salt*2.54/1000))
colnames(newrow1)[1] <- "REPORTED_NAME"
df2 = rbind(df2,newrow1)

# Assemple table of results ----------------------------------------------

n <- nrow(df2)
m <- n-1
df2 <- df2[c(1,n,2:m),]

df2$ENTRY <- round(df2$ENTRY,2)

df2$ENTRY_2 <- df2$ENTRY

df2$ENTRY_2[7] <- df2$ENTRY[7]*nf2/nf1
df2$ENTRY_2[9] <- 100 - sum(df2$ENTRY_2[c(1,3,5,6,7)], na.rm = TRUE)

df2$REPORTED_NAME[7] <- "Protein (N x factor)"

newrow = c(1:3)
df2 = rbind(df2,newrow)
df2[11,1] <- "Factor used"
df2[11,2] <- nf1
df2[11,3] <- nf2

df2 <- df2[c(1,2,3,4,5,6,7,11,8,9,10),]
df2$ENTRY_2 <- round(df2$ENTRY_2,2)

colnames(df2)[2] <- paste("Nx", nf1, sep = "")
colnames(df2)[3] <- paste("Nx", nf2, sep = "")

df2


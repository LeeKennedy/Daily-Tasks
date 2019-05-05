# Clear Environment ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(LK.Toolbox)

# Function -------------------------------------------------------------


# Data input -------------------------------------------------------------

df1 <- read_excel("~/Downloads/12438067.xlsx")

nip <- TCAR(df1)
TCAR(df1)
#write.csv(nip, "NIP_xxxxxxxx.csv)
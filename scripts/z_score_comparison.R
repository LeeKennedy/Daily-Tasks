#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------

library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)

#### Functions -----------------------------


#### Data Input -----------------------------

here::here()

data <- read_excel("H:/GitHub Projects/TMI Vitamin A/data/VITAE_TMI.xlsx", 
                   col_types = c("numeric", "date", "numeric", 
                                 "text", "text", "text", "text", "numeric", 
                                 "numeric", "text", "numeric", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text"))


#### Relevant data -----------------------------

grade <- "TM52"
analysis_1 <- "VITA120507"
analysis_2 <- "VITE050507"

z_plot(analysis_1, analysis_2, grade)


# Optional strip_mm

#### Data Cleaning -----------------------------


#### Visualising Data -----------------------------

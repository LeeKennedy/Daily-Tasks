#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here()

data <- read_excel("data/CAP.xlsx", col_types = c("numeric", 
                                                  "date", "text", "text", "text", "text", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "numeric", "numeric", "text", "text", 
                                                  "text", "text", "text", "text"))

#### Data Cleaning -----------------------------

data$SAMPLE_NUMBER <- paste(data$SAMPLE_NUMBER,data$REPLICATE_COUNT, sep="_")

data <- data[,c(1,3,7,8,9)]

data <- data %>% 
  filter(UNITS == "PCT_M-M" | UNITS == "G_P_100G")
data <- data[,-4]

data_wide <- spread(data, REPORTED_NAME, ENTRY)
data_wide <- na.omit(data_wide)
data_wide <- data_wide %>% 
  filter(Phosphorus > 1) %>% 
  group_by(PRODUCT) %>% 
  filter(n() > 10) %>% 
  filter(PRODUCT != "QC")

#### Visualising Data -----------------------------

data_plot <- ggplot(data_wide, aes(x = Calcium, y = Phosphorus, fill = PRODUCT))+
  geom_point(size=4, shape=21, col="black")
data_plot
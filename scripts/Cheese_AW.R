# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(here)

# Data Input -------------------------------------------------------------
setwd(here())
data.in <- read_excel("~/Desktop/Cheese.xlsx")
# Data Cleaning ----------------------------------------------------------

data.in <- data.in[,c(1,5,6,7,8,9)]

# Save sample names ------------------------------------------------------
samples <- unique(data.in$SAMPLE_NUMBER)

# Identify replicates, if present ----------------------------------------
#data.in$SAMPLE_NUMBER <- paste(data.in$SAMPLE_NUMBER,"_",data.in$REPLICATE_COUNT, sep="")

data.in_ts <- data.in %>% 
  filter(REPORTED_NAME == "Total Solids") %>% 
  mutate(ENTRY = 100-ENTRY)
data.in_ts$REPORTED_NAME = "Moisture"

data.in_m <- data.in %>% 
  filter(REPORTED_NAME == "Moisture")

data.in_wa <- data.in %>% 
  filter(grepl("Water", REPORTED_NAME))
data.in_wa$REPORTED_NAME = "Water Activity"

data.in2 <- rbind(data.in_m, data.in_ts, data.in_wa)

write.csv(data.in2, "temp.csv")

aw_data <- unique(data.in_wa$SAMPLE_NUMBER)

plot_data <- data.in2 %>% 
  filter(SAMPLE_NUMBER %in% aw_data)

plot_data <- plot_data[,c(1,3,5,6)]

data.3 <- spread(plot_data, REPORTED_NAME, ENTRY)
colnames(data.3)[4] <- "aW"


# Visualising Data ----------------------------------------------------

plot_data <- ggplot(data.3, aes(x = Moisture, y = aW)) +
  geom_point(size=4, shape = 21, colour = 'black', fill = 'cornflowerblue') +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
plot_data

ggsave("aw_cheese.png", width = 8, height = 5, dpi=200)

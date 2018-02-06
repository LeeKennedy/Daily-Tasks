# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data Input -------------------------------------------------------------
data.in <- read_csv("C:/Users/leekennedy/Desktop/Cheese.csv")

# Data Cleaning ----------------------------------------------------------

#analytes <- c("Calcium", "Copper", "Iron", "Magnesium", "Manganese", "Potassium", "Sodium", "Zinc", "Ash (@ 550?C)")
analytes <- unique(data.in$REPORTED_NAME)

# Save sample names ------------------------------------------------------
samples <- unique(data.in$SAMPLE_NUMBER)

# Identify replicates, if present ----------------------------------------
data.in$SAMPLE_NUMBER <- paste(data.in$SAMPLE_NUMBER,"_",data.in$REPLICATE_COUNT, sep="")

data.2 <- data.in[,c(1,3,5,6)]
data.2$ENTRY <- as.numeric(data.2$ENTRY)

data.2$ENTRY <- round(data.2$ENTRY,3)

# Use analytes list to select elements (Optional) ------------------------
data.2 <- data.2 %>%
  filter(REPORTED_NAME %in% analytes)

data.2 <- data.2 %>% 
  arrange(REPORTED_NAME) %>% 
  group_by(SAMPLE_NUMBER) %>% 
  mutate(ID = seq_along(SAMPLE_NUMBER))

data.3 <- spread(data.2, SAMPLE_NUMBER, ENTRY)

colnames(data.3)[4] <- "Test"
colnames(data.3)[5] <- "Retest"

data.3 <- data.3 %>% 
  mutate(Ratio = round(Retest/Test,2))

print(tbl_df(data.3))

write_csv(data.3, "summary.csv")

# Visualising Data ----------------------------------------------------

plot_data <- ggplot(data.3, aes(x = Test, y = Retest)) +
  geom_point(size=4, shape = 21, colour = 'black', fill = 'cornflowerblue') +
  geom_abline(slope = 1, intercept = 0, lty = 2, colour = 'red') +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14)) +
  labs(title = "Comparison Graph", subtitle = paste("Samples ", samples[1]," vs ", samples[2]))
plot_data
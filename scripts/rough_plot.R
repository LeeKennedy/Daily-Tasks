# Clean environment ------------------------------------------------------
rm(list = ls())

# Packages ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)

# Input data -------------------------------------------------------------
data.in <- read.csv("B1.csv", as.is=TRUE, header=TRUE)
colnames(data.in)[1] <- "Sample"

# Adust date format ------------------------------------------------------
data.in$LOGIN_DATE <- dmy_hm(data.in$LOGIN_DATE)

# Nominate any points that you want to be coloured differently -----------
ref_samples <- c(6991811, 8039395)

data.in$set <- "No"

for(i in 1:nrow(data.in)){
  
    if(data.in$Sample[i] %in% ref_samples)
    data.in$set[i] = "Yes"
}

# determine mean line ----------------------------------------------------
mean_line <- mean(data.in$ENTRY)

# Plot the graph ---------------------------------------------------------
plot1 <- ggplot(data.in, aes(x=LOGIN_DATE, y = ENTRY, fill = set)) +
  geom_point(size = 4, shape = 21, colour = "black") +
  geom_hline(yintercept = mean_line) +
  scale_fill_brewer(palette = "Set1")+
  labs(x = "", y = "mg/g", title = "WHPN02 Results for MG3380 since 1/1/2015") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14)) 
plot1

# Save graph -------------------------------------------------------------
#ggsave(plot1, device = NULL, width = 10, height = 4, file = paste("text", "_",Sys.Date(),".png", sep=""))


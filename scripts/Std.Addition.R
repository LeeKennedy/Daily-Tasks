# Clean environment ------------------------------------------------------
rm(list = ls())

# Packages ---------------------------------------------------------------
library(ggplot2)

#load data - Col1 = X-axis, Col2 = Y-axis --------------------------------
data1 <- read.csv("stdadd3.csv", as.is = TRUE, header = TRUE)
row_1 <- colnames(data1)[1]
colnames(data1)[1] <- "Conc"
row_2 <- colnames(data1)[2]
colnames(data1)[2] <- "Abs"

#Do a correlation test on two components ---------------------------------
cor.test(data1$Abs, data1$Conc)

# Fit the curve to the data ----------------------------------------------
fit <- lm(data1$Abs ~ data1$Conc)
summary(fit)

a <- coef(fit)
intercept <- a[1]/a[2]


# Plot graph -------------------------------------------------------------
p <- ggplot(data1, aes(x = Conc, y = Abs)) +
  geom_point(size = 4) + 
  stat_smooth(method = lm, se = TRUE, fullrange = TRUE) +
  geom_vline(xintercept = 0, colour="black", lwd = 0.5, linetype=1) +
  geom_vline(xintercept = intercept*-1, colour="red", lwd = 0.75, linetype=2) + geom_hline(yintercept = 0, colour = "black", lwd = 0.5, linetype = 1) +
  annotate("text", label = round(intercept,3), x = -1*intercept, y = 0.2*max(data1[,2])) +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"),  
        text = element_text(size = 14))

p

intercept


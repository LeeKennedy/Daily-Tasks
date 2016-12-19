# Clear Environment ------------------------------------------------------
rm(list=ls())
dev.off()

# Packages ---------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Functions --------------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Data Import ------------------------------------------------------------
data1 <- read.csv("B1.csv", as.is=TRUE, header = TRUE)

#Tidying up data ---------------------------------------------------------
colnames(data1)[1] <- 'Sample'
colnames(data1)[8] <- 'Result'
colnames(data1)[4] <- 'Operator'
data1$Result <- as.numeric(data1$Result)
data1 <- na.omit(data1)

testname <- substr(data1$ANALYSIS[1],1,6)

# checking that only one srm present -------------------------------------
srm_no <- unique(data1$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

data1 <- data1 %>%
        filter(Sample >8655991)

mid_line <- mean(data1$Result)

# Plot by Operators ------------------------------------------------------
p <- ggplot(data1, aes(x = Sample,y = Result, fill = Operator)) + 
        geom_point(size=5, alpha = 1, shape=21, colour="black") +
        geom_hline(yintercept = mid_line, lty=2) +
        theme_bw()
p

# Save Plot --------------------------------------------------------------
ggsave(p, device = NULL, file = paste(testname,"_Operators_2_", Sys.Date(),".png", sep=""))



# Plot individual operators ----------------------------------------------
p + facet_wrap(~ Operator, ncol=2) # individual panels

f3 <- split(data1$Result, data1$Operator)
f4 <- lapply(f3, remove_outliers)
f5 <- lapply(f4, remove_outliers)
boxplot(f5, las=2)
hint <- median(data1$Result)
abline(h = hint)


# Produce summary table --------------------------------------------------
f6 <- unsplit(f5, data1$Operator)
f7 <- cbind(data1,f6)
f7a <- na.omit(f7)

b1 <- tapply(f7a$Result, f7a$Operator, length)
b2 <- tapply(f7a$Result, f7a$Operator, mean)
b3c <- tapply(f7a$Result, f7a$Operator, sd)
b4 <- cbind(b1, b2, b3c)
b4 <- as.data.frame(b4)
colnames(b4) <- c("n", "Mean", "sd")

b4

# Write data file --------------------------------------------------------
write.csv(b4, "Operators.csv")

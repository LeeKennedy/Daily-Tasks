# Clear Environment ------------------------------------------------------
rm(list=ls())

library(readr)
library(readxl)
library(ggplot2)
library(dts.quality)

# Import data ------------------------------------------------------------
#data.in <- read_csv("ANOX04.csv")
data.in <- read_excel("~/Desktop/AFLA09_SRM.xlsx")
data.in <- na.omit(data.in)

#Tidying up column names.-------------------------------------------------
colnames(data.in)[1] <- 'Sample'
colnames(data.in)[8] <- 'Result'
colnames(data.in)[4] <- 'Operator'

testname <- substr(data.in$ANALYSIS[1],1,6)

afla <- unique(data.in$REPORTED_NAME)
afla_key <- afla[2]

data.in <- data.in %>% 
  filter(REPORTED_NAME == afla_key)

# checking that only one srm present.-------------------------------------
srm_no <- unique(data.in$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

data.in$Number <- as.numeric(row.names(data.in))

mid_line <- mean(data.in$Result)
SD <- sd(data.in$Result)
UCL <- mid_line+3*SD
LCL <- mid_line-3*SD
UWL <- mid_line+2*SD
LWL <- mid_line-2*SD

####### Plot by Operators #######-----------------------------------------
p <- ggplot(data.in, aes(x = Number,y = Result, fill = Operator)) + 
        geom_point(size=5, alpha = 1, shape=21, colour="black") +
        geom_hline(yintercept = mid_line, lty=2) +
        geom_hline(yintercept = UCL, lty=2) +
        geom_hline(yintercept = UWL, lty=2) +
        geom_hline(yintercept = LWL, lty=2) +
        geom_hline(yintercept = LCL, lty=2) +
        labs(x="", title = paste(testname,srm_no, afla_key,sep="_")) +
        theme_bw() 
p

# Save Graph -------------------------------------------------------------
ggsave(p, 
       device = NULL, 
       width = 10, 
       height = 5, 
       dpi=100,
       file = paste(testname,"_Operators_", afla_key,"_", Sys.Date(),".png", sep=""))

dev.off()

# Plot by Operator -------------------------------------------------------
p + facet_wrap(~ Operator, ncol=2) # individual panels

# Create summary table ---------------------------------------------------
f3 <- split(data.in$Result, data.in$Operator)
f4 <- lapply(f3, outliers)
f5 <- lapply(f4, outliers)
boxplot(f5, 
        las=2,
        main = paste(testname,srm_no, sep="_"),
        ylab = "Result"
        )
hint <- median(data.in$Result)
abline(h = hint, lty = 2, col = "blue")

f6 <- unsplit(f5, data.in$Operator)
f7 <- cbind(data.in,f6)
f7a <- na.omit(f7)

b1 <- tapply(f7a$Result, f7a$Operator, length)
b2 <- tapply(f7a$Result, f7a$Operator, mean)
b3c <- tapply(f7a$Result, f7a$Operator, sd)
b4 <- cbind(b1, b2, b3c)
b4 <- as.data.frame(b4)
colnames(b4) <- c("n", "Mean", "sd")

# Display table ----------------------------------------------------------
b4
write.csv(b4, paste(testname,"_Operators_", afla_key,"_",Sys.Date(),".csv", sep=""))


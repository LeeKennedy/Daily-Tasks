# Clean environment ------------------------------------------------------
rm(list = ls())

# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readxl)
library(psych)
library(lubridate)

# Functions --------------------------------------------------------------
outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

data.raw <- read.csv("IRM001A_CL.csv", as.is=TRUE, header=TRUE)
#data.raw <- read_excel("CHLN.xlsx", sheet = 1)

colnames(data.raw)[1] <- "SAMPLE_NUMBER"

# Input plotting parameters ----------------------------------------------

max.pts <- 900 # Maximum points plotted
points <- 50   # How many points used to set control lines

# ------------------------------------------------------------------------

testname <- substr(data.raw$ANALYSIS[1],1,6)

Name <- data.raw$REPORTED_NAME[1]
Units <- tolower(sub("_P_","/",(data.raw$REPORTED_UNITS[1])))

# Clean the data ---------------------------------------------------------
data.in <- data.raw %>%
        arrange(SAMPLE_NUMBER) %>%
        filter(SAMPLE_NUMBER > 7100000) %>%
        mutate(ENTRY = as.numeric(as.character(ENTRY))) %>%
        na.omit

data.in <- data.in[,c(2,4,5,8)]

data.in$LOGIN_DATE <- dmy_hms(data.in$LOGIN_DATE)

boxplot(data.in$ENTRY~data.in$SAMPLING_POINT,
        main = paste("Comparative ",testname," SRM/IRM Results"),
        ylab = paste(Name," ", Units))

j <- length(unique(data.in$SAMPLING_POINT))


srm1 <- unique(data.in[3])

srm <- srm1[j,]

data.in3 <- data.in[data.in$SAMPLING_POINT==srm,]
names(data.in3) <- c("Date", "Operator", "SRM","A")
data.in3 <- select(data.in3, everything())%>%
        filter( between(row_number(), n()-max.pts, n()))

clean <- outliers(data.in3$A[1:points])
xx <- describe(clean)

UCL <- xx$mean + 3*xx$sd
UWL <- xx$mean + 2*xx$sd
Centre <- xx$mean 
LWL <- xx$mean - 2*xx$sd
LCL <- xx$mean - 3*xx$sd
MU <- 2*xx$sd

CC <- c(UCL, UWL, Centre, LWL, LCL ,MU)
Labels <- c("UCL", "UWL", "Centre", "LWL", "LCL", "MU +/-")
Clines <- cbind(Labels, round(CC,3))
Clines <- as.data.frame(Clines)

# Calculate outliers -----------------------------------------------------------
data.in3$outliers = NA
j <- nrow(data.in3)

for (i in 1:j) {
        if(between(data.in3$A[i], LCL, UCL)==TRUE)
           data.in3$outliers[i]=data.in3$A[i]
}

# Calculate trend on one side of mean ------------------------------------------
data.in3$T1 <- 1
data.in3$T2 <- 1

for (i in 2:j) {
        if(data.in3$A[i] < Centre)
                data.in3$T1[i] = -1
}
n=1
for (i in 2:j) {
        if(data.in3$T1[i] == data.in3$T1[i - 1]) {
                data.in3$T2[i] = n +1
                n = n + 1
        } else {  
                data.in3$T2[i] = 1
                n = 1
        }
}

data.in3$T3 <- NA
for (i in 1:j) {
        if(data.in3$T2[i] > 6)
                data.in3$T3[i] = data.in3$A[i]
}

# Trending up or down -----------------------------------------------------

data.in3$T4 <- 1
data.in3$T5 <- 1

for (i in 2:j) {
        if(data.in3$A[i] < data.in3$A[i-1])
                data.in3$T4[i] = -1
}
n=1
for (i in 2:j) {
        if(data.in3$T4[i] == data.in3$T4[i - 1]) {
                data.in3$T5[i] = n +1
                n = n + 1
        } else {  
                data.in3$T5[i] = 1
                n = 1
        }
}

data.in3$T6 <- NA
for (i in 1:j) {
        if(data.in3$T5[i] > 6)
                data.in3$T6[i] = data.in3$A[i]
}


# Plotting ----------------------------------------------------------------

data_new <- data.in3
data_new$T6 <- as.numeric(data_new$T6)
data_new$T3 <- as.numeric(data_new$T3)
data_new$row_n <- as.numeric(rownames(data_new))
data_new$login <- as.Date(data_new$Date)



plot_new <- ggplot(data_new, aes(x=login, y=A)) +
        geom_line(aes(y=outliers), size=0.5, colour = "gray50") +
        geom_point(size=4, shape = 21, fill ="red", colour = "black") +
        geom_point(aes(y=outliers), size = 4, shape = 21, fill ="gray50", colour = "black") +
        geom_point(aes(y=T3), size = 4, shape = 21, fill ="green", colour = "black") +
        geom_point(aes(y=T6), size = 4, shape = 21, fill ="orange", colour = "black") +
        geom_hline(yintercept = Centre, lty = 2,lwd=1, colour = "black") +
        geom_hline(yintercept = UWL, lty = 2, lwd=1,colour = "blue") +
        geom_hline(yintercept = LWL, lty = 2, lwd=1,colour = "blue") +
        geom_hline(yintercept = UCL, lty = 2,lwd=1, colour = "red") +
        geom_hline(yintercept = LCL, lty = 2, lwd=1,colour = "red") +
        geom_vline(xintercept = points, lty = 2, lwd=1,colour = "gray50") +
        theme_bw() +
        labs(x="", y=paste(Name, "-", Units),  title= paste(testname," Control Chart: ", srm,"\n")) +
        theme(plot.title = element_text(size=22)) +
        scale_y_continuous(limits = c(0.97*LCL, 1.03*UCL))  
plot_new
ggsave(plot_new, device = NULL, width = 10, height = 5, file = paste(testname,"_", srm,"_Control_Chart_", Sys.Date(),".png", sep=""))


# Histogram of control chart ---------------------------------------------
hist(data.in3$outliers, 
     breaks=20, 
     xlab=srm,
     main = paste("Control Chart",srm,"data distribution"))
abline(v = Centre, col = "red", lty = 2)

print(describe(data.in3$outliers))
print (Clines)

xx <- boxplot(data.in3$A~data.in3$Operator, las=2, cex.axis = 0.8)
abline(h=Centre, col = "blue", lty=2, lwd=2)
mytable <- xx$stats
colnames(mytable)<-xx$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

table(data.in3$Operator)




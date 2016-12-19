library(dplyr)
library(ggplot2)

data1 <- read.delim('clipboard', header = FALSE, as.is = TRUE)
#data1 <- read.delim(pipe('pbpaste'), header=FALSE)


# Functions --------------------------------------------------------------
bs <- rep(0,10000)


#-------------------------------------------------------------------------

N <- nrow(data1)
        
for(i in 1:10000) {
                y <- sample(data1$V1, N, replace=T)
                bs[i] = sd(y)
}

mean_bs <- mean(bs)
sd_bs <- sd(bs)
Upper_bs <- mean_bs + 2*sd_bs
Lower_bs <- mean_bs - 2*sd_bs

bootstrap <- data.frame(Lower_bs, mean_bs, Upper_bs)
colnames(bootstrap) <- c("Lower 95% sd", "Std Dev", "Upper 95% sd")
bootstrap

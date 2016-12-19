# Clear Environment ------------------------------------------------------
rm(list=ls())
dev.off()

# Packages ---------------------------------------------------------------
library(ggplot2)

#Reference mean
xref <- 0.2

# Read data --------------------------------------------------------------
xx <- read.csv("Book1.csv", as.is=TRUE, header=TRUE)
xx$Diff <- xx$A - xx$B
xbar <- mean(xx$Diff)
xsd <- sd(xx$Diff)

#Plot graph --------------------------------------------------------------
fxx <- function(x) dnorm(x, mean = xbar, sd = xsd)
myYLim <- c(0, 2)
plot(fxx, from = xbar-4*xsd, to = xbar+4*xsd, ylim = myYLim, col = "red", lwd = 3, xlab = "", ylab = "")

#Reference point ---------------------------------------------------------
abline(v = xref, lwd = 3)

# X-Y comparison ---------------------------------------------------------
xxy <- lm(xx$A ~ xx$B)
summary(xxy)


# X-Y plot ---------------------------------------------------------------
plotxx <- ggplot(xx, aes(x=xx$A, y=xx$B)) +
        geom_point() +
        geom_smooth(method=lm) +
        geom_abline(intercept = 0, slope=1, lty=2, col="red") +
        xlab("Reference Value") +
        ylab("Comparison Value") +
        ggtitle("Title")

plotx <- plotxx + theme_bw()

#Fancy theme -------------------------------------------------------------
plotx <- plotx + theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
                       axis.line = element_line(size = 0.7, color = "black"), 
                       legend.position = c(0.15, 0.7), 
                       text = element_text(size = 14))    
plotx



# t-test -----------------------------------------------------------------
ttest <- t.test(xx$Diff, mu=xref, alternative = "two.sided", paired = FALSE)
ttest


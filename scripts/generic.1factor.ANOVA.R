# Clean environment ------------------------------------------------------
rm(list = ls())

# packages -----------
library(readxl)

#Import data -------------------------------------------------------------
# Data should be in labelled columns with no row labels.

Input <- read_excel("~/Documents/GitHub/Ammonia_Validation/data/Ammonia by HACH Validation Workbook.xlsx", 
                    sheet = "ANOVA - High", na = "")
Input <- Input[1:7,1:6]
Input <- as.data.frame(sapply(Input, as.numeric))

#Always look at the data -------------------------------------------------
boxplot(Input, 
        frame = FALSE,
        outpch = 16,    # 16 = filled circle
        outcol = "red")

# Review data frame ------------------------------------------------------
summary(Input)

#convert to a two element stack ------------------------------------------
xs <- stack(Input)

# Run ANOVA --------------------------------------------------------------
anova1 <- aov(values ~ ind, data = xs)

summary(anova1)

#Check for significant differences ---------------------------------------
TukeyHSD(anova1)

#Repeatability & Interim Precision ---------------------------------------
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

#Export ------------------------------------------------------------------
capture.output(summary(anova1), file = "test.doc" )


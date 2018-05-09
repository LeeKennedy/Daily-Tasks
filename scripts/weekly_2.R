#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(stringr)


#### Functions -----------------------------
#' This function creates a summary file based on the exported data from Google Kanbanchi.


weekly_list <- function (x, b=FALSE) {
        fnx <- tidyr::gather(x, key = Area, value = Project, na.rm = FALSE, Major, Minor, Doing, `Quality Issues`,  `With Lab`, `With IT`, `With Quality`, Limbo, Done)

        fnx <- na.omit(fnx)

        ### Add sorting key ------------------------------------------------------

        fnx$key <- 0
        fnx <- fnx[,c(3,1,2)]
        n <- nrow(fnx)

        area <- c("Doing", "Major", "Minor","With Lab", "With Quality", "With IT", "Limbo", "Quality Issues", "Done")

        for (i in 1:n) {
                fnx$key[i] <- match(fnx$Area[i], area)
        }

        if(b == FALSE) {
                fnx <- fnx %>% filter(!grepl("LK -", Project))
        }


        ### Sort and initial split ---------------------------------------------

        fnx <- fnx %>%
                arrange(key, Project) %>%
                separate(Project, c("Project","Control"),"\n")

        ### Split of the control markers ---------------------------------------

        fnx$Lab <- NA
        fnx$GB <- NA
        fnx$Proj <- NA
        fnx$CC <- NA
        fnx$NM <- NA
        fnx$EM <- NA
        fnx$Story <- NA
        fnx$Closed <- NA
        fnx$Tag <- NA

        for (i in 1:n){

                temp <- unlist(strsplit(fnx$Control[i], ", "))
                m = length(temp)

                if (m == 0) next

                for (j in 1:m) {
                        if(grepl("Area", temp[j]) == TRUE) fnx$Lab[i] = str_sub(temp[j], start= 6)
                        if(grepl("GB", temp[j]) == TRUE) fnx$GB[i] = "GB"
                        if(grepl("Project", temp[j]) == TRUE) fnx$Proj[i] = str_sub(temp[j], start= -3)
                        if(grepl("CC", temp[j]) == TRUE) fnx$CC[i] = str_sub(temp[j], start= -3)
                        if(grepl("NM", temp[j]) == TRUE) fnx$NM[i] = str_sub(temp[j], start= -5)
                        if(grepl("EM", temp[j]) == TRUE) fnx$EM[i] = str_sub(temp[j], start= -5)
                        if(grepl("Story", temp[j]) == TRUE) fnx$Story[i] = str_sub(temp[j], start= -3)
                        if(grepl("Closed", temp[j]) == TRUE) fnx$Closed[i] = str_sub(temp[j], start= -10)
                        if(grepl("Tag", temp[j]) == TRUE) fnx$Tag[i] = str_sub(temp[j], start= -5)
                }
        }

        fnx[is.na(fnx)] <- ""
        colnames(fnx)[4] <- "Comment"
        fnx$Comment <- ""

        # Exporting Data -------------------------------------------------------

        write_csv(fnx, paste("Project_List_", Sys.Date(), ".csv", sep=""))

}



#### Data Input -----------------------------

data.in <- read_csv("C:/Users/leekennedy/Downloads/DTS_Projects_full.csv")

#data.in <- read_csv("~/Desktop/In Tray/DTS_Projects_full.csv")
#setwd("~/Desktop")
setwd("C:/Users/leekennedy/Desktop")
weekly_list(data.in, FALSE)

#### READ and configure the University of Washington Visual Field Data    #####
#### https://tvst.arvojournals.org/article.aspx?articleid=2778219 to meet #####
#### R visualFields package data structure requirements.                  #####


rm(list=ls())
setwd('/Users/fabianyii/power/')

# install.packages("lubridate")
library("lubridate")

# Read original VF data
vf <- read.csv('data/raw_uwvf.csv')

########## columns must be ordered as follows: id, eye,           ##########
########## date, time, age, type, fpr, fnr, fl, L1...L54          ##########
confVf <- data.frame(id = 1:nrow(vf))
confVf$id <- vf$PatID
confVf$eye <- vf$Eye

rightEyeRows = which(vf$eye == 'Right') 
leftEyeRows = which(vf$eye == 'Left')
confVf$eye[rightEyeRows] <- 'OD' 
confVf$eye[leftEyeRows] <- 'OS'

# First VF test assumed to be taken on 01/01/2000
confVf$date <- as.Date(vf$Time_from_Baseline, origin = '2000-01-01')
confVf$date <- conf_d$date %m+% months( round(vf$Time_from_Baseline*12) )

confVf$time <- 0
confVf$age <- vf$Age
confVf$type <- 'unknown'
confVf$fpr <- 0
confVf$fnr <- 0
confVf$fl <- 0
confVf$duration <- 0
confVf <- cbind(confVf, vf[,23:76])

thresholds <- c()
for (i in 1:54){
  thresholds = c(thresholds, paste('L', i, sep='')) }

names(confVf)[11:64] <- thresholds

# include eye information in id, e.g. change id '647' to '647_OD'
confVf$id <- paste0(confVf$id, '_', confVf$eye)

write.csv(confVf, 'data/confUwvf.csv') 


#### READ and configure the University of Washington Visual Field Data    #####
#### https://tvst.arvojournals.org/article.aspx?articleid=2778219 to meet #####
#### R visualFields package data structure requirements.                  #####


rm(list=ls())
setwd('/Users/fabianyii/power/')

# install.packages("lubridate")
library("lubridate")

# Read original VF data
d <- read.csv('data/raw_uwvf.csv')

########## columns must be ordered as follows: id, eye,           ##########
########## date, time, age, type, fpr, fnr, fl, l1...l54          ##########
conf_d <- data.frame(id = 1:nrow(d))
conf_d$id <- d$PatID
conf_d$eye <- d$Eye

OD_ind = which(conf_d$eye == 'Right'); OS_ind = which(conf_d$eye == 'Left')
conf_d$eye[OD_ind] <- 'OD'; conf_d$eye[OS_ind] <- 'OS'

# First VF test assumed to be taken on 01/01/2000
conf_d$date <- as.Date(d$Time_from_Baseline, origin = '2000-01-01')
conf_d$date <- conf_d$date %m+% months( round(d$Time_from_Baseline*12) )

conf_d$time <- 0
conf_d$age <- d$Age
conf_d$type <- 'unknown'
conf_d$fpr <- 0
conf_d$fnr <- 0
conf_d$fl <- 0
conf_d$duration <- 0
conf_d <- cbind(conf_d, d[,23:76])

sens <- c()
for (i in 1:54){
  sens = c(sens, paste('l', i, sep=''))
}

names(conf_d)[11:64] <- sens

# include eye information in id, e.g. change id '647' to '647_OD'
conf_d$id <- paste0(conf_d$id, '_', conf_d$eye)

write.csv(conf_d, 'data/conf_all_VF.csv') # write configured VF data to 'data/config_uwvf'


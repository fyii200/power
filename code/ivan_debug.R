rm(list=ls())
setwd('/Users/fabianyii/power')

d <- read.csv('data/power.csv')         
d <- d[,-1]

#configure the dataset to be compatable with visualFields
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)
d$id <- rep(1:30, each=12)     # make px id range from 1-30

vflegoplot(d[1:12,], 's')
vfsparklines(d[1:12,], add=TRUE)
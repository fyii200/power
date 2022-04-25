rm(list=ls())
setwd('/Users/fabianyii/power')
# install.packages('doParallel')
# install.packages("devtools")
# install_github("imarinfr/vf1/source")
# install.packages("lubridate")

library(doParallel) # package for parallelisation
library(devtools)
library(visualFields)
library(lubridate)
source('code/powerAnalysis.R')
source('code/vfConfig.R')

#########################################################
# Power analysis on the VF data from Artes et al. (2014)
#########################################################
artesVf <- read.csv('data/artes2014.csv') 
artesVf <- artesVf[,-1]
artesVf <- vfConfig(data='artes', artesVf)

for (i in unique(artesVf$id)){
  vf <- subset(artesVf, id==i)
  powerAnalysis(vf) }

#########################################################
# Power analysis on the University of Washington VF data
#########################################################
# # Read VF data
# d <- read.csv('data/conf_uwvf.csv'); d <- d[,-1]
# eligible_d <- d
# 
# # we only want VF series with at least 5 tests (takes approx 5 min)
# for (i in unique(eligible_d$id)){
#   if (sum(eligible_d$id == i) < 5) { 
#     excl_rows <- which(eligible_d$id == i)
#     eligible_d <- eligible_d[-excl_rows,] }
# }
# 
# # save eligible VF series as 'eligible_conf_uwvf.csv'
# write.csv(eligible_d, 'data/eligible_conf_uwvf.csv')

uwvf <- read.csv('data/raw_uwvf.csv') 
uwvf <- vfConfig(data='uwvf', uwvf)

for (i in unique(uwvf$id)){
  vf <- subset(uwvf, id==i)
  powerAnalysis(vf) }




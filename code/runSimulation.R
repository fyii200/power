rm(list=ls())
setwd("/Users/fabianyii/power")
# install.packages("doParallel")
# install.packages("devtools")
# install_github("imarinfr/vf1/source")
# install.packages("lubridate")
# install.packages("qpdf")

library(doParallel) # package for parallelisation
library(devtools)
library(visualFields)
library(lubridate)
library(qpdf)
source('code/powerAnalysis.R')
source('code/vfConfig.R')

#########################################################
# Power analysis on the VF data from Artes et al. (2014)
#########################################################
artesVf <- read.csv('data/artes2014.csv') 
artesVf <- artesVf[,-1]
artesVf <- vfConfig(data='artes', artesVf)

combinedPowerResult <- data.frame()
for (i in unique(artesVf$id)){
  vf                  <- subset(artesVf, id==i)
  powerResult         <- powerAnalysis(vf)
  combinedPowerResult <- rbind(combinedPowerResult, powerResult)
  write.csv(combinedPowerResult, file.path('result','powerResult.csv') ) }

# save the path to each powerCurves PDF as 'files'. Then, combine them into
# a single PDF. Finally, delete individual PDFs.
files <- list.files('plots', full.names=TRUE, pattern='powerCurves')
pdf_combine(input=files, output=file.path('plots', 'combinedPowerCurves') )
unlink(files)

#########################################################
# Power analysis on the University of Washington VF data
#########################################################
uwvf <- read.csv('data/raw_uwvf.csv') 
uwvf <- vfConfig(data='uwvf', uwvf)

combinedPowerResult <- data.frame()
for (i in unique(uwvf$id)){
  vf                  <- subset(uwvf, id==i)
  powerResult         <- powerAnalysis(vf) 
  combinedPowerResult <- rbind(combinedPowerResult, powerResult) 
  write.csv(combinedPowerResult, file.path('result','powerResult.csv') ) }

# save the path to each powerCurves PDF as 'files'. Then, combine them into
# a single PDF. Finally, delete individual PDFs.
files <- list.files('plots', full.names=TRUE, pattern='powerCurves')
pdf_combine(input=files, output=file.path('plots', 'combinedPowerCurves') )
unlink(files)

#########################################################
# UWVF EXTRA: filter out series with fewer than 5 tests
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


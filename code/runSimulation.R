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
uwvf <- read.csv('data/raw_uwvf.csv') 
uwvf <- vfConfig(data='uwvf', uwvf)

for (i in unique(uwvf$id)){
  vf <- subset(uwvf, id==i)
  powerAnalysis(vf) }




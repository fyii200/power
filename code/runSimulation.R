rm(list=ls())
setwd('/Users/fabianyii/power')
# install.packages('doParallel')
# install.packages("devtools")
# install_github("imarinfr/vf1/source")

library(doParallel) # package for parallelisation
library(devtools)
library(visualFields)
source('code/powerAnalysis.R')
source('code/vfConfig.R')

#################### Power analysis on Artes2014 VF data ######################

artesVf <- read.csv('data/artes2014.csv') 
artesVf <- artesVf[,-1]
artesVf <- vfConfig(data='artes', artesVf)

for (i in unique(artesVf$id)){
  vf <- subset(artesVf, id==i)
  powerAnalysis(vf) }


#################### Power analysis on UWVF data ######################
uwvf <- read.csv('data/raw_uwvf.csv') 
uwvf <- vfConfig(data='uwvf', uwvf)

for (i in unique(uwvf$id)){
  vf <- subset(uwvf, id==i)
  powerAnalysis(vf) }




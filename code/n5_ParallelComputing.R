rm(list=ls())
setwd('/Users/fabianyii/power')
# install.packages('stringr')
# install.packages('doParallel')
library('stringr')
library(usethis)
library(doParallel)                     # package for parallelisation
registerDoParallel(8)                   # set number of cores to 8 (8 on my mac)

# install.packages("devtools")
library(devtools)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')
source('code/empty_plot.R')             # create empty plot (argument: give title)
source('code/plot_avg.R')               # fit the average curve for each px
source('code/power_analysis.R')         # power analysis (takes configured VF data and name for result folder as inputs)



#################### Power analysis on Artes2014 VF data ######################
# Read VF data
d <- read.csv('data/artes2014.csv'); d <- d[,-1]
#configure the dataset to meet visualFields data structure requirements
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)
d$id <- rep(1:30, each=12)     # make px id range from 1-30

# run analysis
power_analysis(vf_data = d, 
               unique_IDs = unique(d$id), 
               fix_interval = TRUE,
               result_name = 'artes2014_n5_power.csv')


######################## Power analysis on UWVF data ##########################
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

# Read dataset (contains only series with at least 5 VFs)
d <- read.csv('data/eligible_conf_uwvf.csv'); d <- d[,-1]
d$date <- as.Date(d$date)

# run analysis
power_analysis(vf_data = d, 
               unique_IDs = unique(d$id)[1:10], 
               fix_interval = FALSE, 
               result_name = 'uwvf_n5_power.csv')













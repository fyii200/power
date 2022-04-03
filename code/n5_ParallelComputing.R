rm(list=ls())
setwd('/Users/fabianyii/power')
# install.packages('stringr')
# install.packages('doParallel')
library('stringr')
library(usethis)
library(doParallel)                     # package for parallelisation
registerDoParallel(8)                   # set number of cores to 8 (8 on my mac)

d <- read.csv('data/power.csv')         # read the "stamps" data
d <- d[,-1]

# install.packages("devtools")
library(devtools)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')
source('code/empty_plot.R')             # create empty plot (argument: give title)
source('code/plot_avg.R')               # fit the average curve for each px

#create data frame to record power at each iteration for all patients
MD_slope <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 4)
pe <- data.frame(no=rep( 1:7, 450), id=rep(1:30, each=105), l1=0, l2=0, l3=0, l4=0, l5=0, 
                 slope=MD_slope*52/5, pow=0)

#configure the dataset to be compatable with visualFields
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)
d$id <- rep(1:30, each=12)     # make px id range from 1-30


########################### Simulation begins here #################################
dat <- data.frame(n=1:700, rate=unique(pe$slope) ,p=0)   # empty data frame to record p-val at each run
num_series <- 5                                          # how many VFs to include in a series?
set.seed(18143)

for (i in 29:30) {
  
  a <- d[which(d$id==unique(d$id)[i]),]                       # "a" = subset where px id = 'i'
  
  if(mean(apply(a[,-c(1:10,36,45)],2,mean) > 10)*52 > 8){     # check that there are >8 test locations with mean sensitivity (across 12 tests) > 10dB 
    
    #Empty plot for each px (Power vs Progression Signal; log scale for x-axis)
    empty_plot(title=paste0('Px ', unique(pe$id)[i]) )
    
    # 'k' (th) iteration (15 sets of 5 progressing locations). 
    # A power curve is produced after each 'k' iteration.
    for(k in 1:15) {
      
      # mean sensitivity across 12 tests at each location (except blind spot)
      col <- apply(a[,-c(1:10,36,45)],2,mean)
      
      # which columns (test locations) with mean sensitivity > 10dB?
      clm <- as.numeric(str_replace_all(rownames(cbind(col[which(as.numeric(col > 10) == 1)])), 'L','')) +10
      
      # random selection of 5 eligible locations (columns)
      neg <- sample(clm, 5, replace=FALSE )   
      
      # data frame to record power at K(th) iteration (for plotting purposes)
      plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                     which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
      
      # record the progressing locations (columns) in both 'plot_res' & 'pe'
      for (l in 1:5) { plot_res[,l+2] <- neg[l] }
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
      
      # 'o' (th) run (each run = 7 progression signals + PoPLR)
      for (o in 1:100) {
        a <- d[which(d$id==unique(d$id)[i]),]                     # 'a' = subset where px id = 'i'
        # a <- a[1:num_series,]                                     # select the first 'num_series' VFs
        # a <- a[sample(1:num_series, num_series, replace=FALSE),]  # randomly reorder the 'num_series' rows

        a <- a[sample(12,5),]                                     # select 'num_series' VFs from the entire series
        
        for (q in 1:4) { a$date[q+1] <- a$date[1] + 180*q }       # change test intervals to 6mths
        
        # The reordered series is evaluated here across 7 different rates of progression
        dat[which(dat$rate== unique(pe$slope)[1] )[o]: which(dat$rate== tail(unique(pe$slope),1) )[o],]$p <-  # record p-values at each run in 'dat'
          foreach (s = 1:7, .combine='c') %dopar% {                                                           # parallelisation (7 cores employed to execute lines 77-81) 
            prog <- a
            for (j in 2:num_series){ prog[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  # inject progression signal
            poplr(prog)$cslp  # run PoPLR on the modified series to return p-val
          }
        
        print(paste0('Px',i ,': ' ,k,' out of 15 iterations: ',o,' run(s) completed!'))  # 'live' report on progress in console 
      }
      
      # compute power (proportion of p<0.05) and record in 'plot_res'
      for (s in 1:7) { plot_res[s,]$pow <- length(which(dat[which(dat$rate==unique(dat$rate[s])),]$p<0.05))/100 }
      
      # copy power from 'plot_res' to 'pe' 
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 9] <- plot_res$pow  
      
      # plot power curves #
      x <- MD_slope
      lines(x ,plot_res$pow*100, col='gray' )
      
    } }
  
  plot_avg(i, legend='off')      # add average power curve
}

write.csv(pe, 'n5_power.csv')    # save result










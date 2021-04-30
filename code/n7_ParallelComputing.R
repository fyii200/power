rm(list=ls())
setwd('/Users/fabianyii/Desktop/power')
# install.packages('stringr')
# install.packages('doParallel')
library('stringr')
library(usethis)
library(doParallel)                     # package for parallelisation
registerDoParallel(3)                   # set number of cores to 3 (4 on my mac)

d <- read.csv('data/power.csv')         # read the "stamps" data
d <- d[,-1]

# install.packages("remotes")
library(remotes)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

#create data frame to record power at each iteration for all patients
pe <- data.frame(no=rep( 1: 11, 450), id=rep(1:30, each=165), l1=0, l2=0, l3=0, l4=0, l5=0, 
                 slope=0.002*2^seq(1,11,1)*54/5, pow=0)

#configure the "stamps" data 
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)
d$id <- rep(1:30, each=12)     # change px id: from 1 to 30


########################### Simulation begins here #################################
dat <- data.frame(n=1:1100, rate=unique(pe$slope) ,p=0)   # "storage" data frame to record p-val at each run
set.seed(18143) # from px 1 to 10
# set.seed(14325) # from px 11 to 20
# set.seed(18135) # from px 21 to 30

for (i in 1) {
  
  a <- d[which(d$id==unique(d$id)[i]),]    # "a" = subset where px id = 'i'
  
  if(mean(apply(a[,-c(1:10,36,45)],2,mean) > 10)*52 > 8){     # check that there are >8 test locations with mean sensitivity (across 12 tests) > 10dB 
    
    #Empty plot (Power vs Progression Signal; log scale) for each px
    plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(min(unique(pe$slope))*5/54+0.006, max(unique(pe$slope))*5/54), 
         type='n', ylab='Power', xlab='Progression Signal (dB/y)', log='x', main=paste0('Px ', unique(pe$id)[i]), xaxt='n')
    
    rate <- c(0.01,0.02,0.05,0.2,0.5,2,5)           # define 'rate'
    axis(labels=paste(-(rate)), side=1, at=rate)    # x-axis values: show (-) sign
    
    # 'k' (th) iteration (set of progressing locations)
    for(k in 1:15) {
      
      # mean sensitivity across 12 tests at each location (except blind spot)
      col <- apply(a[,-c(1:10,36,45)],2,mean)
      
      # which columns (test locations) with mean sensitivity > 10dB?
      clm <- as.numeric(str_replace_all(rownames(cbind(col[which(as.numeric(col > 10) == 1)])), 'L','')) +10
      
      # random selection of 5 eligible locations (columns)
      neg <- sample(clm, 5, replace=FALSE )   
      
      # data frame to record pow at K(th) iteration (for plotting purposes)
      plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                       which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
      
      # record the progressing locations (columns) in both 'plot_res' & 'pe'
      for (l in 1:5) { plot_res[,l+2] <- neg[l] }
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
      
      # 'o' (th) run (each run = 11 progression signals + PoPLR)
      for (o in 1:100) {
        a <- d[which(d$id==unique(d$id)[i]),]      # 'a' = subset where px id = 'i'
        a <- a[sample(1:12, 7, replace=FALSE),]    # randomly select 7 out of 12 rows
        a <- a[sample(1:7, 7, replace=FALSE),]     # randomly reorder the 7 rows
        for (q in 1:6) { a$date[q+1] <- d$date[1] + 180*q } # chg date intervals to 6mths
        
        dat[which(dat$rate==0.0432)[o]: which(dat$rate==44.2368)[o],]$p <-  # record p-values at each run in 'dat'
          foreach (s = 1:11, .combine='c') %dopar% {                        # parallelisation (3 cores employed to execute lines 77-81) 
            prog <- a
            for (j in 2:7){ prog[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  # inject progression signal
            poplr(prog)$cslp/100                                                     # run PoPLR on the modified series to return p-val
          }
        
        print(paste0('Px',i ,': ' ,k,' out of 15 iterations: ',o,' run(s) completed!'))  # 'live' report on progress in console 
      }
      
      # compute power (proportion of p<0.05) and record in 'plot_res'
      for (s in 1:11) { plot_res[s,]$pow <- length(which(dat[which(dat$rate==unique(dat$rate[s])),]$p<0.05))/100 }
      
      # copy power from 'plot_res' to 'pe' 
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 9] <- plot_res$pow  
      
      # plot power curves #
      x <- 0.002*2^seq(1,11,1)
      lines(x ,plot_res$pow, col='gray' )
      
    } }
  
  write.csv(pe, paste0('n7',i,'.csv'))    # save raw data (for each px) as csv
}

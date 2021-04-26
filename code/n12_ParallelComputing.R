rm(list=ls())
setwd('/Users/fabianyii/Desktop/power')
# install.packages('stringr')
# install.packages('doParallel')
library('stringr')
library(usethis)
library(doParallel)
registerDoParallel(3) #set number of cores (4 on my mac)
# install.packages('cachem')
library(cachem)
Sys.setenv(TZ='GMT')

d <- read.csv('data/power.csv')
d <- d[,-1]

#install latest version of visualFields
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS

# install.packages("remotes")
# library(remotes)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

#create data frame to record results
pe <- data.frame(no=1:4950, id=0, l1=0, l2=0, l3=0, l4=0, l5=0, 
                 slope=0.002*2^seq(1,11,1)*54/5, pow=0)

pe$no <- 1:11
pe$idx <- 1:165
for(i in 1:30) { pe[which(pe$idx==1)[i]:which(pe$idx==165)[i],]$id <- i }
pe <- pe[,-10]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)
for(i in 1:30) { d[which(d$id==unique(d$id)[i]),]$id <- i }


###################### Simulation begins here ##########################
dat <- data.frame(n=1:1100, rate=unique(pe$slope) ,p=0) #'storage' data frame to record p values for the 100 reordered series
set.seed(18143) # from px 1 to 10
# set.seed(14325) # from px 11 to 20
# set.seed(18135) # from px 21 to 30

for (i in 1:2) {
  
  a <- d[which(d$id==unique(d$id)[i]),]
  
  if(mean(apply(a[,-c(1:10,36,45)],2,mean) > 10)*52 > 8){
    #Empty plot for each px
    plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(min(unique(pe$slope))*5/54+0.008, max(unique(pe$slope))*5/54-2), 
         type='n', ylab='Power', xlab='Rate (dB/y)', log='x', main=paste0('Px ', unique(pe$id)[i]), xaxt='n')
    rate <- c(0.01,0.02,0.05,0.2,0.5,2,5)
    axis(labels=paste(-rate), side=1, at=rate)
    
    for(k in 1:15) {
      
      col <- apply(a[,-c(1:10,36,45)],2,mean)
      clm <- as.numeric(str_replace_all(rownames(cbind(col[which(as.numeric(col > 10) == 1)])), 'L','')) +10 #eligible columns
      neg <- sample(clm, 5, replace=FALSE ) #random selection of 5 locations (columns)
      
      plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                       which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
      
      for (l in 1:5) { plot_res[,l+2] <- neg[l] }
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
      
      
      for (o in 1:100) {
        a <- d[which(d$id==unique(d$id)[i]),]
        a <- a[sample(1:12, 12, replace=FALSE),] #random reordering (rows)
        for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
        
        dat[which(dat$rate==0.0432)[o]: which(dat$rate==44.2368)[o],]$p <-
          foreach (s = 1:11, .combine='c') %dopar% {
            prog <- a
            for (j in 2:12){ prog[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
            poplr(prog)$cslp/100
          }
        
        print(paste0('Px',i ,': ' ,k,' out of 15 iterations: ',o,' run(s) completed!'))
      }
      
      for (s in 1:11) {
        plot_res[s,]$pow <- length(which(dat[which(dat$rate==unique(dat$rate[s])),]$p<0.05))/100
      }
      
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 9] <- plot_res$pow #record pow in pe
      
      # PLOT Power vs Rate for each px #
      x <- 0.002*2^seq(1,11,1)
      lines(x ,plot_res$pow, col='gray' )
      # Plot completed #
      
    } }
  
  write.csv(pe, paste0('n12',i,'.csv'))
}
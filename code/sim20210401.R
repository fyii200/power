rm(list=ls())
setwd('/Users/fabianyii/Desktop/power')
#install.packages('stringr')
library('stringr')
library(usethis)
#install.packages('cachem')
library(cachem)
Sys.setenv(TZ='GMT')

d <- read.csv('data/power.csv')
d <- d[,-1]

#install latest version of visualFields
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS

#install.packages("remotes")
#library(remotes)
#install_github("imarinfr/vf1/source")
#install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

#create data frame to record results
pe <- data.frame(no=1:5400, id=0, l1=0, l2=0, l3=0, l4=0, l5=0, 
                 slope=c(0.002,0.002*2^seq(1,11,1))*54/5, pow=0)

pe$no <- 1:12
pe$idx <- 1:180
for(i in 1:30) { pe[which(pe$idx==1)[i]:which(pe$idx==180)[i],]$id <- i }
pe <- pe[,-10]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

for(i in 1:30){ d[which(d$id==unique(d$id)[i]),]$id <- i }


###################### Simulation begins here ##########################
dat <- data.frame(n=1:100, p=0) #'storage' data frame to record p values for the 100 reordered series
#set.seed(18143) # from px 1 to 15
set.seed(18135) # from px 16 to 30

for (i in 16:30){
  
  a <- d[which(d$id==unique(d$id)[i]),]
  
  if(mean(apply(a[,-c(1:10,36,45)],2,mean) > 10)*52 > 8){
    #Empty plot for each px
    plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.002,4.096), type='n', ylab='Power',
         xlab='Rate (- dB/y)', main=paste0('Px ', unique(d$id)[i]), log='x')
    
    for(k in 1:15){
      
      col <- apply(a[,-c(1:10,36,45)],2,mean)
      clm <- as.numeric(str_replace_all(rownames(cbind(col[which(as.numeric(col > 10) == 1)])), 'L','')) +10 #eligible columns
      neg <- sample(clm, 5, replace=FALSE ) #random selection of 5 locations (columns)
      
      plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                       which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
      
      for(l in 1:5){ plot_res[,l+2] <- neg[l] }
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
      
      for(s in 1:12 ){
        
        for(o in 1:100){
          a <- d[which(d$id==unique(d$id)[i]),]
          a <- a[sample(1:12, 12, replace=FALSE),] #random reordering (rows)
          for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
          
          for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
          
          # for(l in 1:5){a[,neg[l]][which(a[,neg[l]]<0)] <- 0} #Truncate sensitivity at 0dB
          dat$p[o] <- poplr(a)$cslp/100
        }    
        
        plot_res[,9][s] <- length(which(dat$p<0.05))/100 #record power for each slope for each px in pe
        print(length(which(dat$p<0.05))/100) #to delete
      }
      
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 9] <- plot_res$pow #record pow in pe
      
      # PLOT Power vs Rate for each px #
      x <- c(0.002,0.002*2^seq(1,11,1))
      lines(x ,plot_res$pow, col='gray' )
      # Plot completed #
      
    } }
  
  write.csv(pe, paste0('sim',i,'.csv'))
  
}

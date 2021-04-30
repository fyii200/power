rm(list=ls())
setwd('/Users/fabianyii/Desktop/power')

# install.packages("remotes")
library(remotes)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

# read raw data (power) into r
d <- read.csv('data/n7_RawData/n7.csv')
d <- d[,-1]

# data frame ('avg') to record the average power for each px
avg <- data.frame(id=rep(1:30, each=11), slope=unique(d$slope), pow=0)

# compute the average power for each px
for(i in 1:30){
    for(s in 1:11){
      avg[which(avg$id==unique(d$id)[i]),]$pow[s] <-
      mean(d[which(d$id==unique(d$id)[i] & d$slope==unique(d$slope)[s]),]$pow) 
      }
}

################# Power vs Progression Signal (Individual) #####################
pdf(file='indiv.pdf', width=16, height=12)
# par(mfrow=c(3,5), las=1)

col <- c(brewer.pal(12, 'Paired'), brewer.pal(8, 'Dark2'),  # 30 colour codes for 30 px
         brewer.pal(8, 'Accent'), brewer.pal(8, 'Set2') )

for(i in 21){
  plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(min(unique(d$slope))*5/54+0.006, max(unique(d$slope))*5/54), log='x',xaxt='n',
       type='n', ylab='Power', xlab='Progression Signal (dB/y)', main=paste0('Px ',unique(d$id)[i]) )
  
  rate <- c(0.01,0.02,0.05,0.2,0.5,2,5)           # define 'rate'
  axis(labels=paste(-(rate)), side=1, at=rate)    # x-axis values: show (-) sign
  
  # 's' (th) power curve
  for(s in 1:15) { 
    idx <- d[row.names(d[which(d$id==unique(d$id)[i] & d$no==1),][s,]) :     # subset: data for 1 curve
               row.names(d[which(d$id==unique(d$id)[i] & d$no==11),][s,]),] 
    lines(idx$slope*5/54, idx$pow, col='gray', lwd=1)                        # plot individual curves (not smoothed)
  }
  
  plot_avg <- subset(avg, id==i)                                             # subset where px id = 'i': average of 15 curves
  plot_avg$slope <- plot_avg$slope*5/54                                      # chg progression to MD terms
  fit_glm <- glm(pow ~ slope, family=binomial(link='logit'), data=plot_avg)  # fit logit model
  lines(seq(0.0001,4,0.01),                                                  # fit average curve (logit)
        predict(fit_glm, data.frame(slope=seq(0.0001,4,0.01)), type='response'), col=col[i], lwd=6)               
}
dev.off()
################################## Completed ########################################

#################### Power vs Progression Signal Combined) ##########################
pdf(file='CombPowVsSlope.pdf', width=6, height=6)
par(las=1)
plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(min(unique(d$slope))*5/54+0.006, max(unique(d$slope))*5/54), 
     type='n', ylab='Power', xlab='Progression Signal (dB/y)', log='x', main='Power vs Rate of Progression (MD)', xaxt='n')

axis(labels=paste(-(rate)), side=1, at=rate)               # x-axis values: show (-) sign

# 'i' (th) px
for(i in 1:30){
  
  plot_avg <- subset(avg, id==i)                                             # subset where px id = 'i': average of 15 curves
  plot_avg$slope <- plot_avg$slope*5/54                                      # chg progression to MD terms
  fit_glm <- glm(pow ~ slope, family=binomial(link='logit'), data=plot_avg)  # fit logit model
  lines(seq(0.0001,4,0.01),                                                  # fit average curve (logit)
        predict(fit_glm, data.frame(slope=seq(0.0001,4,0.01)), type='response'), col=col[i], lwd=3)     
}
dev.off()
#################################### Completed ############################################









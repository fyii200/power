rm(list=ls())
setwd("/Users/fabianyii/Desktop/power")
library(visualFields)

d <- read.csv('data/power.csv')
d <- d[,-1]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

# ## Sam's dataset
# d <- read.csv('data/sam.csv')
# d <- d[,-c(1:2,67:71)]
# d$date <- as.Date(d$date, '%m/%d/%Y')

spv <- function(dt){
  lv <- dt
  
  x <- vector('numeric',length(unique(d$id)))
  for(i in 1:length(unique(d$id))){
    idx <- d[which(d$id==unique(d$id)[i]),]
    md <- apply(gettd(idx)[,-c(1:10,36,45)],1,mean)
    x[i] <- abs(max(md) - min(md)) }
  
  pdf(file='spv.pdf', width=25, height=30)
  for(i in unique(lv[,1])) {
    layout(matrix(c(1,1,1,1,1,
                    2,2,2,3,3,
                    4:23,
                    24,24,24,24,24)
                  ,nrow=7,ncol=5,byrow=TRUE),
           heights=c(1.1,2.2,1.3,1.3,1.3,1.3,0.4)
           , widths=c(0.5,3,3,3,3))
    idx <- lv[which(lv[,1]==i),]
    
    par(mar=c(0,8,4,2))
    plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, ann=FALSE, type='n')
    text(0.252,0.75, labels='Serial Visual Fields Visualisation', cex=7, font=2)
    text(0.21,0.5, labels='Static Automated Perimetry with the SITA Standard Strategy', cex=3.4, font=1)
    text(0.209,0.35, labels='Stimulus Size: Size III           SUNY-IU classic NVs for 24-2', cex=3.4, font=1)
    
    par(mar=c(15,14.5,5,6) )
    md <- apply(gettd(idx)[,-c(1:10,36,45)],1,mean)
    int <- (max(x) - abs(max(md)-min(md)))/2
    plot(min(idx$date[1:nrow(idx)])-10, 0, bty='n', cex.axis=2.5, cex.main=2, xlab='', ylab='', yaxs='i',
         ylim=c(min(md)-int, max(md)+int), xlim=c(min(idx$date[1:nrow(idx)]), max(idx$date[1:nrow(idx)])) )
    legend('left',legend='MD', text.col='gray88', cex=18, bty='n')
    if(nrow(idx)>1 & idx$date[1] != idx$date[2] ){
      lines(lowess(idx$date[1:nrow(idx)], md ), lwd=7, lty=1, col='green')
      abline(a=lm(md~idx$date)$coefficients[1],
             b=lm(md~idx$date)$coefficients[2], col='gray', lwd=7, lty=1)
      points(idx$date[1:nrow(idx)], md, cex=5, col='maroon', pch=19)
      legend('topright',c('LOESS', 'OLSR'), lty=c(1,1), bty='n', col=c('green', 'gray'),cex=2, lwd=8 )} else{print('NA')}
    
    par(mar=c(9,0,0,5))
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.88,0.92, legend=c(paste0('ID: ',idx$id[1]), paste0('Age: ',min(idx$age),' to ',max(idx$age)),
                               paste0('FPR: ', round(mean(idx$fpr),digits=1), ' (',min(idx$fpr), ' to ', max(idx$fpr), ')'),
                               paste0('FNR: ', round(mean(idx$fnr),digits=1), ' (',min(idx$fnr), ' to ', max(idx$fnr), ')'),
                               paste0('FL: ', round(mean(idx$fl),digits=1), ' (',min(idx$fl), ' to ', max(idx$fl), ')'),
                               paste0('MD: ', round(mean(md),digits=1), ' (',round(min(md),digits=1), ' to ', round(max(md),digits=1),')') ),
           cex=3.4, bty='n', y.intersp=1.9, adj=1)
    
    
    par(mar=c(0,0,0,0))
    plot.new()
    for (i in 1:3) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=0.9) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.21,0.91, legend=c(idx$date[1:3]), cex=2.6, bty='n', y.intersp=2)
    
    plot.new()
    for (i in 4:6) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=0.9) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.21,0.91, legend=c(idx$date[4:6]), cex=2.6, bty='n', y.intersp=2)
    
    plot.new()
    for (i in 7:9) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=0.9) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.21,0.91, legend=c(idx$date[7:9]), cex=2.6, bty='n', y.intersp=2)
    
    plot.new()
    for (i in 10:12) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=0.9) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.21,0.91, legend=c(idx$date[10:12]), cex=2.6, bty='n', y.intersp=2)
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  }
  dev.off() }

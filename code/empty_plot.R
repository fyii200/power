empty_plot <- 
 function(title){
  par(las=1)
  plot(0, 0, bty='n', pch=19, cex=0.4, ylim=c(0,1), log='x',xaxt='n', type='n',  # empty plot 'i'th px
     xlim=c(min(unique(d$slope))*5/52+0.006, max(unique(d$slope))*5/52),
     ylab='Power', xlab='Progression Signal (dB/y)', main=title)
  rate <- c(0.01,0.02,0.05,0.2,0.5,2,5)                                          # define 'rate'
  axis(labels=paste(-(rate)), side=1, at=rate)                                   # x-axis: show (-) sign
 }
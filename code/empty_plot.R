empty_plot <- 
 function(data, title){
  par(las=1)
  plot(0, 0, bty='n', pch=19, cex=0.4, ylim=c(0,100), log='x',xaxt='n', yaxt='n', type='n',  # empty plot 'i'th px
     xlim=c(0.05, 4), ylab='Detectability of Progression (%)', xlab='Progression Signal (dB/y)', main=title)
  rate <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 4)                                          # define 'rate'
  axis(labels=paste(-(rate)), side=1, at=rate, cex.axis=1.6)                       # x-axis: show (-) sign
 }
empty_plot <- 
 function(d, title){
  par(las=1)
  plot(0, 0, bty='n', pch=19, cex=0.4, ylim=c(0,100), log='x',xaxt='n', type='n',  # empty plot 'i'th px
     xlim=c(min(unique(pe$slope))*5/52, max(unique(pe$slope))*5/52),
     ylab='Detectability of Progression (%)', xlab='Progression Signal (dB/y)', main=title)
  rate <- c(0.06,0.20,0.50,2.00,4.1)                                          # define 'rate'
  axis(labels=paste(-(rate)), side=1, at=rate)                                   # x-axis: show (-) sign
 }
plot_avg <- function(data, px, legend) {
  a <- subset(data, id==px)                                              # 'a' is a subset of 'pe' where id = 'px'
  
  
  pow_avg <- data.frame(prog=unique(a$slope)*5/52,                   # empty data frame to record average power
                        pow=0)                                       # at each of the 11 progression signals (MD)
                                                                 
  
  for (i in 1:length(unique(a$slope)) ){                             # mean of power at each progression signal 
  pow_avg$pow[i] <- mean(subset(a, slope == unique(a$slope)[i])$pow) 
  }
  
  lines(pow_avg$prog, pow_avg$pow*100, lwd=4, col=col[px])
  
  txt <- round( pow_avg$pow[ which(pow_avg$prog == 1) ]*100, digits=2)
  abline(h=txt, v=1, lty=2)
  legend('left', bty='n', paste0(txt, '%'), title='-1dB/y', text.col='gray', cex=1.6, lty=2, lwd=2.5)
  
  # fit_glm <- glm(pow~prog, family=binomial(link='logit'), data=pow_avg)      # logit model
  # 
  # predict_dat <- data.frame(x=seq(0.01, 4.1, 0.001) , y=0)                  # 'predict_dat' = data frame to store predictor x and predicted y
  # 
  # predict_dat$y <- predict(fit_glm, data.frame(prog=predict_dat$x), type='response') # give predicted power (y) given progression (x)
  # lines(predict_dat$x, predict_dat$y, lwd=4, col=col[px])                            # fit the average curve
  
  # # progression signal (x) when power (y) = 0.8 #
  # if(legend=='on'){                               # argument: 80% power progression signal to be shown? 
  # r <- which(round(predict_dat$y,2) == 0.8)       # which row(s) in 'predict_dat'?
  # 
  # ps <- round( predict_dat[r,]$x[1], digits=2)    # what's the 'ps' progression signal when pow=0.8?
  #                                         
  # abline(h=0.8, v=ps, lty=2, col=col[px])         # show the line corresponding to 'ps'
  # 
  # legend(0.8, 0.5, bty='n', paste0('-',ps),       # print progression signal when power = 0.8
  #        title='80% power', text.col='gray', 
  #        cex=1.2, lty=2, col=col[px], lwd=2.5)
  # }
}






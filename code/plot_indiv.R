plot_indiv <- function(data, px){
  a <- subset(data, id==px)                                # 'a' is a subset of 'd' where id = 'px'
  
  
  for(i in 1:15){                                          # 'itr' = one location configuration ('i'th iteration)
    itr <- a[(i+6*(i-1)):(7*i),]
    lines(itr$slope*5/52, itr$pow*100, col='gray', lwd=1)  # plot power vs progression signal (in MD terms) 
  }
}
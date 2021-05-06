plot_indiv <- function(px){
  a <- subset(d, id==px)                                # 'a' is a subset of 'd' where id = 'px'
  
  
  for(i in 1:15){                                       # 'itr' = one location configuration ('i'th iteration)
    itr <- a[(i+10*(i-1)):(11*i),]
    lines(itr$slope*5/52, itr$pow, col='gray', lwd=1)   # plot power vs progression signal (in MD terms) 
  }
}
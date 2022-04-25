# Function that injects progression signal to 5 randomly selected VF locations #
# and run PoPLR on the modified VF series. This is repeated 100 times and      #
# the proportion of statistically significant results (p<0.05) is treated as   #
# the power to detect a particular rate of progression. Only the first 5 VFs   #
# in each series is used by default.                                           #

# 
powerAnalysis <- function(vf_data, unique_IDs, fix_interval, result_name){
  ### 'vf_data'     : (data frame) configured dataset as per 'VisualFields' package requirements.
  ### 'unique_IDs'  : (int) Vector of unique participant ID for each VF series. 
  ### 'fix_interval': (Bool) If 'True' test intervals between VFs in each series = 6 months; actual intervals used if 'False'.
  ### 'result_name' : (Char) Name of the CSV result.
  
  # Create data frame to record power at each iteration for each patient.
  MD_slope <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 4)
  pe <- data.frame( no = rep( 1:7, 15*length(unique_IDs)), 
                    id = rep(unique_IDs, each=105), 
                    l1=0, l2=0, l3=0, l4=0, l5=0, 
                    slope = MD_slope*52/5, 
                    pow = 0)
  
  # Empty data frame to record p-val at each run
  dat <- data.frame(n=1:700, rate=unique(pe$slope) ,p=0)   
  # How many VFs to include in a series?
  num_series <- 5      
    
  # "a" = subset where px id = 'i'.
  a <- vf_data[ which(vf_data$id == unique_IDs[i]), ]
  # Select the first 'num_series' VFs
  a <- a[1:num_series,]                                    
    
  # Check that there are >8 test locations with mean sensitivity (across first 5 tests) > 10dB 
  if(mean(apply(a[, -c(1:10,36,45)] , 2, mean) > 10)*52 > 8){    
      
  # Empty plot for each px (Power vs Progression Signal; log scale used for x-axis)
  empty_plot(title = unique_IDs[i])
      
  # Mean sensitivity across 12 tests at each location (except blind spot)
  col <- apply(a[, -c(1:10,36,45)], 2, mean)
      
  # Which columns (test locations) with mean sensitivity > 10dB?
  clm <- as.numeric(str_replace_all(rownames(cbind(col[which(as.numeric(col > 10) == 1)])), 'l','')) +10
      
  # 'k' (th) iteration (15 sets of 5 progressing locations). 
  #  Each 'k' iteration produces an individual power curve.
  for(k in 1:15) {
    
    # Random selection of 5 eligible locations (columns)
    neg <- sample(clm, 5, replace=FALSE )   
        
    # Data frame to record power at K(th) iteration (used for plotting the power curves).
    plot_res <- pe[ which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                      which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
    
    # Record the progressing locations (columns) in both 'plot_res' & 'pe'.
    for (l in 1:5) { plot_res[,l+2] <- neg[l] }
    pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] :
         which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], 3:7] <- plot_res[,3:7]
    
    # 'o' (th) run (each run = 7 progression signals + PoPLR)
    for (o in 1:100) {
      # "a" = subset where px id = 'i'
      a <- vf_data[ which(vf_data$id == unique_IDs[i]), ]
      # Select the first 'num_series' VFs
      a <- a[1:num_series,] 
      # Original date sequence
      ori_date_seq <- a$date
      # Randomly reorder the 'num_series' rows
      a <- a[ sample(1:num_series, num_series, replace=FALSE), ]
      
      # If 'fix_interval' is true, fix test intervals to 6 months, 
      # otherwise use actual test intervals.
      if (fix_interval==TRUE){
        for (q in 1:4) {
          a$date[q+1] <- a$date[1] + 180*q
          intervals <- append(0,rep(0.5,4))
          # Cumulative intervals
          cum_intervals <- cumsum(intervals) }  
        } else {
          a$date <- ori_date_seq
          # Compute avtual test intervals (in years)
          intervals <- c(0)
          for (q in 1:4){
            interval <- as.double(difftime(a$date[q+1], a$date[q], units='days') / 365) 
            intervals <- append(intervals, interval)
            # Cumulative intervals
            cum_intervals <- cumsum(intervals) }
          }
      
      # The reordered series is evaluated here across the full range of progression signals.
      dat[which(dat$rate== unique(pe$slope)[1] )[o]: which(dat$rate== tail(unique(pe$slope),1) )[o],]$p <-  # Record p-values at each run in 'dat'.
        foreach (s = 1:7, .combine='c') %dopar% {                                                           # Parallelisation (7 cores employed to execute lines 99-101). 
          prog <- a
          # Inject progression signal of a particular magnitude (7 in total).
          for (j in 2:num_series){ prog[j,neg] <- prog[j, neg] - unique(pe$slope)[s]*cum_intervals[j] }  
          # Run PoPLR on the modified series to return p-val
          poplr(prog)$cslp  
        }
      }
    
    # Compute power (proportion of p<0.05) and record result in 'plot_res'.
    for (s in 1:7) { plot_res[s,]$pow <- length(which(dat[which(dat$rate==unique(dat$rate[s])),]$p<0.05))/100 }
    
    # Copy power from 'plot_res' to 'pe'. 
    pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], 9] <- plot_res$pow  
    
    ## Plot an individual power curve ##
    x <- MD_slope
    lines(x ,plot_res$pow*100, col='gray' )
    
    # Progress tracker.   
    print(paste0('Px ', unique_IDs[i], ': ' ,k,' out of 15 iterations completed!'))  
    
    } }
  
  # Add average power curve.
  plot_avg(data=pe, px=unique_IDs[i], legend='off')              
  axis(2, cex.axis=1.6)
  
  # Save result.
  write.csv(pe, paste0('result/', result_name) )    
}







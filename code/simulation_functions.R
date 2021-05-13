################# Repertoire of basic functions ######################

# return 1 specific px (specify 'id') series #
select.series <- function(data ,id){
                 subset(data[which(data$id==id),]) 
                 }

# check which locations are blind (return corresponding columns) #
is.blind <- function(data){
            a <- subset(select.series(data, data$id), select=L1:L54)    # 'a' = 1 px series (only sensitivity columns)
            bs <- c(35, 45)                                             # physiological blind spot (columns) 
            c(as.vector(which(apply(a[,-c(35,45)],2,mean)==0)+10), bs)  # return columns with ms = 0dB
            }

# randomly sample 'n' number of non-blind locations (return columns) #
sample.eligible.column <- function(test.series, n){
                          xclude <- is.blind(test.series) - 10           # identify blind locations
                          sensitivity.columns <- 11:64                   # a vector of all columns associated with 54 test locations                   
                          eligible <- sensitivity.columns[-xclude]       # exclude blind locations
                          sample(eligible, n)                                       
                          } 

# return a seq of dates with specified intervals (month_int; in mth) #
date_int <- function(data, id, month_int){              
            a <- select.series(data, id)
            first <- a$date[1]                                         # 1st date in the original series
            last <- first + (30*month_int)*nrow(a)                     # compute last date after changing intervals
            seq(first, last, length.out=nrow(a) )                      # return a seq of modified dates with desired int.          
            }

# return 1 re-ordered px series with test intervals = 'month_int' #
shuffle <- function(data, id, month_int ,nrow=12){
           a <- select.series(data, id)
           a <- a[sample(1:nrow(a), nrow, replace=F),]                   # what's the desired total number of rows in the reordered series? 
           a$date <- date_int(data, id, month_int)                       # set test intervals to 'month_int' months
           return(a)
           }

# return progression signals (test intervals = 6mths by default) #
prog <- function(annual.rate, nrow, month_int=6){               # 'nrow' = how many rows in a series?
        signal_1st_int <- annual.rate/ (12/month_int)           # signal for the 1st 'month_int'(th) month
        signal_last_int <- signal_1st_int * (nrow-1)            # signal for the last 'month_int'(th) month
        signal <- seq(signal_1st_int, signal_last_int,          # seq of progression signals;
                  length.out=nrow-1)                            # increasing linearly
        c(0,signal)                                             # '0' = no progression in year 0
        }

# return power corresponding to each of the total number of 
# progression signals ('rate' = vector of all progression signals)
compute.power <- function(data, rate){
                 power <- vector('numeric', length(rate) )               # empty vector to store power
                   for(i in seq_along(rate) ){
                   p <- subset(data, rate==rate[i], select=p_val)        # subset containing only p values from a specific progression signal
                   power[i] <- sum(p<0.05) / nrow(p)                     # compute power       
                   }
                 return(power)
                 }





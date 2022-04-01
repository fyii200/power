# function to configure raw data so 'visualFields' can be run 
# 'npx' = how many patients in total ; 'nrow' = how many rows within each px series

vf_config <- function(data, npx, nrow){
  # include only relevant columns
  data <- data[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
  
  # change column names to what visualFields package recognises
  names(data)[c(2:10)] <- c('eye','date','time', 'age','type',
                            'fpr', 'fnr','fl','duration')
  
  data$date <- as.Date(data$date)
  
  # change px id such that 1st px = 1, 2nd px = 2...
  data$id <- rep(1:npx, each=nrow)
  data <- return(data)
}

# install.packages("lubridate")
library("lubridate")

vfConfig <- function(data, vf, saveData=FALSE, saveDir='data'){
  
  if(data=='artes'){
    # include only relevant columns
    confVf <- vf[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
    # change column names to what visualFields package use
    names(confVf)[c(2:10)] <- c('eye','date','time', 'age','type', 'fpr', 'fnr','fl','duration')
    confVf$date <- as.Date(confVf$date)
    # change px id such that 1st px = 1, 2nd px = 2...
    confVf$id <- rep(1:30, each=12) }
    
  
  if(data=='uwvf'){
    confVf <- data.frame(id = 1:nrow(vf))
    confVf$id <- vf$PatID
    confVf$eye <- vf$Eye
    rightEyeRows = which(confVf$eye == 'Right') 
    leftEyeRows = which(confVf$eye == 'Left')
    confVf$eye[rightEyeRows] <- 'OD' 
    confVf$eye[leftEyeRows] <- 'OS'
    
    # First VF test assumed to be taken on 01/01/2000
    confVf$date <- as.Date(vf$Time_from_Baseline, origin = '2000-01-01')
    confVf$date <- confVf$date %m+% months( round(vf$Time_from_Baseline*12) )
    
    confVf$time <- 0
    confVf$age <- vf$Age
    confVf$type <- 'unknown'
    confVf$fpr <- 0
    confVf$fnr <- 0
    confVf$fl <- 0
    confVf$duration <- 0
    confVf <- cbind(confVf, vf[,23:76])
    
    thresholds <- c()
    for (i in 1:54){ thresholds = c(thresholds, paste('L', i, sep='')) }
    names(confVf)[11:64] <- thresholds
    
    # include eye information in id, e.g. change id '647' to '647_OD'
    confVf$id <- paste0(confVf$id, '_', confVf$eye) }
    
  
  if(saveData){
    fileName <- paste0(data, 'Configured')
    write.csv(confVf, file.path(saveDir,fileName) ) }
  
  return(confVf) }


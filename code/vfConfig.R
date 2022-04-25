#' @rdname vfConfig
#' @title Configure visual field data
#' @description `vfConfig` takes either the visual field data from Artes et al. (2014)
#'   which is available as `vfpwgRetest24d2` from R \code{visualFields} or Montesano et al. 
#'   (2022) which is available as VF_Data.csv via the [link]: https://github.com/uw-biomedical-ml/uwhvf/tree/master/CSV
#'   and returns a \code{visualFields}-compatible data frame. 
#'  @return `vfConfig` returns a data frame with the following columns
#'  \itemize{
#'    \item id: patient ID (character string).
#'    \item eye: patient's eye (character string).
#'    \item date: test date (date object), year-month-day.
#'    \item time: time at which test was taken (numeric).
#'    \item age: age when test was taken (numeric).
#'    \item type: type of glaucoma (character string), if known.
#'    \item fpr: false positive rate (numeric).
#'    \item fnr: false negative rate (nuemric).
#'    \item fl: fixation losses (nuemric).
#'    \item duration: duration of test (character).
#'    \item L1..L54: threshold sensitivity at each visual field location (integer or numeric).
#'}  
#' @references
#' P. H. Artes, N. O'Leary, M. T. Nicolela, B. C. Chauhan, and D. P. Crabb.
#' \emph{Visual field progression in glaucoma: What is the specificity of the
#' guided progression analysis?} American Academy of Ophthalmology,
#' 121(10):2023-2027, 2014.
#' 
#' Giovanni Montesano, Andrew Chen, Randy Lu, Cecilia S. Lee, and Aaron Y. Lee. 
#' \emph{UWHVF: A Real-World, Open Source Dataset of Perimetry Tests From the 
#' Humphrey Field Analyzer at the University of Washington.} Trans. Vis. Sci. Tech., 
#' 11(1):2, 2022.
#' @examples
#' artesVf <- vfConfig(data='artes', vf=artesVf) # configure artesVF 
#' uwvf <- vfConfig(data='uwvf', uwvf) # configure university of washington VF data 
#' @param data A data frame: raw visual field data (downloaded 'as if').
#' @param vf A Character string, 'artes' if data from Artes et al. (2014) are
#'   used; 'uwvf' if data from Montesano et al. (2022) are used.
#' @param saveData A boolean indicating if the configured data frame is to be 
#'   saved (as CSV) inside a directory specified by \code{savePath}. FALSE by default.
#' @param savePath desired path to which the configured data frame should be saved. 
#'   A relative path called 'data' is used by default.
#' @export
vfConfig <- function(data, vf, saveData=FALSE, savePath='data'){
  if(data=='artes'){
    # include only relevant columns
    confVf <- vf[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
    # define column names as per visualFields package
    names(confVf)[c(2:10)] <- c('eye','date','time', 'age','type', 'fpr', 'fnr','fl','duration')
    confVf$date <- as.Date(confVf$date)
    # change px id such that 1st px = 1, 2nd px = 2...
    confVf$id <- rep(1:30, each=12) }
    
  if(data=='uwvf'){
    confVf                   <- data.frame(id = 1:nrow(vf))
    confVf$id                <- vf$PatID
    confVf$eye               <- vf$Eye
    rightEyeRows             <- which(confVf$eye == 'Right') 
    leftEyeRows              <- which(confVf$eye == 'Left')
    confVf$eye[rightEyeRows] <- 'OD' 
    confVf$eye[leftEyeRows]  <- 'OS'
    
    # first VF test assumed to be taken on 01/01/2000
    confVf$date <- as.Date(vf$Time_from_Baseline, origin = '2000-01-01')
    confVf$date <- confVf$date %m+% months( round(vf$Time_from_Baseline*12) )
    
    confVf$time     <- 0
    confVf$age      <- vf$Age
    confVf$type     <- 'unknown'
    confVf$fpr      <- 0
    confVf$fnr      <- 0
    confVf$fl       <- 0
    confVf$duration <- 0
    confVf          <- cbind(confVf, vf[,23:76])
    
    thresholds <- c()
    for (i in 1:54){ thresholds <- c(thresholds, paste('L', i, sep='')) }
    names(confVf)[11:64] <- thresholds
    
    # include eye information in id, e.g. change id '647' to '647_OD'
    confVf$id <- paste0(confVf$id, '_', confVf$eye) }
  
  if(saveData){
    fileName <- paste0(data, 'Configured')
    write.csv(confVf, file.path(savePath,fileName) ) }
  
  return(confVf) }


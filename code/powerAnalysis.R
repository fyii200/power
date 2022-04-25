#' @rdname powerAnalysis
#' @title Power analysis for visual fields
#' @description `powerAnalysis` estimates the power of PoPLR in a given series of visual fields. 
#' @details This is a function that takes a series of visual fields as input and estimates the 
#'   statistical power of permutation of pointwise linear regression (PoPLR) as proposed by
#'   O'Leary et al. using Monte Carlo simulations. Briefly, linear progression signals of varying
#'   magnitudes are introduced to 5 randomly selected visual field locations. The modified series is
#'   then analysed for the presence of statistically significant progression using PoPLR. Iterated many
#'   times, the power to detect progression in the series concerned can be estimated as
#'   the proportion of statistically significant results (p<0.05).
#' @return `powerAnalysis` returns the following
#'\itemize{
#'  \item A data frame which consists of the following columns.
#'    \item\code{id} A character string representing patient ID.
#'    \item\code{eye} A character string representing patient's eye.
#'    \item\code{meanAge} A numeric scalar representing the average age across the visual field series
#'      used in the analysis.
#'    \item\code{meanMD} A numeric scalar representing the average mean deviation across the visual
#'      field series used in the analysis.
#'    \item\code{originalSeriesPoplr} A numeric scalar representing the p-value for the left-tailed permutation 
#'      test (i.e. concerned with deterioration) when analyzing the original (i.e. non-permuted) series with PoPLR. 
#'    \item\code{powerCurve} An integer scalar representing the index of power curves. Each power curve describes 
#'      the power estimates across a range of progression signals.
#'    \item\code{mdRates} A numeric scalar specifying the magnitude of progression signal, expressed as mean deviation. 
#'    \item\code{power} A numeric scalar representing the power estimate for a given magnitude of progression signal, 
#'      expressed as percentage.
#'    \item\code{dateRangeInMonths} An integer scalar representing the date range in months during which the first
#'      and final visual field tests are taken, or assumed to be taken if the argument \code{fixedInterval} is set 
#'      to \code{true}.
#'  \item A plot depicting the power estimates across the full range of progression magnitudes in mean deviation 
#'    terms. Saved in the default subdirectory \code{plots} if \code{savePlot} is set to \code{TRUE}, otherwise 
#'    displayed inside the Plots pane.
#'}
#'@references
#' N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in
#' glaucoma: estimating the overall significance of deterioration with permutation
#' analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology
#' and Visual Science, 53, 2012
#' @examples
#' artesVf <- vfConfig(data='artes', artesVf) # configure artesVF
#' vf <- subset(artesVf, id==1) # select patient 1's series of VFs
#' powerResult <- powerAnalysis(vf) # run power analysis on this series and save result (data frame) as 'powerResult'
#'@param vf A data frame storing pertinent data from a series of visual fields at individual- and eye-level. Structure 
#'  must conform to R \code{visualFields} package's requirements.
#'
#'@param useNumCores An integer scalar representing the number of CPU cores to be used for parallel execution using 
#'  R \code{doParallel} package. All available cores are used by default, determined using the \code{detectCores} function.
#'
#'@param mdRates A numeric vector specifying the magnitudes of progression signals expressed in mean deviation terms. c(-0.05,
#'  -0.10, -0.20, -0.50, -1.00, -2.00, -4.00) dB/year is used by default, which corresponds to c(-0.52, -1.04, -2.08, -5.20, 
#'  -10.40, -20.80, -41.60) dB/year in pointwise terms, i.e. pointwise progression = \code{mdRates}*52/5.  
#'
#'@param nSeries An integer scalar specifying the first \code{n} visual field tests to be analyzed in each series. Default is 5.
#'
#'@param fixedInterval A logical scalar. If set to \code{TRUE}, test intervals are fixed at 6 months. Hence, assuming the
#'  default \code{nSeries}=5 setting, the first and last tests span 24 months which gives rise to, say, a progression signal 
#'  of -2dB for a progression rate of -1dB/year. If set to \code{FALSE}, the actual test intervals are used. Default is \code{TRUE}.
#'
#'@param nPowerCurves An integer scalar specifying the number of power curves one desires. Each power curve depicts the power
#'  estimates across a pre-defined range of progression magnitudes (n=7 if the default \code{mdRates} is used) introduced to 
#'  **a specific set of 5 randomly sampled visual field locations** (excluding blind-spot locations). Hence, a different power
#'  curve corresponds to a different set of modified visual field locations. Default is 15.
#'
#'@param nPermutationsPerCurve An integer scalar specifying the number of times a given series is randomly permuted aka reordered
#'  before being modified by adding progression signals for each power curve. As PoPLR is performed each time a series is reordered
#'  and modified, \code{nPermutationsPerCurve} corresponds to the number of p-values generated for each progression magnitude, and
#'  the proportion of which is a measure of power at that given magnitude. Note that the maximum number of permutations is 120 (5!)
#'  using the default \code{nSeries}=5, which should smaller than \code{nPermutationsPerCurve}. Default is 100.
#'
#'@param savePlot A logical scalar. If set to \code{TRUE}, a PDF of the plot depicting power(%) vs rates of progression(dB/year) 
#'  will be saved to \code{savePlotPath}. If \code{FALSE}, the plot will instead be displayed in the Plots pane. Default is TRUE.
#'
#'@param savePlotPath Path specifying where the plot depicting the power curves for a given series should be saved to. Default 
#'  is a relative path called 'plots'.
#'@export
powerAnalysis <- function(vf, 
                          useNumCores            = detectCores(),
                          mdRates                = c(0.05, 0.1, 0.2, 0.5, 1, 2, 4),
                          nSeries                = 5, 
                          fixedInterval          = TRUE, 
                          nPowerCurves           = 15, 
                          nPermutationsPerCurve  = 100, 
                          savePlot               = TRUE,
                          savePlotPath           = 'plots'){
  
  registerDoParallel(cores=nCores)
  
  pointwiseRates <- mdRates*52/5
  
  if(nrow(vf)>4 & seriesIsEligible(vfSeries) )
    
    vfSeries <- vf[1:nSeries, ]
    powerResult <- createEmptyDataframe(vfSeries, nPowerCurves, mdRates)
    if(fixedInterval) vfSeries <- fixToSixMonths(vfSeries) 
    
    eligibleColumns <- eligibleColumns(vfSeries)
    
    for(powerCurve in 1:nPowerCurves) {
      
      fiveVFLocations <- sample(eligibleColumns, 5, replace=FALSE )   
      
      pValues <- c()
      for (permutation in 1:nPermutationsPerCurve) {
        reorderedSeries        <- reorderSeries(vfSeries)
        onePValueForEachMdRate <- onePowerRun(reorderedSeries, pointwiseRates, fiveVFLocations)
        pValues                <- append(pValues,  onePValueForEachMdRate) }
      
      powers <- computePowers(pValues)
      
      powerCurveRows <- which(powerResult$powerCurve == powerCurve)
      powerResult[powerCurveRows, ]$power <- powers }
  
  plotPowerCurves(powerResult, unique(vf$id), mdRates, savePlot, savePlotPath)
  return(powerResult) }

###################################################################################
# INTERNAL FUNCTIONS
###################################################################################
# Internal function: creates and return an empty data frame to store pertinent
#   information and result of power analysis (power expressed as percentage) for a
#   given visual field series. Column names explained in detail under @return tag.
#' @noRd
createEmptyDataframe <- function(vfSeries, nPowerCurves, mdRates){
  firstDate           <- head(vfSeries$date,1)
  lastDate            <- tail(vfSeries$date,1)
  dateRangeInDays     <- difftime(lastDate, firstDate)
  dateRangeInMonths   <- round(as.double(dateRangeInDays/30), 2)
  totalDeviations     <- gettd(vfSeries)[,11:64]
  meanMD              <- mean(as.matrix(totalDeviations), na.rm=TRUE)
  originalSeriesPoplr <- round(poplr(vfSeries)$cslp,2)
  
  powerResult <- data.frame('id'                 = unique(vfSeries$id),
                            'eye'                = unique(vfSeries$eye),
                            'meanAge'            = round(mean(vfSeries$age)),
                            'meanMD'             = round(meanMD, 2),
                            'originalSeriesPoplr'= originalSeriesPoplr,
                            'powerCurve'         = rep(1:nPowerCurves, each=length(mdRates)), 
                            'mdRates'            = mdRates, 
                            'power'              = NA,
                            'dateRangeInMonths'  = dateRangeInMonths)
  
  return(powerResult) }

# Internal function: takes a specific visual field series as input and fixes the
#   test intervals to 6 months. Return the altered series.
#' @noRd
fixToSixMonths <- function(vfSeries){
  seriesLength <- nrow(vfSeries)
  for (i in 1:seriesLength-1) vfSeries$date[i+1] <- vfSeries$date[1] + 180*i
  
  return(vfSeries) }

# Internal function: takes a particular visual field series as input and checks 
#   if there are more than 8 test locations with a mean sensitivity (across the entire 
#   series) of greater than 10dB. Return a boolean: `TRUE` if the series meets this
#   criterion and `FALSE` otherwise.
#' @noRd
seriesIsEligible <- function(vfSeries){
  nrow(vf) >= 5
  vfLocations         <- vfSeries[, -c(1:10,36,45)]
  meanSensPerLocation <- apply(vfLocations , 2, mean)
  meanSensAtLeast10dB <- meanSensPerLocation > 10
  nEligibleLocations  <- mean(meanSensAtLeast10dB)*52
  
  return(nEligibleLocations > 8) }


# Internal function: which locations (columns) have a mean sensitivity (across
#   series) of at least 10dB?
#' @noRd
eligibleColumns <- function(vfSeries){
  vfLocations           <- vfSeries[, -c(1:10,36,45)]
  meanSensPerLocation   <- apply(vfLocations , 2, mean) 
  eligibleBoolean       <- as.numeric(meanSensPerLocation > 10) == 1
  eligibleLocationNames <- names(meanSensPerLocation[eligibleBoolean])
  eligibleLocations     <- as.numeric(substr(eligibleLocationNames, 2,3))
  eligibleColumns       <- eligibleLocations + 10
  
  return(eligibleColumns) }


# Internal function: given a visual field series, return a numeric vector storing
#   its cumulative test intervals in years.
#' @noRd
cumulativeIntervals <- function(vfSeries){
  yearIntervals <- c(0)
  for (i in 1:4){
    dayInterval         <- difftime(vfSeries$date[i+1], vfSeries$date[i], units='days')
    yearInterval        <- as.double(dayInterval / 365)
    yearIntervals       <- append(yearIntervals, yearInterval)
    cumulativeIntervals <- cumsum(yearIntervals) }
  
  return(cumulativeIntervals) }
  
# Internal function: takes a series and randomly reorder it by sampling the
#   series without replacement the same number of times as the number of tests
#   in that series. Returns the reordered series with the original sequence
#   of test dates preserved.
#' @noRd
reorderSeries <- function(vfSeries){
  originalDates        <- vfSeries$date
  reorderedSeries      <- vfSeries[ sample(1:nrow(vfSeries), nrow(vfSeries), replace=FALSE), ]
  reorderedSeries$date <- originalDates
  
  return(reorderedSeries) }

# Internal function: modifies a reordered series by introducing progression signal 
#   of a specific magnitude as defined by 'pointwiseRate' to five randomly sampled
#   locations (columns) as defined by 'fiveVFLocations'. Returns the modified series.
#' @noRd
modifySeries <- function(reorderedSeries, pointwiseRate, fiveVFLocations){
  modifiedSeries      <- reorderedSeries
  cumulativeIntervals <- cumulativeIntervals(reorderedSeries)
  
  for (i in 2:nrow(reorderedSeries) ){
    modifiedSeries[i,fiveVFLocations] <- reorderedSeries[i, fiveVFLocations] - pointwiseRate*cumulativeIntervals[i] }
  
  return(modifiedSeries) }

# Internal function: modify a reordered series by introducing a progression signal
#   of a particular magnitude to 5 randomly sampled locations before performing PoPLR
#   on the modified series which returns a p-value. This is repeated for other rates 
#   of progression, yielding as many p-values as there are rates of progression (7
#   by default). These steps are defined as one power run. Returns a numeric vector
#   of p-values, with each corresponding to a specific rate of progression.
#' @noRd
onePowerRun <- function(reorderedSeries, pointwiseRates, fiveVFLocations){
  foreach (i = 1:length(pointwiseRates), .combine='c') %dopar%                                                           
    modifiedSeries <- modifySeries(reorderedSeries, pointwiseRates[i], fiveVFLocations)
    poplr(modifiedSeries)$cslp }

# Internal function: takes a numeric vector of p-values generated after running
#   the internal function 'onePowerRun' as many times as that defined by 
#   'nPermutationsPerCurve' (default is 100) and returns a numeric vector of power
#   estimates (%). Each power estimate in the vector corresponds to a specific rate 
#   of progression and is derived from the proportion of statistically significant 
#   PoPLR results (p<0.05) at that rate of progression.
#' @noRd
computePowers <- function(pValues){
  powers <- c()
  for (i in 0:6) { 
    pValuesForThisMdRate <- pValues[seq(1,700, 7)+i]
    powerForThisMdRate   <- sum(pValuesForThisMdRate < 0.05) / 100
    powers               <- append(powers, powerForThisMdRate) }
  
  return(powers) }

# Internal function: takes a filled data frame 'powerResult' generated after running 
# 'powerAnalysis' on a specific visual field series and plots the individual power
# curves (15 by default) plus their average, i.e. power(%) vs MD rates of progression
# (dB/year). The average power associated with a -1dB/year rate of progression is also
# shown on the plot. Plot will be saved to 'savePlotPath' if 'savePlot' is TRUE; otherwise,
# plot is displayed in the Plots pane.
#' @noRd
plotPowerCurves <- function(powerResult, id, mdRates, savePlot, savePlotPath){
  if(savePlot) pdf(file   = file.path(savePlotPath, paste0(id,'_','powerCurves')), 
                   width  = 6, 
                   height = 6)
  par(las=1)
  plot(x        = 0, 
       y        = 0, 
       bty      = 'n', 
       pch      = 19, 
       cex      = 0.4, 
       ylim     = c(0,100), 
       log      = 'x',
       xaxt     = 'n', 
       yaxt     = 'n', 
       type     = 'n', 
       xlim     = c(0.05, 4), 
       xlab     = 'Progression Signal (dB/y)', 
       ylab     = 'Power (%)', 
       main     = id, 
       cex.main = 1.6, 
       cex.lab  = 1.5)
  
  axis(labels=paste(-(mdRates)), side=1, at=mdRates, cex.axis=1.6)
  axis(side=2, cex.axis=1.6)
  
  nPowerCurves <- unique(powerResult$powerCurve)
  for (i in nPowerCurves){
    currentPowerCurve <- subset(powerResult, powerCurve==i)
    lines(currentPowerCurve$mdRates, currentPowerCurve$power*100, col='gray') }
  
  meanPowers <- c()
  for (i in mdRates){
    currentMdRate <- subset(powerResult, mdRates==i)
    meanPower     <- mean(currentMdRate$power)
    meanPowers    <- append(meanPowers, meanPower) }
  
  lines(mdRates, meanPowers*100, col='maroon', lwd=4)
  
  txt <- round( meanPowers[ which(mdRates == 1) ]*100, digits=2)
  abline(h=txt, v=1, lty=2)
  legend('topleft', bty='n', paste0(txt, '%'), title='-1dB/y', text.col='gray', lty=2, lwd=2) 
  dev.off() }










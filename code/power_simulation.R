rm(list=ls())
setwd('/Users/fabianyii/Google Drive/power')

# install.packages('RColorBrewer')
# install.packages("remotes")
# library(remotes)
# install_github("imarinfr/vf1/source")
library('visualFields')
library('RColorBrewer')
library('stringr')
library(usethis)
library(doParallel)                           # package for parallelisation

registerDoParallel(3)                         # set number of cores to 3 (4 on my mac)

source('code/vf_config.R')

d <- read.csv('data/power.csv')               # read the "stamps" data
d <- vf_config(d, npx=30, nrow=12)            # config dataset acc to 'visualFields' requirements



# create d.frame to record results (power) #

md_rate <- 0.002 * 2^(1:11)                                         # prog rate in MD terms; each step 2fold increase
pt_rate <- (md_rate) * 52/5                                         # convert to pointwise prog rate

res <- data.frame(iteration=rep(1:15, each=length(pt_rate)), 
                  id=rep(1:30, each=165), pt_rate=pt_rate, pow=0)

# create d.frame to store progressing locations
prog.columns <- data.frame(id=rep(1:30, each=15), 
                           iteration=rep(1:15,30),
                           l1=0, l2=0, l3=0, l4=0, l5=0)

# temporary data frame to store p-val during simulation #
temp.res <- data.frame(run=rep(1:100,each=11), pt_rate=pt_rate, 
                       p_val=0)


################### Simulation begins here! #######################
set.seed(1234)
source('code/simulation_functions.R')

for(id in 1:30){
  
# select a px series
px.series <-select.series(data=d, id=id)

      # 15 iterations with 15 different sets of 
      # progressing locations (columns) per px
      for(iteration in 1:15){
  
      # which n=5 test locations are progressing (give corresponding columns)?
      columns <- sample.eligible.column (px.series, n=5)
      
      # store progressing locations in 'prog.columns'
      prog.columns[which(prog.columns$id==id & 
                  prog.columns$iteration==iteration), 3:7] <- columns

      
               # The 'run' loop: note progressing locations don't change throughout 
               # the total number of runs since 'columns' is kept outside the loop!
               for(run in 1:2){
  
               # RE-ORDER the px series & set test intervals to 6 months;
               # called 'clean.slate' because progression not injected yet.
               # Re-ordering is repeated with each run (inside the 'run' loop)!
               clean.slate <- shuffle(px.series, id=id, month_int=6, nrow=12)


               # function to inject 'annual.rate' progression signal into a
               # specified 'test.series' [1] and run PoPLR to get p-val [2]
               return.pval <- function(test.series, annual.rate){ 
  
                              progression <- prog(annual.rate, nrow=nrow(test.series) )     #[1]
                              test.series[,columns] <- test.series[,columns] - progression  #[1]
  
                              poplr(test.series)$cslp/100                                   #[2]
                              }


               # run 'return.pval' for each of the 11 progression signals 
               # ('pt_rate') with parallelisation. THIS IS EQUAL TO 1 RUN!
               temp.res[which(temp.res$run==run),]$p_val <-
                 
               foreach (i = seq_along(pt_rate), .combine='c') %dopar% {
                       return.pval(test.series=clean.slate, annual.rate=pt_rate[i])
                       }
            
               # live update on progress!
               print( paste( 'Px', id, '—', 
                             iteration, 'out of 15 iterations:', 
                             run, 'out of 100 runs done!')  )
                   
               }      # 'run' loop closed bracket
          
         # compute 'power' for each progression signal at
         # this iteration & store in 'res' data frame
         res[which(res$id==id & res$iteration==iteration),]$pow <-
         compute.power(temp.res, pt_rate) 
        
          }      # 'iteration' loop       

    }      # 'px' loop






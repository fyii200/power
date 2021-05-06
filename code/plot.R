rm(list=ls())
setwd('/Users/fabianyii/Google Drive/power')

# install.packages("remotes")
library(remotes)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

# read raw data (n=12) into r
d <- read.csv('data/n12.csv'); d <- d[,-1]

source('code/empty_plot.R')                                                           # load function to create empty plot (argument: give title)
source('code/plot_indiv.R')                                                           # load function to fit (15) individual curves for each px
source('code/plot_avg.R')                                                             # load function to fit the average curve for each px

col <- c(brewer.pal(12, 'Paired'), brewer.pal(8, 'Dark2'),                       # 30 colour codes for 30 patients
         brewer.pal(8, 'Accent'), brewer.pal(8, 'Set2') )

############# Power vs Progression Signal for each patient (in separate plots) ##############
pdf(file='plots/plot_indiv.pdf', width=6, height=6)
for(i in c(12,13,21)){ 
  empty_plot( title=paste0('Px ', i) )                                           # create empty plot
  plot_indiv(i)                                                                  # fit all individual curves for 'i'th px
  plot_avg(i, legend='on')                                                       # fit the average curve (logit model) for 'i'th px
}
dev.off()
###################################### Done #################################################


####### Power vs Progression Signal for each patient (combined into a single plot) ##########
pdf(file='plot_combined.pdf', width=6, height=6)
empty_plot('')                                                                   # create empty plot without title

for(i in 1:30){                                                                  # fit the average (logit model)
  plot_avg(i, legend='off')                                                      # curve for 'i'th px
}
legend('topleft', bty='n', lty=1, col=col[1:30], legend=1:30, cex=0.55, lwd=4)   # which colour = which px?
dev.off()
###################################### Done #################################################


rm(list=ls())
setwd('/Users/fabianyii/power')

# install.packages("remotes")
library(remotes)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')
library('plotrix')

source('code/vf_config.R')                                   # load function to configure vf data based on 'visualFields' requirements
source('code/empty_plot.R')                             # '' create empty plot (argument: give title)
source('code/plot_indiv.R')                             # '' fit (15) individual curves for each px
source('code/plot_avg.R')                               # '' fit the average curve for each px
source('code/plot_grayscale_sparklines.R')              # grayscale with sparklines

# read raw data (power; n=12) into r
d <- read.csv('n5_power.csv'); d <- d[,-1]

# read visual field raw data into r & configuration
vf <- read.csv('data/power.csv'); vf <- vf[,-1]
vf <- vf_config(vf, npx=30, nrow=12)                                                         

# 30 colour codes for 30 patients
col <- c(brewer.pal(12, 'Paired'),brewer.pal(8, 'Dark2'),brewer.pal(8, 'Accent'),brewer.pal(8, 'Set2'))


############# Power vs Progression Signal for each patient (in separate plots) ##############
pdf(file='plots/plot_indiv.pdf', width=6, height=6)
for(i in 30){ 
  empty_plot(data=vf, title=paste0('Px ', i))                                    # create empty plot
  plot_indiv(data=d, px=i)                                                       # fit all individual curves for 'i'th px
  plot_avg(data=d, px=i, legend='on')                                            # fit the average curve for the 'i'th px
  axis(2, cex.axis=1.6)
}
dev.off()
###################################### Done #################################################

################################ Grayscale with sparklines ##################################
pdf(file='plots/grayscale_sparklines_px.pdf', width=6, height=6)
num_VF <- 5 # number of VFs per series
lwd <- 2.5 # sparkline width
for(i in 30){ 
  VF_series <- subset(vf, id==i)[1:num_VF,]   # extract 'num_VF' VFs from px i's VF series
  mean_series <- vfmean(VF_series, by='eye') # mean of 'num_VF' VFs
  grayscale_sparklines(vf=mean_series, vf_series=VF_series, 's', cex=0.6, lwd=lwd) 
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






# plot serial VF report; specify which px; rmarkdown will turn svf_report.Rmd into pdf #
plot_svf <- function(px){
rmarkdown::render(input = 'svf_report.Rmd',
                  output_format = 'pdf_document',
                  output_file = paste(px, '_svf.pdf', sep=  '') )
}

for(i in 1:30){
plot_svf(px = i)
}

library(pdftools)
pdf_combine(paste(1:30, '_svf.pdf', sep = ''), 'combined_svf.pdf' )        # combine all pdf files into one file
file.remove(paste(1:30, '_svf.pdf', sep = '') )                            # delete individual pdf files





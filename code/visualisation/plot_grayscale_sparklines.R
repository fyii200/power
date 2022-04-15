grayscale_sparklines <- function(vf, vf_series, type = "td", lwd, ...) {
  if(nrow(vf) != 1) stop("can plot only 1 visual field at a time")
  if(!vfisvalid(vf)) stop("incorrect visual field data structure. Cannot plot")
  nv   <- getnv()   # get normative values
  gpar <- getgpar() # get graphical parameters
  locs <- getlocini():ncol(vf)
  # left or right eye
  if(vf$eye == "OS") gpar$tess$xlim <- gpar$tess$xlim[2:1]
  defpar <- par(no.readonly = TRUE) # read default par
  on.exit(par(defpar))              # reset default par on exit, even if the code crashes
  par(mar = c(0, 0, 0, 0), ...)
  # maximum values for grayscales used in type "s", "tds", and "pds"
  maxdb <- nv$agem$model(vf$age)
  # for locations in the blind spot, we input the values of the previous locations 
  maxdb[which(is.na(maxdb))] <- maxdb[which(is.na(maxdb)) - 1]
  # dispatch to raw sensitivity (grayscale) plots, TD and PD (color) plots, or the hybrid
  # sensitivity with TD and PD plot
  
  vfplotsens(gpar, vf[,locs], maxdb, ...)
  vfsparklines(vf_series, type, thr=2, width=4, height=2, add = TRUE, lwd)
}



vfsparklines <- function(vf, type = "td", thr = 2, width = 4,
                         height = 2, add = FALSE, lwd, ...) {
  if(nrow(unique(data.frame(vf$id, vf$eye))) != 1)
    stop("all visual fields must belong to the same subject id and eye")
  nv   <- getnv()
  gpar <- getgpar() # get graphical parameters
  locs <- getlocini():ncol(vf)
  # left or right eye
  x <- as.numeric(vf$date - vf$date[1]) / 365.25 # it should be difference in years from baseline date
  if(type == "s") {
    y <- vf[,locs]
  } else if(type == "td") {
    y <- gettd(vf)[,locs]
  } else if(type == "pd") {
    y <- getpd(gettd(vf))[,locs]
  } else stop("wrong type of plot requested. Must be 's', 'td', or 'pd'")
  # remove blind spot locations
  if(length(getlocmap()$bs) > 0) {
    gpar$coord <- gpar$coord[-getlocmap()$bs,]
    y <- y[,-getlocmap()$bs]
  }
  # left or right eye
  if(vf$eye[1] == "OS") gpar$tess$xlim <- gpar$tess$xlim[2:1]
  xlim <- c(0, max(x))
  ylim <- c(min(y), max(y))
  suppressWarnings(resmad <- sapply(as.list(y), function(y) mad(lm(y ~ x)$residuals)))
  cols <- rep("#4D4D4D", length(resmad))
  cols[resmad > thr] <- "#FF0000"
  defpar <- par(no.readonly = TRUE) # read default par
  on.exit(par(defpar))              # reset default par on exit, even if the code crashes
  if(!add) {
    par(mar = c(0, 0, 0, 0), ...)
    plot(gpar$coord$x, gpar$coord$y, typ = "n",
         ann = FALSE, axes = FALSE, asp = 1,
         xlim = gpar$tess$xlim, ylim = gpar$tess$ylim, ...)
    # plot polygons
    lapply(gpar$tess$tiles, polygon, border = "lightgray", col = "#FFFFFF")
    # add blind spot
    draw.ellipse(15, -1.5, 2.75, 3.75, col = "lightgray", border = NA)
    # outer hull
    polygon(gpar$tess$hull, border = "lightgray")
  }
  # plot the spark lines:  for left eyes, handling figure positions is incompatible
  # with swapping the min and max x limits in the plot. We need a patch here
  if(gpar$tess$xlim[1] < gpar$tess$xlim[2]) {
    figs <- cbind(grconvertX(gpar$coord$x - width  / 2, to = "ndc"),
                  grconvertX(gpar$coord$x + width  / 2, to = "ndc"),
                  grconvertY(gpar$coord$y,              to = "ndc"),
                  grconvertY(gpar$coord$y + height,     to = "ndc"))
  } else {
    figs <- cbind(grconvertX(gpar$coord$x + width  / 2, to = "ndc"),
                  grconvertX(gpar$coord$x - width  / 2, to = "ndc"),
                  grconvertY(gpar$coord$y,              to = "ndc"),
                  grconvertY(gpar$coord$y + height,     to = "ndc"))
  }
  for(i in 1:nrow(figs)) {
    par(fig = figs[i,], new = TRUE)
    plot(x, y[,i], type = "l", xlim = xlim, ylim = ylim, axes = FALSE, col = cols[i], lwd=lwd, ...)
  }
}

#### INTERNAL FUNCTIONS

# plot sensitivity values
vfplotsens <- function(gpar, vf, maxval, digits = 0, ...) {
  # background gray shades
  fcol <- (vf - gpar$tess$floor) / (maxval - gpar$tess$floor)
  fcol[fcol > 1] <- 1
  fcol[fcol < 0] <- 0
  # foreground text gray shades
  tcol <- rep(0.3, length(vf))
  tcol[fcol < 0.5] <- 0.7
  # blind spot
  fcol[getlocmap()$bs] <- 1
  tcol[getlocmap()$bs] <- 0.3
  # convert to hexadecimal color
  fcol <- rgb(fcol, fcol, fcol)
  tcol <- rgb(tcol, tcol, tcol)
  plot(gpar$coord$x, gpar$coord$y, typ = "n", ann = FALSE,
       axes = FALSE, asp = 1,
       xlim = gpar$tess$xlim, ylim = gpar$tess$ylim, ...)
  # plot polygons
  for(i in 1:length(gpar$tess$tiles))
    polygon(gpar$tess$tiles[[i]], col = fcol[i], border = NA)
  # add blind spot
  draw.ellipse(15, -1.5, 2.75, 3.75, col = "lightgray", border = NA)
  # outer hull
  polygon(gpar$tess$hull, border = "lightgray")
  txt <- round(vf, digits)
  txt[is.na(vf)] <- ""
  txt[vf < gpar$tess$floor] <- paste0("<", gpar$tess$floor)
  text(gpar$coord$x, gpar$coord$y, txt, col = tcol, ...)
}











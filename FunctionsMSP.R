## functions page for 'MultiSpPrac.Rmd'



# function to plot distributions 
plot.map <- function(col,df) {
  r <- raster(nrow=1177,ncol=1342,xmn=1, xmx=1342, ymn=1, ymx=1177)
  df <- df[order(df$uniqueID),]
  r[] <- df[,col]
  plot(r)
}



plotmeansCI <- function(m,ylim) {
  
  mc <- summary(m)$coef[,1]
  sdc <- 1.96*summary(m)$coef[,2]
  
  plot(1:length(mc),mc,
       pch=19, col=c('cadetblue4'),xaxt='n',xlab="", ylab="Mean Parameter   Estimates",cex=1.4,cex.lab=1.4,las=1,ylim=ylim)
  arrows(1:length(mc), mc-sdc, 1:length(mc), mc+sdc, length=0.05, angle=90, code=3, col=c('cadetblue4'))
  abline(h=0,col='grey85')
  axis(1,at=c(1:length(mc)),labels=names(mc))
  
}

plotmeansCredInt <- function(mc,lo,up,ylim) {
  

  plot(1:length(mc),mc,
       pch=19, col=c('cadetblue4'),xaxt='n',xlab="", ylab="Mean Parameter   Estimates",cex=1.4,cex.lab=1.4,las=1,ylim=ylim)
  arrows(1:length(mc), lo, 1:length(mc), up, length=0.05, angle=90, code=3, col=c('cadetblue4'))
  abline(h=0,col='grey85')
  axis(1,at=c(1:length(mc)),labels=c('int','b4','b12','foot'))
  
}

plotmeansCredIntRho <- function(mc,lo,up,ylim) {
  
  
  plot(1:length(mc),mc,
       pch=19, col=c('cadetblue4'),xaxt='n',xlab="", ylab="Mean Parameter   Estimates",cex=1.4,cex.lab=1.4,las=1,ylim=ylim)
  arrows(1:length(mc), lo, 1:length(mc), up, length=0.05, angle=90, code=3, col=c('cadetblue4'))
  abline(h=0,col='grey85')
  axis(1,at=c(1:length(mc)),labels=c('M295','M61','M94'))
  
}



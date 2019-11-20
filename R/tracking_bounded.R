library('svglite')
source('R/common.R')


# Figures -----

plotBoundedTrackingRaw <- function(target='inline') {
  
  participants <- c(1,3,5,6,7,8)
  
  if (target=='svg') {
    svglite(file='doc/Fig03.svg',width=7,height=6)
  }
  
  par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
  
  colors <- getColors()
  
  red <- colors$yorkred
  
  plot(-1000,-1000,main='',ylim=c(0.5,5.5),xlim=c(0.5,6.5),xlab='participant',ylab='internal speed [cps]',asp=1,bty='n',ax=F)

  speeds <- c(-3,-1,0,1,3)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speedidx in c(1:length(speeds))) {
      
      speed <- speeds[speedidx]
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) 
      
      for (trialno in trials) {
        
        idx <- which(sdf$trial_no == trialno)
        
        t <- sdf$time_ms[idx]
        t <- (t - t[1]) / 1000
        x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
        y <- sdf$handy_pix[idx]
        
        x <- (x / 960)
        x <- x + ppidx
        
        y <- (y / 960)
        y <- y + speedidx
        
        lines(x,y,col=red$t,lw=2)
        
      }
      
    }
    
  }
  
  axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
  axis(side=2,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}
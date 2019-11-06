library('svglite')


# Figures -----

plotBoundedTrackingRaw <- function(target='inline') {
  
  participants <- c(1,3,5,6:8)
  
  if (target=='svg') {
    svglite(file='doc/bounded_tracking_uncluttered.svg',width=6,height=9)
  }
  
  par(mfrow=c(length(participants),5),mar=c(4.1,4.1,1.0,1.0))
  
  for (ppno in participants) {
    
    df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speed in c(-3,-1,0,1,3)) {
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) 
      
      if (ppno == participants[1]) {
        main=sprintf('speed: %0.0f',speed)
      } else {
        main=''
      }
      xlab=''
      if (speed == -3) {
        ylab = sprintf('participant %d',ppno)
      } else {
        ylab = ''
      }
      
      plot(-1000,-1000,main=main,ylim=c(-450,450),xlim=c(-450,450),ylab=ylab,xlab=xlab,asp=1,bty='n',ax=F)
      lines(c(0,0),c(0,12),col='#999999')
      
      for (trialno in trials) {
        
        idx <- which(sdf$trial_no == trialno)
        
        t <- sdf$time_ms[idx]
        t <- (t - t[1]) / 1000
        x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
        y <- sdf$handy_pix[idx]
        
        lines(x,y,col='#99000066')
        
      }
      
    }
    
  }
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}
library('svglite')

plotResetTrial <- function(participant,trial) {
  
  library(colormap)
  
  df <- read.csv(sprintf('data/reset_pause/reset_p%02d.csv', participant), stringsAsFactors=F)
  df <- df[which(df$trial==trial & df$step==2),]
  
  par(mfrow=c(1,1))
  
  # mycolors <- colormap(colormap=colormaps$velocity_blue, nshades=256)
  mycolors <- colormap(colormap=colormaps$autumn, nshades=256)
  mycolors <- colormap(colormap=colormaps$plasma, nshades=256)
  
  velocity <- sqrt(diff(df$handx_pix)^2 + diff(df$handy_pix)^2) / diff(df$time_ms)
  
  segment_colors <- mycolors[floor(velocity/max(velocity, na.rm=T)*256)+1]
  
  plot(-1000,-1000,main=sprintf('participant: %d, trial: %d',participant,trial),xlab='',ylab='',xlim=c(-500,500),ylim=c(-750,750),asp=1)
  
  N <- nrow(df)
  
  X0 <- df$handx_pix[1:N-1]
  Y0 <- df$handy_pix[1:N-1]
  X1 <- df$handx_pix[2:N]
  Y1 <- df$handy_pix[2:N]
  
  segments(x0=X0, y0=Y0, x1=X1, y1=Y1, col=segment_colors)
  
}

plotResetData <- function(participants=c(1,2,9,10,11,12,13),trials=c(1:64)) {
  
  for (participant in participants) {
    
    pdf(file=sprintf('reset_p%02d.pdf',participant),width=6,height=4)
    
    for (trial in trials) {
      
      plotResetTrial(participant=participant, trial=trial)
      
    }
    
    dev.off()
    
  }
  
}
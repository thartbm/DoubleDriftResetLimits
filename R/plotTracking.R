require('ez')
require('svglite')

plotInfiniteTracking <- function() {
  
  participants <- c(1:5)
  
  par(mfrow=c(length(participants),5),mar=c(3.8,4,1.8,0.5))
  
  for (ppno in participants) {
    
    df <- read.csv(sprintf('../data/tracking_full/tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speed in c(-3,-1,0,1,3)) {
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) # trial_no column is missing... 
                                     # read in as index when converting trials to full data set?
      
      plot(-1000,-1000,main=sprintf('speed: %0.0f',speed),ylim=c(0,12),xlim=c(-900,900),ylab='time [s]',xlab='horizontal hand position [px]')
      lines(c(0,0),c(0,12),col='#999999')
      
      for (trialno in trials) {
        
        idx <- which(sdf$trial_no == trialno)
        
        
        t <- sdf$time_ms[idx]
        t <- (t - t[1]) / 1000
        x <- sdf$handx_pix[idx]
        
        lines(x,t,col='#99000066')
        
      }
      
    }
    
  }
  
}




plotBoundedTracking <- function(target='inline') {
  
  participants <- c(1,3,5,6:8)
  
  if (target=='svg') {
    svglite(file='bounded_tracking_uncluttered.svg',width=6,height=9)
  }
  
  par(mfrow=c(length(participants),5),mar=c(4.1,4.1,1.0,1.0))
  
  for (ppno in participants) {
    
    df <- read.csv(sprintf('../data/boundtrack/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speed in c(-3,-1,0,1,3)) {
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) # trial_no column is missing... 
      # read in as index when converting trials to full data set?
      
      if (ppno == participants[1]) {
        main=sprintf('speed: %0.0f',speed)
      } else {
        main=''
      }
      # if (ppno == participants[length(participants)]) {
      #   xlab = 'horizontal position [px]'
      # } else {
      #   xlab = ''
      # }
      xlab=''
      # if (speed == -3) {
      #   ylab = 'vert pos [px]'
      # } else {
      #   ylab = ''
      # }
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
        
        #lines(x,t,col='#99000066')
        lines(x,y,col='#99000066')
        
      }
      
    }
    
  }
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

# setwd("~/Science/IllusoryMotion/ana")
plotOnePass <- function() {
  
  participants <- c(1,2,9:13)
  
  participant_data <- list()
  
  for (participant in participants) {
    
    participant_data[[participant]] <- read.csv(sprintf('../data/onepass/onepass_p%02d.csv',participant), stringsAsFactors=F)
    
  }
  
  par(mfrow=c(3,6))
  
  for (plotrow in c(1:3)) {
    
    for (task in c(1,2)) {
      
      for (plotcol in c(1:3)) {
        
        plot(-1000,-1000,xlim=c(0,1),ylim=c(0,1),asp=1,main=c('percept','track')[task],xlab='',ylab='')
        
        for (participant in participants) {
          
          df <- participant_data[[participant]]
          
          externalmovement <- c(.125,.167,.250)[plotrow] # 4,3,2 seconds
          internalmovement <- c(1,2,3)[plotcol]          # 
          
          plotdf <- df[which(df$internalMovement == internalmovement & df$externalMovement == externalmovement),]
          if (task == 1) {
            plotdf <- plotdf[which(plotdf$trial_no > 30),]
          }
          if (task == 2) {
            plotdf <- plotdf[which(plotdf$trial_no <= 30),]
          }
          
          plottrials <- unique(plotdf$trial_no)
          for (trial in plottrials) {
            
            trialdf <- plotdf[which(plotdf$trial_no == trial & plotdf$step == 2),]
            
            if (task == 1) {
              percept <- trialdf$percept[1]
              lines(c(0,sin(percept/180.)),c(0,cos(percept/180.)),col='#00000033')
            }
            
            if (task == 2) {
              X <- trialdf$handx_pix / 524.
              Y <- (trialdf$handy_pix + 262.) / 524.
              lines(X,Y,col='#00000033')
            }
            
          }
          
        }
        
      }
    }
  }
  
  # done looping
  
}

plotOnePass_V2 <- function(participants = c(2,3,6), target='pdf') {
  
  internalMovements <- c(1,2,3)
  externalMovements <- rev(c(.125,.167,.25))
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V2.pdf',onefile=TRUE)
  }
  
  for (participant in participants) {
    
    df <- read.csv(sprintf('../data/onepass_v2/onepass_p%02d.csv',participant))
    
    par(mfrow=c(2,2),mar=c(4,4,2,0.1))
    
    for (task in c('arrow','ruler','track','re-trace')) {
      
      tasktrials <- list('track'    = c(1:30),
                         'arrow'    = c(31:60),
                         're-trace' = c(61:90),
                         'ruler'    = c(91:120) )[[ task ]]
      
      plot(-1000,-1000,main=task,xlab='external motion',ylab='internal motion',xlim=c(0,3),ylim=c(0,3),bty='n',ax=FALSE,asp=1)
      
      taskdf <- df[which(df$trial_no %in% tasktrials),]
      
      for (EM.idx in c(1:length(externalMovements))) {
        
        for (IM.idx in c(1:length(internalMovements))) {
          
          trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == externalMovements[EM.idx] & taskdf$internalMovement == internalMovements[IM.idx])])
          
          for (trial in trials) {
            
            trialdf <- taskdf[taskdf$trial_no == trial,]
            
            if (task %in% c('arrow','ruler')) {
              
              percept <- trialdf$percept[1]
              
              if (task == 'arrow') percept <- 90 - (percept - 90)
              
              x <- c(0,cos((percept/180)*pi)) * 0.8
              y <- c(0,sin((percept/180)*pi)) * 0.8
              
              lines(x+EM.idx-0.5,y+IM.idx-0.9)
              
            } else {
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              x <- ((trialdf$handx_pix[trialdf$step == step] / (524)) + 0.0) * 0.8
              y <- ((trialdf$handy_pix[trialdf$step == step] / (524)) + 0.5) * 0.8
              
              lines(x+EM.idx-0.5,y+IM.idx-0.9)
              
            }
            
          }
          
        }
        
      }
      
      axis(side=1,at=c(0.5,1.5,2.5),labels=rev(c('1/8','1/6','1/4')))
      axis(side=2,at=c(0.5,1.5,2.5),labels=c('1','2','3'))
      
    }

  }
  
  if (target == 'pdf') {
    dev.off()
  }
  
}

# localMaxima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(-.Machine$integer.max, x)) > 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   y
# }



plotOnePass_V3 <- function(participants = c(1,101,5), target='pdf') {
  
  graphics.off()
  
  internalMovements <- c(1,2,4)
  externalMovements <- rev(c(.125,.250,0.500))
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V3.pdf',onefile=TRUE)
  }
  
  for (participant in participants) {
    
    df <- read.csv(sprintf('../data/onepass_v2/onepass_V3_p%02d.csv',participant))
    
    par(mfrow=c(2,2),mar=c(4,4,2,0.1))
    
    for (task in c('arrow','ruler','track','re-trace')) {
      
      if (participant %in% c(101,5,105)) {
        tasktrials <- unique(df$trial[df$taskname == task])
      }
      if (participant == 1) {
        tasktrials <- list('track'    = c(1:72),
                           'arrow'    = c(72:144),
                           're-trace' = c(145:216),
                           'ruler'    = c(217:288) )[[ task ]]
      }
      plot(-1000,-1000,main=task,xlab='external motion',ylab='internal motion',xlim=c(0,3),ylim=c(0,3),bty='n',ax=FALSE,asp=1)
      
      taskdf <- df[which(df$trial_no %in% tasktrials),]
      
      for (EM.idx in c(1:length(externalMovements))) {
        
        for (IM.idx in c(1:length(internalMovements))) {
          
          if (task %in% c('track','re-trace')) {
            lines(c(0,0)+EM.idx-0.5,c(0.1,0.9)+IM.idx-1,col='#000000',lty=3)
          }
          
          trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == externalMovements[EM.idx] & abs(taskdf$internalMovement) == internalMovements[IM.idx])])
          
          for (trial in trials) {
            
            trialdf <- taskdf[taskdf$trial_no == trial,]
            
            if (task %in% c('arrow','ruler')) {
              
              percept <- trialdf$percept[1]
              
              #if (task == 'arrow') percept <- 90 - (percept - 90)
              
              if (trialdf$internalMovement[1] < 0) percept <- 90 - (percept - 90)
              
              x <- c(0,cos((percept/180)*pi)) * 0.6
              y <- c(0,sin((percept/180)*pi)) * 0.6
              
              lines(x+EM.idx-0.5,y+IM.idx-0.9)
              
            } else {
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              step.idx <- which(trialdf$step == step)
              
              x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
              y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
              t <- trialdf$time_ms[step.idx]
              
              # if (trial == 2 & participant == 1) {
              #   print(step.idx)
              #   # print(x)
              #   # print(y)
              # }
              
              if (trialdf$internalMovement[1] < 0) x <- -x
              
              boundary <- which(diff(x) < 0)[1]
              
              if (is.na(boundary)) {
                lines(x+EM.idx-0.5,y+IM.idx-0.9,col='#666666')
              } else {
                lines(x[1:boundary]+EM.idx-0.5,y[1:boundary]+IM.idx-0.9,col='#990000')
                lines(x[boundary:length(x)]+EM.idx-0.5,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC')
                
                smspl <- smooth.spline(t, x, spar=.25)
                x_p <- predict(smspl$fit, t)$y
                
                localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
                lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
                localmaxima <- localmaxima[which(lmd > 0.1)]
                lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
                localmaxima <- localmaxima[which(lmd > 0.01)]
                if (length(localmaxima) > 0) {
                  points(x[localmaxima[1]]+EM.idx-0.5,IM.idx-0.1,col='#FF0000')
                  points(EM.idx-0.7,y[localmaxima[1]]+IM.idx-0.9,col='#FF0000')
                }
                
              }
              
            }
            
          }
          
        }
        
      }
      
      axis(side=1,at=c(0.5,1.5,2.5),labels=rev(c('1/8','1/4','1/2')))
      axis(side=2,at=c(0.5,1.5,2.5),labels=c('1','2','4'))
      
    }
    
  }
  
  if (target == 'pdf') {
    dev.off()
  }
  
}




plotResetTrial <- function(participant,trial) {
  
  library(colormap)
  
  df <- read.csv(sprintf('../data/reset/reset_p%02d.csv', participant), stringsAsFactors=F)
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







plotOnePass_V4_participants <- function(participants = c(1,2,3,4,5), target='pdf') {
  
  graphics.off()
  
  internalMovements <- c(2,3,4)
  externalMovements <- rev(c(.125,.167))
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V4.pdf',onefile=TRUE)
  }
  
  for (participant in participants) {
    
    df <- read.csv(sprintf('../data/onepass_V4/onepass_V4_p%02d.csv',participant))
    
    par(mfrow=c(2,1),mar=c(4,4,2,0.1))
    
    for (task in c('arrow','re-trace')) {
      
      tasktrials <- unique(df$trial[df$taskname == task])
      
      plot(-1000,-1000,main=task,xlab='external motion',ylab='internal motion',xlim=c(0,1),ylim=c(0,3),bty='n',ax=FALSE,asp=1)
      
      taskdf <- df[which(df$trial_no %in% tasktrials),]
      
      for (EM.idx in c(1:length(externalMovements))) {
        
        for (IM.idx in c(1:length(internalMovements))) {
          
          if (task %in% c('track','re-trace')) {
            lines(c(0,0)+EM.idx-0.5,c(0.1,0.9)+IM.idx-1,col='#000000',lty=3)
          }
          
          trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == externalMovements[EM.idx] & abs(taskdf$internalMovement) == internalMovements[IM.idx])])
          
          for (trial in trials) {
            
            trialdf <- taskdf[taskdf$trial_no == trial,]
            
            if (task %in% c('arrow','ruler')) {
              
              percept <- trialdf$percept[1]
              
              #if (task == 'arrow') percept <- 90 - (percept - 90)
              
              if (trialdf$internalMovement[1] < 0) percept <- 90 - (percept - 90)
              
              x <- c(0,cos((percept/180)*pi)) * 0.6
              y <- c(0,sin((percept/180)*pi)) * 0.6
              
              lines(x+EM.idx-0.5,y+IM.idx-0.9)
              
            } else {
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              step.idx <- which(trialdf$step == step)
              
              x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
              y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
              t <- trialdf$time_ms[step.idx]
              
              # if (trial == 2 & participant == 1) {
              #   print(step.idx)
              #   # print(x)
              #   # print(y)
              # }
              
              if (trialdf$internalMovement[1] < 0) x <- -x
              
              boundary <- which(diff(x) < 0)[1]
              
              if (is.na(boundary)) {
                lines(x+EM.idx-0.5,y+IM.idx-0.9,col='#666666')
              } else {
                lines(x[1:boundary]+EM.idx-0.5,y[1:boundary]+IM.idx-0.9,col='#990000')
                lines(x[boundary:length(x)]+EM.idx-0.5,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC')
                
                smspl <- smooth.spline(t, x, spar=.25)
                x_p <- predict(smspl$fit, t)$y
                
                localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
                lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
                localmaxima <- localmaxima[which(lmd > 0.1)]
                lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
                localmaxima <- localmaxima[which(lmd > 0.01)]
                if (length(localmaxima) > 0) {
                  points(x[localmaxima[1]]+EM.idx-0.5,IM.idx-0.1,col='#FF0000')
                  points(EM.idx-0.7,y[localmaxima[1]]+IM.idx-0.9,col='#FF0000')
                }
                
              }
              
            }
            
          }
          
        }
        
      }
      
      axis(side=1,at=c(0.5,1.5),labels=rev(c('4 s','3 s')))
      axis(side=2,at=c(0.5,1.5,2.5),labels=c('2','3','4'))
      
    }
    
  }
  
  if (target == 'pdf') {
    dev.off()
  }
  
}



plotOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), target='pdf') {
  
  graphics.off()
  
  internalMovements <- c(2,3,4)
  externalMovements <- rev(c(.125, .167))
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V4.pdf',onefile=TRUE,width=11,height=8)
  }
  if (target == 'svg') {
    svglite(file='onePass_V4.svg',width=11,height=8)
  }
  
  par(mfrow=c(2,1),mar=c(4,4,2,0.1))
  
  for (task in c('arrow','re-trace')) {
    
    # we will generate data files for both tasks, with these columns:
    # except that there is no bound in the arrow task
    participant <- c()
    trial <- c()
    internalspeed <- c()
    internaldirection <- c()
    externalspeed <- c()
    fixationside <- c()
    initialdirection <- c()
    boundX <-c()
    boundY <-c()
    
    
    plot(-1000,-1000,main=task,xlab='participant',ylab='internal motion',xlim=c(0,(length(participants)*2)+1),ylim=c(0,3),bty='n',ax=FALSE,asp=1)
    
    for (participant.idx in c(1:length(participants))) {
      
      ppno <- participants[participant.idx]
      
      df <- read.csv(sprintf('../data/onepass_V4/onepass_V4_p%02d.csv',ppno))
    
      tasktrials <- unique(df$trial[df$taskname == task])
      
      taskdf <- df[which(df$trial_no %in% tasktrials),]
      
      for (EM.idx in c(1:length(externalMovements))) {
        
        for (IM.idx in c(1:length(internalMovements))) {
          
          if (task %in% c('track','re-trace')) {
            lines(c(0,0)+(participant.idx*2)-2+EM.idx,c(0.1,0.9)+IM.idx-1,col='#000000',lty=3)
          }
          
          trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == externalMovements[EM.idx] & abs(taskdf$internalMovement) == internalMovements[IM.idx])])
          
          Xbounds <- c()
          Ybounds <- c()
          
          for (trialno in trials) {
            
            trialdf <- taskdf[taskdf$trial_no == trialno,]
            
            
            participant <- c(participant, ppno)
            trial <- c(trial, trialno)
            internalspeed <- c(internalspeed, internalMovements[IM.idx])
            internaldirection <- c(internaldirection, ifelse(trialdf$internalMovement[1] > 0, 1, -1))
            externalspeed <- c(externalspeed, externalMovements[EM.idx])
            fixationside <- c(fixationside, trialdf$fixationside[1])
            
            
            
            if (task %in% c('arrow','ruler')) {
              
              percept <- trialdf$percept[1]
              
              #if (task == 'arrow') percept <- 90 - (percept - 90)
              
              if (trialdf$internalMovement[1] < 0) percept <- 90 - (percept - 90)
              
              x <- c(0,cos((percept/180)*pi)) * 0.6
              y <- c(0,sin((percept/180)*pi)) * 0.6
              
              lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#00000033')
              
              initialdirection <- c(initialdirection, 90 - percept)
              boundX <- c(boundX, NA)
              boundY <- c(boundY, NA)
              
            } else {
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              step.idx <- which(trialdf$step == step)
              
              x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
              y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
              t <- trialdf$time_ms[step.idx]
              
              # if (trial == 2 & participant == 1) {
              #   print(step.idx)
              #   # print(x)
              #   # print(y)
              # }
              
              
              if (trialdf$internalMovement[1] < 0) x <- -x
              
              point <- which(sqrt(x^2 + y^2) > 0.15)[1]
              percept <- (atan2(y[point], x[point]) / pi) * 180
              initialdirection <- c(initialdirection, 90 - percept)
              
              # why do this and also the smoothed spline?
              boundary <- which(diff(x) < 0)[1]
              
              if (is.na(boundary)) {
                lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')

                boundX <- c(boundX, NA)
                boundY <- c(boundY, NA)

              } else {

              #lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
              #lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
              # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
              
                smspl <- smooth.spline(t, x, spar=.25)
                x_p <- predict(smspl$fit, t)$y
                
                localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
                lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
                localmaxima <- localmaxima[which(lmd > 0.1)]
                lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
                localmaxima <- localmaxima[which(lmd > 0.01)]
                if (length(localmaxima) > 0) {
                  
                  #   points(x[localmaxima[1]]+participant-0.5,IM.idx-0.1,col='#FF0000')
                  #   points(participant-0.7,y[localmaxima[1]]+IM.idx-0.9,col='#FF0000')
                  Xbounds <- c(Xbounds, x[localmaxima[1]])
                  Ybounds <- c(Ybounds, y[localmaxima[1]])
                  
                  boundX <- c(boundX, x[localmaxima[1]])
                  boundY <- c(boundY, y[localmaxima[1]])
                  
                  points(x[localmaxima[1]]+(participant.idx*2)-2+EM.idx,y[localmaxima[1]]+IM.idx-0.9,col='#b400e4ff')
                  
                  boundary <- localmaxima[1]
                  lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
                  lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
                  
                } else {
                  lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
                  boundX <- c(boundX, NA)
                  boundY <- c(boundY, NA)
                }
                
              }
              
            } 
            
          }
          
          # done all trials, can now draw distributions of end points
          
          if (task %in% c('track', 're-trace')) {
            
            XdistrX <- seq(-0.1, 0.6,  .01)
            XdistrY <- dnorm(XdistrX,mean=mean(Xbounds),sd=sd(Xbounds)) / dnorm(c(mean(Xbounds)),mean=mean(Xbounds),sd=sd(Xbounds))
            XdistrY <- XdistrY / 10
            
            YdistrY <- seq(0.0, 0.70, .01)
            YdistrX <- dnorm(YdistrY,mean=mean(Ybounds),sd=sd(Ybounds)) / dnorm(c(mean(Ybounds)),mean=mean(Ybounds),sd=sd(Ybounds))
            YdistrX <- YdistrX / 10
            
            lines(XdistrX+(participant.idx*2)-2+EM.idx,XdistrY+IM.idx-1.1,col='#0fd2e2ff')
            lines(YdistrX+(participant.idx*2)-2.2+EM.idx,YdistrY+IM.idx-0.9,col='#0fd2e2ff')
            
            
            # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC')
            
          }
          
        }
        
      }
      
    }
    
    axis(side=1,at=(c(1:length(participants)*2)-0.5),labels=c(1:length(participants)))
    axis(side=2,at=c(0.5,1.5,2.5),labels=c('2','3','4'))
    
    # write out csv file:
    
    # participant <- c()
    # internalspeed <- c()
    # externalspeed <- c()
    # fixationside <- c()
    # initialdirection <- c()
    # boundX <-c()
    # boundY <-c()
    
    df <- data.frame(participant, trial, internalspeed, internaldirection, externalspeed, fixationside, initialdirection, boundX, boundY)
    write.csv(df, file=sprintf('../data/onePassV4_%s.csv', task), quote=F, row.names=F)
    
  }
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}

summarizeTraceBoundsV4 <- function() {
  
  df <- read.csv('../data/onePassV4_re-trace.csv', stringsAsFactors = F)
  
  df2 <- read.csv('../data/onePassV4_arrow.csv', stringsAsFactors = F)
  
  # participants <- unique(df$participant)
  # 
  # internalspeeds <- unique(df$internalspeed)
  # externalspeeds <- unique(df$externalspeed)
  
  aggdf <- aggregate(boundX ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  names(aggdf)[which(names(aggdf) == 'boundX')] <- 'boundX_mean'
  
  tempdf <- aggregate(boundX ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['boundX_sd'] <- tempdf$boundX
  
  tempdf <- aggregate(boundY ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  aggdf['boundY_mean'] <- tempdf$boundY
  
  tempdf <- aggregate(boundY ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['boundY_sd'] <- tempdf$boundY
  
  tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  aggdf['initialdirection_mean'] <- tempdf$initialdirection
  
  tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['initialdirection_sd'] <- tempdf$initialdirection
  
  tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=mean, na.rm=TRUE)
  aggdf['arrowdirection_mean'] <- tempdf$initialdirection
  
  tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=sd, na.rm=TRUE)
  aggdf['arrowdirection_sd'] <- tempdf$initialdirection
  
  return(aggdf)
  
}

doTraceBoundANOVAs <- function() {
  
  df <- summarizeTraceBoundsV4()
  
  df$participant <- as.factor(df$participant)
  df$internalspeed <- as.factor(df$internalspeed)
  df$externalspeed <- as.factor(df$externalspeed)
  
  #cat('\nX boundary\n\n')
  #print(ezANOVA(data=df, dv=boundX_mean, wid=participant, within=c(internalspeed, externalspeed)))
  
  #cat('\nY boundary\n\n')
  #print(ezANOVA(data=df, dv=boundY_mean, wid=participant, within=c(internalspeed, externalspeed)))
  
  cat('\ninitial direction\n\n')
  print(ezANOVA(data=df, dv=initialdirection_mean, wid=participant, within=c(internalspeed, externalspeed)))
  
  cat('\narrow direction\n\n')
  print(ezANOVA(data=df, dv=arrowdirection_mean, wid=participant, within=c(internalspeed, externalspeed)))
  
}

# we'll control the color in figures centrally from here:
colorset <- list()

colorset[['expActS']] <- '#005de4ff' # blue
colorset[['expActT']] <- '#005de42f'
colorset[['expPasS']] <- '#0fd2e2ff' # lighter blue
colorset[['expPasT']] <- '#0fd2e22f'
colorset[['claActS']] <- '#e51636ff' # "York red"
colorset[['claActT']] <- '#e516362f'
colorset[['claPasS']] <- '#ff8200ff' # orange
colorset[['claPasT']] <- '#ff82002f'

# colorset[['extra1S']] <- '#c400c4ff' # purple
# colorset[['extra1T']] <- '#c400c42f'

colorset[['onlActS']] <- '#b400e4ff' # purple
colorset[['onlActT']] <- '#b400e42f'
# colorset[['onlPasS']] <- '#8266f4ff' # violet
# colorset[['onlPasT']] <- '#8266ff2f'
colorset[['onlPasS']] <- '#ff6ec7ff' # pink
colorset[['onlPasT']] <- '#ff6ec72f'

plotTraceBounds <- function(target='pdf') {
  
  graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V4_boundaries.pdf',onefile=TRUE,width=8,height=11)
  }
  if (target == 'svg') {
    svglite(file='onePass_V4_boundaries.svg',width=8,height=11)
  }
  
  
  df <- summarizeTraceBoundsV4()
  df$externalspeed[which(df$externalspeed == 0.125)] <- 4
  df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  
  #df <- df[which(df$participant != 5),]
  #df <- df[which(df$participant != 8),]
  
  par(mfrow=c(3,2),mar=c(4,4.2,1,4.2))
  
  solids <- list('2'='#b400e4ff', '3'='#e51636ff', '4'='#ff8200ff')
  transp <- list('2'='#b400e42f', '3'='#e516362f', '4'='#ff82002f')
  
  # X bounds over conditions
  
  plot(-1000,-1000,main='horizontal reset position and speed',xlab='external speed',ylab='horizontal reset',xlim=c(2.5,4.5),ylim=c(0,0.25),bty='n',ax=F)
  
  for (internalspeed in c(2,3,4)) {
    
    meanX <- aggregate(boundX_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
    
    confX <- aggregate(boundX_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
    
    X <- c(confX$externalspeed, rev(confX$externalspeed))
    Y <- c(confX[,2][,1], rev(confX[,2][,2]))
    polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
    
    lines(meanX$externalspeed, meanX$boundX_mean, col=solids[[sprintf('%d',internalspeed)]])
    
  }
  
  legend(2.5,0.25,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  
  axis(1,at=c(3,4))
  axis(2,at=seq(0,0.25,0.05),las=1)
  
  
  # Y bounds over conditions
  
  plot(-1000,-1000,main='vertical reset position and speed',xlab='external speed',ylab='vertical reset',xlim=c(2.5,4.5),ylim=c(0,0.75),bty='n',ax=F)
  
  for (internalspeed in c(2,3,4)) {
    
    meanY <- aggregate(boundY_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
    
    confY <- aggregate(boundY_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
    
    X <- c(confY$externalspeed, rev(confY$externalspeed))
    Y <- c(confY[,2][,1], rev(confY[,2][,2]))
    polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
    
    lines(meanY$externalspeed, meanY$boundY_mean, col=solids[[sprintf('%d',internalspeed)]])
    
  }
  
  legend(2.5,0.75,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  
  axis(1,at=c(3,4))
  axis(2,at=seq(0,0.75,0.15),las=1)
  
  # illusion strength over speeds
  
  plot(-1000,-1000,main='initial trace angle and speed',xlab='external speed',ylab='initial trace angle [deg]',xlim=c(2.5,4.5),ylim=c(0,60),bty='n',ax=F)
  
  for (internalspeed in c(2,3,4)) {
    
    meanID <- aggregate(arrowdirection_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
    
    confID <- aggregate(arrowdirection_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
    
    X <- c(confID$externalspeed, rev(confID$externalspeed))
    Y <- c(confID[,2][,1], rev(confID[,2][,2]))
    polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
    
    lines(meanID$externalspeed, meanID$arrowdirection_mean, col=solids[[sprintf('%d',internalspeed)]])
    
  }
  
  legend(2.5,30,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  
  axis(1,at=c(3,4))
  axis(2,at=seq(0,30,5),las=1)
  
  # illusion strength validity
  
  plot(-1000,-1000,main='measures of illusion strength',xlab='arrow direction [deg]',ylab='initial direction [deg]',xlim=c(0,50),ylim=c(0,50),bty='n',ax=F)
  
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df)
  
  # check outliers:
  
  strengthCookD <- cooks.distance(strengthLM)
  cutoff <- 4/((nrow(df)-length(strengthLM$coefficients)-2))
  outliers <- unique(which(strengthCookD > cutoff))
  df <- df[-outliers,]
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df)
  print(summary(strengthLM))
  #log_strengthLM <- lm(log(initialdirection_mean) ~ arrowdirection_mean, data=df)
  #abline(strengthLM$coefficients, col='#b400e4ff')
  
  x <- range(df$arrowdirection_mean)
  # log_x <- log(x)
  y <- predict(strengthLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  #log_y <- predict(log_strengthLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  lines(x,y[,"fit"],col='#b400e4ff')
  
  x <- seq(min(x),max(x),.1)
  #X <- c(x,rev(x))
  X <- c(x,rev(x))
  y <- predict(strengthLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  #y <- predict(log_strengthLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  Y <- c(y[,"upr"],rev(y[,"lwr"]))
  #log_Y <- c(y[,"upr"],rev(y[,"lwr"]))
  
  
  polygon(X,Y,border=NA,col='#b400e42f')
  
  #text(df$arrowdirection_mean, df$initialdirection_mean, labels=sprintf('%d',df$participant), col='#b400e4ff')
  
  points(df$arrowdirection_mean, df$initialdirection_mean, col='#b400e4ff')
  #points(df$arrowdirection_mean, log(df$initialdirection_mean), col='#b400e4ff')
  
  #axis(1,at=seq(0,50,10))
  axis(2,at=seq(0,50,10))
  axis(1,at=seq(0,50,10))
  #axis(2,at=log(seq(10,40,10)),labels=c('10','20','30','40'))
  
  
  
  # bounds over initial illusion strength
  
  plot(-1000,-1000,main='reset position and illusion strength',xlab='arrow direction [deg]',ylab='reset position',xlim=c(0,50),ylim=c(0,0.75),bty='n',ax=F)
  
  XinitialLM <- lm(boundX_mean ~ arrowdirection_mean, data=df)
  
  x <- range(df$arrowdirection_mean)
  y <- predict(XinitialLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  lines(x,y[,"fit"],col='#005de4ff')
  
  x <- seq(min(x),max(x),.1)
  X <- c(x,rev(x))
  y <- predict(XinitialLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  Y <- c(y[,"upr"],rev(y[,"lwr"]))
  
  polygon(X,Y,border=NA,col='#005de42f')
  
  
  
  
  YinitialLM <- lm(boundY_mean ~ arrowdirection_mean, data=df)
  
  #abline(YinitialLM$coefficients, col='#0fd2e2ff')
  
  x <- range(df$arrowdirection_mean)
  y <- predict(YinitialLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  lines(x,y[,"fit"],col='#0fd2e2ff')
  
  x <- seq(min(x),max(x),.1)
  X <- c(x,rev(x))
  y <- predict(YinitialLM, newdata=data.frame(arrowdirection_mean=x), interval="confidence")
  Y <- c(y[,"upr"],rev(y[,"lwr"]))
  
  polygon(X,Y,border=NA,col='#0fd2e22f')
  
  
  #text(df$initialdirection_mean, df$boundX_mean, sprintf('%d',df$participant), col='#005de4ff')
  #text(df$initialdirection_mean, df$boundY_mean, sprintf('%d',df$participant), col='#0fd2e2ff')
  
  points(df$arrowdirection_mean, df$boundX_mean, col='#005de4ff')
  points(df$arrowdirection_mean, df$boundY_mean, col='#0fd2e2ff')
  
  legend(30,0.75,c('X','Y'),col=c('#005de4ff', '#0fd2e2ff'), pch=1, title='reset coordinate:',bty='n')
  
  axis(1,at=seq(0,50,10))
  axis(2,at=seq(0,0.75,0.15),las=1)
  
  # bounds over initial illusion strength
  
  plot(-1000,-1000,main='reset position and illusion strength',xlab='illusion strength (initial direction) [deg]',ylab='horizontal reset distance [cm]',xlim=c(5,45),ylim=c(0,0.75),bty='n',ax=F)
  
  
  # XinitialLM <- lm(boundX_mean ~ initialdirection_mean, data=df)
  # print(XinitialLM)
  # print(cor.test(df$initialdirection_mean, df$boundX_mean))
  # 
  # x <- range(df$initialdirection_mean)
  # y <- predict(XinitialLM, newdata=data.frame(initialdirection_mean=x), interval="confidence")
  # lines(x,y[,"fit"],col='#005de4ff')
  # 
  # 
  # x <- seq(min(x),max(x),.1)
  # X <- c(x,rev(x))
  # y <- predict(XinitialLM, newdata=data.frame(initialdirection_mean=x), interval="confidence")
  # Y <- c(y[,"upr"],rev(y[,"lwr"]))
  # 
  # polygon(X,Y,border=NA,col='#005de42f')
  # 
  # 
  # # =============================
  # 
  # YinitialLM <- lm(boundY_mean ~ initialdirection_mean, data=df)
  # print(YinitialLM)
  # print(cor.test(df$initialdirection_mean, df$boundY_mean))
  # 
  # x <- range(df$initialdirection_mean)
  # y <- predict(YinitialLM, newdata=data.frame(initialdirection_mean=x), interval="confidence")
  # lines(x,y[,"fit"],col='#0fd2e2ff')
  # 
  # x <- seq(min(x),max(x),.1)
  # X <- c(x,rev(x))
  # y <- predict(YinitialLM, newdata=data.frame(initialdirection_mean=x), interval="confidence")
  # Y <- c(y[,"upr"],rev(y[,"lwr"]))
  # 
  # polygon(X,Y,border=NA,col='#0fd2e22f')
  # 
  # 
  # 
  # #text(df$initialdirection_mean, df$boundX_mean, sprintf('%d',df$participant), col='#005de4ff')
  # #text(df$initialdirection_mean, df$boundY_mean, sprintf('%d',df$participant), col='#0fd2e2ff')
  # 
  # points(df$initialdirection_mean, df$boundX_mean, col='#005de4ff')
  # points(df$initialdirection_mean, df$boundY_mean, col='#0fd2e2ff')
  # 
  # # predicted Y coordinates
  # intercept <- 0.087275
  # slope <- 0.001513
  # 
  # initialdirections <- seq(10,40)
  # 
  # X <- (initialdirections * slope) + intercept
  # 
  # rad_dirs <- (initialdirections / 180) * pi
  # perc_slopes <- cos(rad_dirs) / sin(rad_dirs) 
  # 
  # Y <- X * perc_slopes
  # 
  # #plot(-1000,-1000,xlim=c(0,50),ylim=c(0,.75),xlab='initial direction',ylab='reset coordinate')
  # #lines(initialdirections,X,col='#005de4ff')
  # lines(initialdirections,Y,col='#0fd2e2ff',lty=2)
  
  
  # BETTER MODEL
  
  df$boundY_mean[which(df$externalspeed == 3)] <- 0.75 * df$boundY_mean[which(df$externalspeed == 3)]
  
  df <- df[which(df$initialdirection_mean > 5),]
  
  par <- fitResetModel(directions=df$initialdirection_mean, X=df$boundX_mean, Y=df$boundY_mean)
  
  #coords <- data.frame(X=df$boundX_mean,Y=df$boundY_mean)
  #directions <- ((90 - df$initialdirection_mean) / 180) * pi
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  
  model_resets <- resetModel(par,directions=model_directions,verbose=TRUE)
  
  #directions <- 90 - ((directions / pi) * 180)
  
  #plot(-1000,-1000,xlim=c(0,45),ylim=c(0,.8),xlab='illusion strength [deg]',ylab='reset coordinate [normalized]')
  
  lines(model_angles,model_resets$X,col='#005de4ff')
  lines(model_angles,model_resets$Y,col='#e51636ff')
  
  # points(directions,coords$X,pch=16,col='#005de42f')
  # points(directions,coords$Y,pch=16,col='#e516362f')
  points(df$initialdirection_mean,df$boundX_mean,col='#005de4ff')
  points(df$initialdirection_mean,df$boundY_mean,col='#e51636ff')
  
  
  legend(30,0.75,c('X','Y'),col=c('#005de4ff', '#e51636ff'), lty=c(1,1), title='reset coordinate:',bty='n')
  
  axis(1,at=seq(10,40,10))
  #axis(2,at=seq(0,0.75,0.15),las=1)
  axis(2,at=seq(0,10,2)/13.5,labels=c('0','2','4','6','8','10'),las=1)
  axis(4,at=seq(0,3,0.5)/4,labels=c('0.0','0.5','1.0','1.5','2.0','2.5','3.0'),las=1)
  
  mtext('reset time [s]',side=4,line=3)
  
  par['Lx'] <- par['Lx'] * 13.5
  par['Ly'] <- par['Ly'] * 4
  print(par)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


predictYfromX <- function() {
  
  intercept <- 0.087275
  slope <- 0.001513
  
  initialdirections <- seq(10,40)
  
  X <- (initialdirections * slope) + intercept
  
  rad_dirs <- (initialdirections / 180) * pi
  perc_slopes <- cos(rad_dirs) / sin(rad_dirs) 
  
  Y <- X * perc_slopes
  
  plot(-1000,-1000,xlim=c(0,50),ylim=c(0,.75),xlab='initial direction',ylab='reset coordinate')
  lines(initialdirections,X,col='#005de4ff')
  lines(initialdirections,Y,col='#0fd2e2ff')
  
  legend(15,0.7,legend=c('X (fitted on data)','Y (predicted from X and direction'),col=c('#005de4ff','#0fd2e2ff'),bty='n',lty=1)
  
}



#df <- summarizeTraceBoundsV4()
#df$externalspeed[which(df$externalspeed == 0.125)] <- 4
#df$externalspeed[which(df$externalspeed == 0.167)] <- 3

resetModel <- function(par,directions,verbose=FALSE) {
  
  Lx <- par['Lx']
  Ly <- par['Ly']
  a  <- par['a']
  
  slopes <- sin(directions) / cos(directions)
  
  Y <- (a * Lx * slopes) + ((1-a) * Ly)
  X <- (a * Lx)          + ((1-a) * Ly / slopes)
  
  resets <- data.frame(X,Y)
  
  if (verbose) {
    cat(sprintf('Lx: %0.3f, Ly: %0.3f, a: %0.3f\n',Lx,Ly,a))
  }
  
  return(resets)
  
}

resetModelMSE <- function(par,directions,coords) {
  
  # the Lx, Ly and a parameters should be between 0 and 1, but optimx checks that
  
  resets <- resetModel(par,directions)
  
  MSE <- mean( (resets$X - coords$X)^2 + (resets$Y - coords$Y)^2 )
  
  return(MSE)
  
}

library('optimx')

fitResetModel <- function(directions,X,Y) {
  
  # convert input to more useful variables:
  coords <- data.frame(X,Y)
  directions <- ((90 - directions) / 180) * pi
  
  # create search grid:
  
  # # bound to "reasonable" values:
  # Lx=seq(0,3/13.5,.05)
  # Ly=seq(.5,1,.05)
  # a=seq(0,1,.05)
  
  # bound to data:
  Lx=seq(0,median(X),diff(range(X))/10)
  Ly=seq(median(Y),13.5,diff(range(Y))/10)
  a=seq(0,1,.1)
  
  searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly, 'a'=a) # 11^3 combinations
  
  # get MSE for points in search grid:
  MSE <- apply(searchgrid,FUN=resetModelMSE,MARGIN=c(1),directions=directions,coords=coords)
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(MSE)[1:5],]
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetModelMSE,
                            method='L-BFGS-B',
                            lower=c(min(Lx),min(Ly),min(a)),
                            upper=c(max(Lx),max(Ly),max(a)),
                            directions=directions,
                            coords=coords ) )
  # print(allfits)
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  
  winpar <- as.numeric(win[1:3])
  names(winpar) <- names(win[1:3])
  
  return(winpar)
  
}

plotModel <- function() {
  
  df <- summarizeTraceBoundsV4()
  df$externalspeed[which(df$externalspeed == 0.125)] <- 4
  df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  
  df$boundY_mean[which(df$externalspeed == 3)] <- 0.75 * df$boundY_mean[which(df$externalspeed == 3)]
  
  df <- df[which(df$initialdirection_mean > 5),]
  
  #par <- fitResetModel(directions=df$initialdirection_mean, X=df$boundX_mean, Y=df$boundY_mean)
  
  
  par <- c('Lx'=mean(c(0,median(df$boundX_mean))),
           'Ly'=mean(c(median(df$boundY_mean),1)),
           'a'=0.50 )
  par <- c('Lx'=mean(df$boundX_mean),
           'Ly'=0.75,
           'a'=0.50 )
  
  par <- optimx(par,
                fn=resetModelMSE,
                method='L-BFGS-B', 
                lower=c(0,0,0), 
                upper=c(1,1,1), 
                directions=((90 - df$initialdirection_mean)/180)*pi, 
                coords=data.frame('X'=df$boundX_mean, 'Y'=df$boundY_mean) ) 
  
  par <- as.numeric(par[1:3])
  names(par) <- c('Lx','Ly','a')
  print(par * c(13.5, 4, 100))
  
  coords <- data.frame(X=df$boundX_mean,Y=df$boundY_mean)
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  
  model_resets <- resetModel(par,directions=model_directions)
  
  plot(-1000,-1000,xlim=c(0,45),ylim=c(0,.8),xlab='illusion strength [deg]',ylab='reset coordinate [norm]',bty='n',ax=F)
  
  lines(model_angles,model_resets$X,col='red')
  lines(model_angles,model_resets$Y,col='blue')
  
  points(df$initialdirection_mean,coords$X,col='red')
  points(df$initialdirection_mean,coords$Y,col='blue')
  
}

plotWeightDifferences <- function(target='pdf') {
  
  graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='simpleModelWeightChanges.pdf',onefile=TRUE,width=11,height=8)
  }
  
  
  df <- summarizeTraceBoundsV4()
  df$externalspeed[which(df$externalspeed == 0.125)] <- 4
  df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  
  df$boundY_mean[which(df$externalspeed == 3)] <- 0.75 * df$boundY_mean[which(df$externalspeed == 3)]
  
  df <- df[which(df$initialdirection_mean > 5),]
  
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  
  
  par(mfrow=c(2,3),mar=c(4,4.2,1,4.2))
  
  # # parameters with Lx < 3 cm and Ly > 2 s
  # Lx <- 0.083
  # Ly <- 0.519
  
  # parameters with no bounds:
  Lx <- 6.1/13.5
  Ly <- 0.42/4
  
  for (A in seq(0,1,.2)) {
    
    par <- c('Lx'=Lx, 'Ly'=Ly, 'a'=A)
    
    plot(-1000,-1000,main=sprintf('Lx: %0.2f cm, Ly: %0.2f s, a: %0.1f%%',Lx*13.5,Ly*4,A*100),xlab='illusion strength (initial direction) [deg]',ylab='horizontal reset distance [cm]',xlim=c(5,45),ylim=c(0,0.75),bty='n',ax=F)
    
    model_resets <- resetModel(par,directions=model_directions,verbose=TRUE)
    
    #plot(-1000,-1000,xlim=c(0,45),ylim=c(0,.8),xlab='illusion strength [deg]',ylab='reset coordinate [normalized]')
    
    lines(model_angles,model_resets$X,col='#005de4ff')
    lines(model_angles,model_resets$Y,col='#e51636ff')
    
    points(df$initialdirection_mean,df$boundX_mean,col='#005de4ff')
    points(df$initialdirection_mean,df$boundY_mean,col='#e51636ff')
    
    
    legend(30,0.75,c('X','Y'),col=c('#005de4ff', '#e51636ff'), lty=c(1,1), title='reset coordinate:',bty='n')
    
    axis(1,at=seq(10,40,10))
    #axis(2,at=seq(0,0.75,0.15),las=1)
    axis(2,at=seq(0,10,2)/13.5,labels=c('0','2','4','6','8','10'),las=1)
    axis(4,at=seq(0,3,0.5)/4,labels=c('0.0','0.5','1.0','1.5','2.0','2.5','3.0'),las=1)
    
    mtext('reset time [s]',side=4,line=3)
    
    
  }
  
  dev.off()
  
}




# CVR demo -----

plotCVRdemo <- function(target='pdf') {
  
  graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='CVRdemo.pdf',onefile=TRUE,width=8,height=8)
  }
  
  participants <- c(1,2,3,4,5,6,7,8,9)
  
  par(mfrow=c(3,3))
  
  participant <- c()
  trial <- c()
  internalspeed <- c()
  internaldirection <- c()
  fixationside <- c()
  initialdirection <- c()
  resetX <-c()
  resetY <-c()
  
  
  # scale: c(-262,262) -> 524 px == 13.5 cm
  
  # page one: raw traces
  
  for (ppno in participants) {
    
    #print(ppno)
    
    plot(-1000, -1000, main=sprintf('participant: %d',ppno), xlab='horizontal [cm]', ylab='vertical [cm]', xlim=c(-5,5), ylim=c(0,10), asp=1)
    
    ppdf <- read.csv(sprintf('../data/CVRdemo/CVRdemo_OPV4_resets_p%04d.csv', ppno), stringsAsFactors=F)
    
    trialnos <- unique(ppdf$trial_no)
    
    for (trialno in trialnos) {
      
      trialdf <- ppdf[which(ppdf$trial_no == trialno & ppdf$step == 99),]
      
      X <- trialdf$handx_pix / 38.814814815
      Y <- (trialdf$handy_pix + 262) / 38.814814815
      
      # if the endpoint Y-coordinate is negative (or close to 0?) we don't trust it
      # if it is beyond 8 cm, we also don't trust it
      
      participant <- c(participant, ppno)
      trial <- c(trial, trialno)
      internalspeed <- c(internalspeed, trialdf$internalMovement[1])
      internaldirection <- ifelse(trialdf$internalMovement[1] > 0, 1, -1)
      fixationside <- trialdf$fixationside[1]
      
      # something something
      
      X <- X * internaldirection
      # endpoint <- c(X[length(X)], Y[length(Y)])
      farpointidx <- which.max(sqrt(X^2 + Y^2))
      resetpoint <- c(X[farpointidx], Y[farpointidx]) 
      
      if ((resetpoint[2] > 0) & (resetpoint[2] < 8)) {
        
        #print('good one')
        
        indirpoint <- which(sqrt(X^2 + Y^2) > (sqrt(sum(resetpoint^2))/2))[1]
        #print(indirpoint)
        angle <- 90 - ((atan2(Y[indirpoint], X[indirpoint]) / pi) * 180)
        #print(c(Y[indirpoint], X[indirpoint], angle))
        initialdirection <- c(initialdirection, angle)
        resetX <- c(resetX, resetpoint[1])
        resetY <- c(resetY, resetpoint[2])
        
        lines(X,Y,col='#770077')
        
      } else {
        
        #print('bad one')
        
        #cat(sprintf('participant %d, trial %d\n',ppno,trialno))
        
        initialdirection <- c(initialdirection, NA)
        resetX <-c(resetX, NA)
        resetY <-c(resetY, NA)
        
        lines(X,Y,col='#CCCCCC')
        
      }
      
      
      
    }
    
    
  }
  
  CVRdata <- data.frame(participant,trial,internalspeed,internaldirection,fixationside,initialdirection,resetX,resetY)
  
  write.csv(CVRdata, '../data/CVRdemo.csv', quote=F, row.names=F)
  
  # page two: reset coordinates & model
  
  for (ppno in participants) {
    
    ylab <- ''
    if (ppno %% 3 == 1) {
      ylab <- 'reset coordinate [cm]'
    }
    
    ppdf <- CVRdata[which(CVRdata$participant == ppno & CVRdata$initialdirection > 5 & CVRdata$initialdirection < 50),]
    plot(-1000,-1000,main=sprintf('participant: %d',ppno),xlab='illusion strength [deg]',ylab=ylab,xlim=c(0,50),ylim=c(0,8),bty='n',ax=F)
    
    points(ppdf$initialdirection,ppdf$resetX,col='#005de4ff')
    points(ppdf$initialdirection,ppdf$resetY,col='#e51636ff')
    
    # get the model
    par <- fitResetModel(directions=ppdf$initialdirection, X=ppdf$resetX, Y=ppdf$resetY)
    alpha <- par[['a']]
    
    # plot the model
    model_angles <- seq(min(ppdf$initialdirection),max(ppdf$initialdirection))
    model_directions <- ((90-model_angles)/180)*pi
    
    model_resets <- resetModel(par,directions=model_directions,verbose=TRUE)
    
    lines(model_angles,model_resets$X,col='#005de4ff')
    lines(model_angles,model_resets$Y,col='#e51636ff')
    
    # add legend:
    if (ppno == 9) {
      legend(0,7,c('X','Y'),col=c('#005de4ff', '#e51636ff'), lty=c(1,1), title='reset coordinate:',bty='n')
    }
    
    text(25,7.8,sprintf('space: %0.1f%% - time: %0.1f%%', 100*alpha, 100*(1-alpha)))
    
    # add three axes:
    axis(1,at=seq(10,40,10))
    #if (ppno %% 3 == 1) {
      axis(2,at=seq(0,8,2),las=1)
    #}
    #axis(2,at=seq(0,0.75,0.15),las=1)
    
    cmps <- (13.5/4)
    ticklocs <- seq(0,8,cmps/2)
    ticklabs <- sprintf('%0.1f',ticklocs/cmps)
    axis(4,at=seq(0,8,cmps/2),labels=c('0.0','0.5','1.0','1.5','2.0'),las=1)
    if (ppno %% 3 == 0) {
      # cmps <- (13.5/4)
      # ticklocs <- seq(0,8,cmps/2)
      # ticklabs <- sprintf('%0.1f',ticklocs/cmps)
      # axis(4,at=seq(0,8,cmps/2),labels=c('0.0','0.5','1.0','1.5','2.0'),las=1)
      
      mtext('reset time [s]',side=4,line=3)
    }
    
  }
  
  dev.off()
  
}
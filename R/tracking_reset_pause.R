library('svglite')
source('R/common.R')

# Data handling -----

getPauseTrajectories <- function() {
  
  participants <- c(1,2,9,10,11,12,13)
  
  trajectories <- NA
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    df <- read.csv(sprintf('data/reset_pause/reset_p%02d.csv',ppno))
    
    trials <- unique(df$trial_no)
    
    for (trialn in trials) {
      
      trialdf <- df[which(df$trial_no == trialn & df$step == 2),]
      
      yd <- diff(trialdf$gabory_pix)
      
      pause.idx <- which(yd == 0)
      
      if (length(pause.idx) > 0) {
        
        # first we will determine which pass this was on:
        pauseonsettime <- trialdf$time_ms[pause.idx[1]] - trialdf$time_ms[which(trialdf$step == 2)][1]
        passn <- round(pauseonsettime/4000)
        
        # which samples do we care about?
        # those just before the pause, and up to some time after
        # half a second before = 15 samples
        #    1.5 seconds after = 45 samples
        
        handX <- trialdf$handx_pix
        handY <- trialdf$handy_pix
        
        handX <- handX[c(pause.idx[1]-15:pause.idx[length(pause.idx)]+45)]
        handY <- handY[c(pause.idx[1]-15:pause.idx[length(pause.idx)]+45)]
        
        # we want to stop looking the trajectories at certain points, by two rules
        # the first is implemented here:
        # when people hit the edge of the drawing tablet
        
        upto.idx <- length(handX) # start with the full trajectory
        if (any(handY == -554)) {
          upto.idx <- min(upto.idx,which(handY == -554)[1])
        }
        if (any(handY == 495)) {
          upto.idx <- min(upto.idx,which(handY == 495)[1])
        }
        
        # we align everthing so that (0,0) is where the hand is when the pause starts:
        handX <- handX - handX[33]
        handY <- handY - handY[33]
        
        # we normalize trajectories so that they all go in the same direction:
        if (trialdf$internalSpeed[1] < 0) {
          handX <- handX * -1
        }
        handY <- (handY * trialdf$externalDirection[1])
        
        # we scale it within [0,1)
        handX <- (handX / 1200)
        handY <- (handY / 1200)
        
        # the second reason to stop looking is that
        # sometimes the path includes a part of the next pass
        # so we'll exclude that too
        
        splineY <- predict(smooth.spline(x=c(1:length(handY)),y=handY,spar=0.5), x=c(1:length(handY)))$y
        
        yd <- diff(splineY)
        donotwant.idx <- which(yd < 0)
        if (length(donotwant.idx) > 0) {
          upto.idx <- min(upto.idx,min(donotwant.idx))
        }
        handX <- handX[c(1:upto.idx)]
        handY <- handY[c(1:upto.idx)]
        
        # if the resulting trajectory is too short, we don't want it:
        
        if (length(handX) < 55) {
          next()
        }
        
        trial <- rep(trialn,length(handX))
        sample <- c(1:length(handX))
        participant <- rep(ppno,length(handX))
        pause <- rep(1,length(handX))
        pass <- rep(passn, length(handX))
        if (is.data.frame(trajectories)) {
          trajectories <- rbind(trajectories,data.frame(participant,trial,pass,pause,sample,handX,handY))
        } else {
          trajectories <- data.frame(participant,trial,pass,pause,sample,handX,handY)
        }
        
      } else {
        
        # we also want the trajectories without pauses
        # let's do *both* the 1st and 3rd pass for now?
        
        for (passn in c(1,3)) {
          
          # which samples do we care about?
          # those just before the halfway point, and up to some time after
          # half a second before = 15 samples
          #    1.5 seconds after = 45 samples
          
          sample1 <- trialdf$time_ms[which(trialdf$step == 2)][1]
          
          starttime <- 2000 + sample1 + ((passn-1)*4000)
          endtime <- starttime + 4000
          
          sample.idx <- which(trialdf$time_ms >= starttime & trialdf$time_ms <= endtime)
          
          handX <- trialdf$handx_pix[sample.idx]
          handY <- trialdf$handy_pix[sample.idx]
          
          crossover.idx <- which.min(abs(trialdf$gabory_pix[sample.idx]))
          
          handX <- handX[c(crossover.idx-15:crossover.idx+45)]
          handY <- handY[c(crossover.idx-15:crossover.idx+45)]
          
          # again we stop looking the trajectories at certain points, by two rules
          # the first is implemented here:
          # when people hit the edge of the drawing tablet
          
          upto.idx <- length(handX) # start with the full trajectory
          if (any(handY == -554)) {
            upto.idx <- min(upto.idx,which(handY == -554)[1])
          }
          if (any(handY == 495)) {
            upto.idx <- min(upto.idx,which(handY == 495)[1])
          }
          
          # we align everthing so that (0,0) is where the hand is when the pause starts:
          handX <- handX - handX[16]
          handY <- handY - handY[16]
          
          # we normalize trajectories so that they all go in the same direction:
          if (trialdf$internalSpeed[1] < 0) {
            handX <- handX * -1
          }
          handY <- (handY * trialdf$externalDirection[1])
          
          # we scale it within [0,1)
          handX <- (handX / 1200)
          handY <- (handY / 1200)
          
          # the second reason to stop looking is that
          # sometimes the path includes a part of the next pass
          # so we'll exclude that too
          
          splineY <- predict(smooth.spline(x=c(1:length(handY)),y=handY,spar=0.5), x=c(1:length(handY)))$y
          
          yd <- diff(splineY)
          donotwant.idx <- which(yd < 0)
          if (length(donotwant.idx) > 0) {
            upto.idx <- min(upto.idx,min(donotwant.idx))
          }
          handX <- handX[c(1:upto.idx)]
          handY <- handY[c(1:upto.idx)]
          
          # very short trajectories are not informative
          if (length(handX) < 45) {
            next()
          }
          
          participant <- rep(ppno,length(handX))
          trial <- rep(trialn,length(handX))
          sample <- c(1:length(handX))
          pause <- rep(0,length(handX))
          pass <- rep(passn,length(handX))
          if (is.data.frame(trajectories)) {
            trajectories <- rbind(trajectories,data.frame(participant,trial,pass,pause,sample,handX,handY))
          } else {
            trajectories <- data.frame(participant,trial,pass,pause,sample,handX,handY)
          }
          
        }
        
      }
      
    }
    
    # withpause:
    # 53 samples
    
    # nopause:
    # 47 samples
    
  }
  
  write.csv(trajectories, file='data/reset_pause/trajectories.csv', quote=F, row.names=F)
  
}

getReachDirections <- function() {
  
  trajectories <- read.csv('data/reset_pause/trajectories.csv', stringsAsFactors = F)
  
  participants <- unique(trajectories$participant)
  
  participant <- c()
  trial       <- c()
  pass        <- c()
  pause       <- c()
  predir      <- c()
  postdir     <- c()
  
  for (ppno in participants) {
    
    ppdf <- trajectories[which(trajectories$participant == ppno),]
    
    trials <- unique(ppdf$trial)
    
    for (trialn in trials) {
      
      trialdf <- ppdf[which(ppdf$trial == trialn),]
      
      handX <- trialdf$handX
      handY <- trialdf$handY
      
      cross.idx <- which(handX == 0 & handY == 0)[1]
      
      pre.dir  <- atan2(diff(handY[c(1,cross.idx-1)]), diff(handX[c(1,cross.idx-1)]))
      post.dir <- atan2(diff(handY[c(cross.idx+1,length(handY))]), diff(handX[c(cross.idx+1,length(handX))]))
      
      participant <- c(participant, ppno)
      trial       <- c(trial,       trialn)
      pass        <- c(pass,        trialdf$pass[1])
      pause       <- c(pause,       trialdf$pause[1])
      predir      <- c(predir,      pre.dir)
      postdir     <- c(postdir,     post.dir)

    }
    
  }
  
  write.csv( data.frame(participant,
                       trial,
                       pass,
                       pause,
                       predir,
                       postdir),
             file='data/reset_pause/reachdir.csv',
             quote=F,
             row.names=F)
  
}

# Figures -----

plotPostPauseTrajectories <- function(target='inline') {
  
  trajectories <- read.csv('data/reset_pause/trajectories.csv', stringsAsFactors = F)
  
  participants <- unique(trajectories$participant)
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig04c.svg',width=7,height=7)
  }
  
  par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
  
  colors <- getColors()
  
  orange    <- colors$orange
  red       <- colors$yorkred
  purple    <- colors$purple
  blue      <- colors$blue
  lightblue <- colors$lightblue
  
  plot(-1000,-1000,main='',ylim=c(0,4),xlim=c(0.5,7.5),xlab='participant',ylab='',asp=1,bty='n',ax=F)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    ppdf <- trajectories[which(trajectories$participant == ppno),]
    
    for (lineno in seq(0,3)) {
      lines(c(ppidx,ppidx),c(lineno+0.1,lineno+0.8), col='#AAAAAA')
    }

    for (pause in c(1,0)) {
      
      if (pause == 1) {
        Yoffset <- 3.4
        color <- orange$t
        maxsample <- 53
        avgcol <- red$s
      } else {
        Yoffset <- 1.4
        color <- lightblue$t
        maxsample <- 47
        avgcol <- blue$s
      }
      
      pausedf <- ppdf[which(ppdf$pause == pause),]
      
      trials <- unique(pausedf$trial)
      
      for (trial in trials) {
        
        trialdf <- pausedf[which(pausedf$trial == trial),]
        
        lines(trialdf$handX+ppidx, trialdf$handY+Yoffset, col=color)
        
      }
      
      pausedft <- pausedf[which(pausedf$sample < maxsample),]
      avgReach <- aggregate(cbind(handX, handY) ~ sample, data=pausedft, FUN=mean, na.rm=TRUE)
      lines(avgReach$handX+ppidx, avgReach$handY+Yoffset, col=avgcol)
      
    }
    
  }
  
  dirs <- read.csv('data/reset_pause/reachdir.csv', stringsAsFactors = F)
  
  PausePreDirCI   <- aggregate(predir  ~ participant, data=dirs[which(dirs$pause == 1),], FUN=getConfidenceInterval)
  #PausePreDirAVG  <- aggregate(predir  ~ participant, data=dirs[which(dirs$pause == 1),], FUN=mean)
  PausePostDirAVG <- aggregate(postdir ~ participant, data=dirs[which(dirs$pause == 1),], FUN=mean)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    ppCIpreDir <- PausePreDirCI$predir[which(PausePreDirCI$participant == ppno),]
    angles <- seq(ppCIpreDir[1],ppCIpreDir[2],diff(ppCIpreDir)/50)
    X <- c(ppidx,ppidx+(0.35*cos(angles)),ppidx)
    Y <- c(2.4,2.4+(0.35*sin(angles)),2.4)
    polygon(X,Y,border=NA,col=orange$t)
    #ppAvgPre <- PausePreDirAVG$predir[which(PausePreDirAVG$participant == ppno)]
    
    ppAvgPost <- PausePostDirAVG$postdir[which(PausePostDirAVG$participant == ppno)]
    lines(c(ppidx,ppidx+(0.35*cos(ppAvgPost))),c(2.4,2.4+(0.35*sin(ppAvgPost))), col=red$s)

  }
  
  NoPausePostDirCI   <- aggregate(postdir  ~ participant, data=dirs[which(dirs$pause == 0),], FUN=getConfidenceInterval)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    ppCIpostDirNP <- NoPausePostDirCI$postdir[which(NoPausePostDirCI$participant == ppno),]
    angles <- seq(ppCIpostDirNP[1],ppCIpostDirNP[2],diff(ppCIpostDirNP)/50)
    X <- c(ppidx,ppidx+(0.35*cos(angles)),ppidx)
    Y <- c(0.4,0.4+(0.35*sin(angles)),0.4)
    polygon(X,Y,border=NA,col=lightblue$t)

    ppAvgPost <- PausePostDirAVG$postdir[which(PausePostDirAVG$participant == ppno)]
    lines(c(ppidx,ppidx+(0.35*cos(ppAvgPost))),c(0.4,0.4+(0.35*sin(ppAvgPost))), col=red$s)
    
  }
  
  axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}





# Code Graveyard -----

# plotPausedTrackingRaw <- function(target='inline') {
#   
#   participants <- c(1,2,9,10,11,12,13)
#   
#   if (target=='svg') {
#     svglite(file='doc/Fig04.svg',width=7,height=3)
#   }
#   
#   par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
#   
#   colors <- getColors()
#   
#   red <- colors$yorkred
#   
#   plot(-1000,-1000,main='',ylim=c(0,5),xlim=c(0.5,7.5),xlab='participant',ylab='internal speed [cps]',asp=1,bty='n',ax=F)
#   
#   speeds <- c(-3,3)
#   
#   for (ppidx in c(1:length(participants))) {
#     
#     ppno <- participants[ppidx]
#     
#     df <- read.csv(sprintf('data/reset_pause/reset_p%02d.csv',ppno))
#     
#     df <- df[which(df$step == 2),]
#     
#     for (speedidx in c(1:length(speeds))) {
#       
#       speed <- speeds[speedidx]
#       
#       sdf <- df[which(df$internalSpeed == speed),]
#       
#       trials <- unique(sdf$trial_no) 
#       
#       for (trialno in trials) {
#         
#         idx <- which(sdf$trial_no == trialno)
#         
#         t <- sdf$time_ms[idx]
#         t <- (t - t[1]) / 1000
#         x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
#         y <- sdf$handy_pix[idx]
#         
#         x <- (x / 1200)
#         x <- x + ppidx
#         
#         y <- (y / 1200)
#         y <- y + speedidx
#         
#         lines(x,y,col=red$t,lw=2)
#         
#       }
#       
#     }
#     
#   }
#   
#   axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
#   axis(side=2,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
#   
#   if (target %in% c('svg')) {
#     dev.off()
#   }
#   
# }

# 
# 
# plotResetTrial <- function(participant,trial) {
#   
#   library(colormap)
#   
#   df <- read.csv(sprintf('data/reset_pause/reset_p%02d.csv', participant), stringsAsFactors=F)
#   df <- df[which(df$trial==trial & df$step==2),]
#   
#   par(mfrow=c(1,1))
#   
#   # mycolors <- colormap(colormap=colormaps$velocity_blue, nshades=256)
#   mycolors <- colormap(colormap=colormaps$autumn, nshades=256)
#   mycolors <- colormap(colormap=colormaps$plasma, nshades=256)
#   
#   velocity <- sqrt(diff(df$handx_pix)^2 + diff(df$handy_pix)^2) / diff(df$time_ms)
#   
#   segment_colors <- mycolors[floor(velocity/max(velocity, na.rm=T)*256)+1]
#   
#   plot(-1000,-1000,main=sprintf('participant: %d, trial: %d',participant,trial),xlab='',ylab='',xlim=c(-500,500),ylim=c(-750,750),asp=1)
#   
#   N <- nrow(df)
#   
#   X0 <- df$handx_pix[1:N-1]
#   Y0 <- df$handy_pix[1:N-1]
#   X1 <- df$handx_pix[2:N]
#   Y1 <- df$handy_pix[2:N]
#   
#   segments(x0=X0, y0=Y0, x1=X1, y1=Y1, col=segment_colors)
#   
# }
# 
# plotResetData <- function(participants=c(1,2,9,10,11,12,13),trials=c(1:64)) {
#   
#   for (participant in participants) {
#     
#     pdf(file=sprintf('reset_p%02d.pdf',participant),width=6,height=4)
#     
#     for (trial in trials) {
#       
#       plotResetTrial(participant=participant, trial=trial)
#       
#     }
#     
#     dev.off()
#     
#   }
#   
# }

# findPauseLocations <- function() {
#   
#   participants <- c(1,2,9,10,11,12,13)
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/Fig04b.svg',width=7,height=1.5)
#   }
#   
#   
#   for (ppidx in c(1:length(participants))) {
#     
#     ppno <- participants[ppidx]
#     
#     df <- read.csv(sprintf('data/reset_pause/reset_p%02d.csv',ppno))
#     
#     trials <- unique(df$trial_no)
#     
#     haspause <- c()
#     pause_locations <- c()
#     
#     for (trial in trials) {
#       
#       trialdf <- df[which(df$trial_no == trial & df$step == 2),]
#       
#       yd <- diff(trialdf$gabory_pix)
#       
#       pause.idx <- which(yd == 0)
#       
#       haspause <- c(haspause, (length(pause.idx) > 0))
#       
#       if (length(pause.idx) > 0) {
#         
#         pause_locations <- c(pause_locations, trialdf$gabory_pix[pause.idx[1]])
#         
#       }
#       
#     }
#     
#     print(pause_locations)
#     
#   }
#   
# }
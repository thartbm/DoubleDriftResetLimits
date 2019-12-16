library('svglite')
source('R/common.R')

# Data handling -----

getPauseTrajectories <- function(normalizeX=TRUE) {
  
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
        
        #cat(sprintf('pause samples: %d\n',length(pause.idx)))
        # first we will determine which pass this was on:
        pauseonsettime <- trialdf$time_ms[pause.idx[1]] - trialdf$time_ms[which(trialdf$step == 2)][1]
        passn <- round(pauseonsettime/4000)
        
        # which samples do we care about?
        # those just before the pause, and up to some time after
        # half a second before = 15 samples
        #    1.5 seconds after = 45 samples
        
        handX <- trialdf$handx_pix
        handY <- trialdf$handy_pix
        
        handX <- handX[c(pause.idx[1]-15:pause.idx[length(pause.idx)]+30)]
        handY <- handY[c(pause.idx[1]-15:pause.idx[length(pause.idx)]+30)]
        
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
        if (normalizeX) {
          handX <- handX - handX[24]
        }
        handY <- handY - handY[24]
        
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
        
        if (length(handX) < 50) { # 15 + 8 + 30 = 53
          next()
        }
        
        trial <- rep(trialn,length(handX))
        sample <- c(1:length(handX)) - 24
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
          
          handX <- handX[c(crossover.idx-15:crossover.idx+30)]
          handY <- handY[c(crossover.idx-15:crossover.idx+30)]
          
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
          if (normalizeX) {
            handX <- handX - handX[16]
          }
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
          if (length(handX) < 42) { # 15 + 30 = 45
            #cat('removed\n')
            next()
          } else {
            #cat('kept\n')
          }
          
          participant <- rep(ppno,length(handX))
          trial <- rep(trialn,length(handX))
          sample <- c(1:length(handX)) - 16
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


injectResets <- function(trajectories, proportion=c(.05,.05), approx=FALSE, seed=NA, type='jump', onesided=FALSE) {
  
  # set the same proportion for without AND with pause trials, using a single number:
  if (length(proportion) == 1) { proportion <- rep(proportion,2) }
  
  if (is.numeric(seed)) {
    set.seed(seed=seed)
  }
  
  participants <- unique(trajectories$participant)
  
  nChanged <- 0
  nTotal <- 0
  
  for (participant in participants) {
    
    for (pause in c(1,0)) {
      
      uniqueTrials <- unique(trajectories[which(trajectories$participant == participant & trajectories$pause == pause),c('trial','pass')])
      #print(uniqueTrials)
      nTrials <- nrow(uniqueTrials)
      #print(nTrials)
      
      #trialPassList <- aggregate(participant ~ trial + pass, data=trajectories[which(trajectories$participant == participant & trajectories$pause == pause),], FUN=median)
      #nTrials <- dim(trialPassList)[1]
      #print(nTrials)
      
      nTotal <- nTotal + nTrials
      
      if (approx) {
        fraction <- nTrials * proportion[pause+1]
        nChange <- floor(fraction)
        nChange <- nChange + (runif(1) <= (fraction - nChange))
      } else {
        nChange <- round(nTrials * proportion[pause+1])
      }
      nChanged <- nChanged + nChange
      
      #cat(sprintf('change %d / %d\n',nChange,nTrials))
      #changeTrialPass <- trialPassList[sample(seq(1:nTrials),size=nChange),c('trial','pass')]
      changeTrialPass <- uniqueTrials[sample(seq(1:nTrials),size=nChange),c('trial','pass')]
      #print(changeTrialPass)
      
      for (ctIDX in c(1:dim(changeTrialPass)[1])) {
        
        # we're going to change this in place
        # so we don't have to touch the rest of the data frame
        # this means we get the sample indices for the trajectory we will change:
        trialIDX <- which(    trajectories$participant == participant &
                              trajectories$pause       == pause &
                              trajectories$trial       == changeTrialPass$trial[ctIDX] &
                              trajectories$pass        == changeTrialPass$pass[ctIDX]  ) 
        
        #print(trialIDX)
        
        trajectory <- trajectories[trialIDX,]
        
        # see the size of the discrepancy at the end of the pause (or halfway through the pass)
        crossIDX <- which(trajectory$sample == 0)
        dist <- trajectory$handX[crossIDX]
        
        if (onesided) { dist <- abs(dist) }
        
        # print(trialIDX)
        # print(crossIDX)
        # print(dim(trajectory)[1])
        
        RT <- sample(c(5:10),1)
        shifted.idx <- c((crossIDX+RT):dim(trajectory)[1])
        
        if (type == 'jump') {
          
          step <- 1/sample(c(1:5),1)
          shiftby <- pmin(rep(1,length(shifted.idx)),c(1:length(shifted.idx))*step)
          shiftby <- shiftby * ((runif(1)/2)+0.5)
          
          # correct for this discrepancy after always 150 ms, and in 1 single sample:
          trajectory$handX[shifted.idx] <- trajectory$handX[shifted.idx] - (dist * shiftby)
          
        }
        
        if (type == 'wall') {
          
          handX <- trajectory$handX[shifted.idx]
          
          slope <- (handX[1] - handX[length(handX)]) / length(shifted.idx)
          
          trajectory$handX[shifted.idx] <- trajectory$handX[shifted.idx] + (c(1:length(shifted.idx) * slope))
          
        }
        
        # put this back in the trajectories data frame:
        trajectories[trialIDX,] <- trajectory
        
      }
      
    }
    
  }
  

  
  
  #cat(sprintf('proportion changed: %0.5f\n',nChanged/nTotal))
  
  return(trajectories)
  
}

normalizeTrajectoryX <- function(trajectories) {
  
  participants <- unique(trajectories$participant)
  
  for (participant in participants) {
    
    for (pause in c(1,0)) {
      
      uniqueTrials <- unique(trajectories[which(trajectories$participant == participant & trajectories$pause == pause),c('trial','pass')])
      
      
      for (trialno in c(1:dim(uniqueTrials)[1])) {
        
        # we're going to change this in place
        # so we don't have to touch the rest of the data frame
        # this means we get the sample indices for the trajectory we will change:
        trialIDX <- which(    trajectories$participant == participant &
                              trajectories$pause       == pause &
                              trajectories$trial       == uniqueTrials$trial[trialno] &
                              trajectories$pass        == uniqueTrials$pass[trialno]  ) 
        
        
        trajectory <- trajectories[trialIDX,]
        
        trajectory$handX <- trajectory$handX - trajectory$handX[which(trajectory$sample == 0)]
        
        trajectories[trialIDX,] <- trajectory 
        
      }
      
    }
    
  }
  
  return(trajectories)
  
}




# Figures -----

plotPostPauseTrajectories <- function(target='inline') {
  
  trajectories <- read.csv('data/reset_pause/trajectories.csv', stringsAsFactors = F)
  
  trajectories <- injectResets(trajectories, proportion=c(.05,.05), approx=TRUE)
  
  participants <- unique(trajectories$participant)
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig04c.svg',width=8,height=3)
  }
  
  par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
  
  colors <- getColors()
  
  orange    <- colors$orange
  red       <- colors$yorkred
  purple    <- colors$purple
  blue      <- colors$blue
  lightblue <- colors$lightblue
  
  plot(-1000,-1000,main='',ylim=c(0,2),xlim=c(0.5,8.5),xlab='participant',ylab='',asp=1,bty='n',ax=F)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    ppdf <- trajectories[which(trajectories$participant == ppno),]
    
    for (lineno in seq(0,1)) {
      lines(c(ppidx-.4,ppidx+.4),c(lineno+0.4,lineno+0.4), col='#999999', lty=2)
    }
    
    for (lineno in seq(0,1)) {
      lines(c(ppidx,ppidx),c(lineno+0.1,lineno+0.8), col='#999999')
    }
    
    for (pause in c(1,0)) {
      
      if (pause == 1) {
        Yoffset <- 1.4
        color <- orange$t
        maxsample <- 25 # 53
        avgcol <- red$s
      } else {
        Yoffset <- 0.4
        color <- lightblue$t
        maxsample <- 25 # 47
        avgcol <- blue$s
      }
      
      pausedf <- ppdf[which(ppdf$pause == pause),]
      
      trials <- unique(pausedf$trial)
      
      for (trial in trials) {
        
        trialdf <- pausedf[which(pausedf$trial == trial),]
        
        passes <- unique(trialdf$pass)
        
        for (pass in passes) {
          
          passdf <- trialdf[which(trialdf$pass == pass),]
          
          lines(passdf$handX+ppidx, passdf$handY+Yoffset, col=color)
          
        }
        
      }
      
      pausedft <- pausedf[which(abs(pausedf$sample) < maxsample),]
      avgReach <- aggregate(cbind(handX, handY) ~ sample, data=pausedft, FUN=mean, na.rm=TRUE)
      lines(avgReach$handX+ppidx, avgReach$handY+Yoffset, col=avgcol)
      
    }
    
  }
  
  axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
  axis(side=2,at=c(0.5,1.5), labels=c('no\npause','with\npause'),adj=0.5)
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

plotAvgHeading <- function(target='inline') {
  
  trajectories <- read.csv(file='data/reset_pause/trajectories.csv', stringsAsFactors = F)
  
  #trajectories <- injectResets(trajectories, proportion=c(.05,.05), approx=TRUE)
  
  participants <- unique(trajectories$participant)
  
  if (target=='svg') {
    svglite::svglite(file='doc/Fig04e.svg',width=8,height=4)
  }
  
  par(mfrow=c(1,2),mar=c(4.5,4.1,2.1,0.1))
  
  colors <- getColors()
  
  orange    <- colors$orange
  red       <- colors$yorkred
  purple    <- colors$purple
  blue      <- colors$blue
  lightblue <- colors$lightblue
  
  # # let's first see how many samples we can go forward and backward from the corssing point:
  # maxsamplesWP <- aggregate(sample ~ participant + trial, data=trajectories[which(trajectories$pause==1),], FUN=max)
  # minsamplesWP <- aggregate(sample ~ participant + trial, data=trajectories[which(trajectories$pause==1),], FUN=min)
  # maxsamplesNP <- aggregate(sample ~ participant + trial, data=trajectories[which(trajectories$pause==0),], FUN=max)
  # minsamplesNP <- aggregate(sample ~ participant + trial, data=trajectories[which(trajectories$pause==0),], FUN=min)
  # 
  # # we can go from sample -26 (with pause) or -15 (no pause) to sample 26
  # print(max(minsamplesWP$sample))
  # print(min(maxsamplesWP$sample))
  # print(max(minsamplesNP$sample))
  # print(min(maxsamplesNP$sample))
  
  for (pause in c(1,0)) {
    
    # create the subplot:
    plot(-1000,-1000,main=c('no pause','with pause')[pause+1],ylim=c(0,1),xlim=c(0,1),xlab='heading [°]',ylab='time [ms]',asp=1,bty='n',ax=F)
    
    lines(c(.5,.5),c(0,1),col='#999999')
    text(0.51,0,'no illusion',srt=90,adj=c(0,1))
    
    lines(c(0,1),c(.5,.5),col='#999999',lty=2)
    text(1,.51,c('cross point','pause offset')[pause+1],adj=c(1,0))
    
    if (pause == 1) {
      lines(c(0,1),rep((-8/60)+0.5,2),col='#999999',lty=2)
      text(1,(-8/60)+0.51,'pause onset',adj=c(1,0))
    }
    
    # we only want trajectories for the current subplot: with or without pause
    subtraj <- trajectories[which(trajectories$pause == pause),]
    
    # we limit the average to the minimum sample we have for every participant:
    subtraj <- subtraj[which(subtraj$sample > c(-16,-24)[pause+1] & subtraj$sample < 27),]
    
    # we'll get samples-1 heading directions:
    headinglength <- c(41,49)[pause+1]
    
    # we make a matrix for the average heading for every participant:
    averageHeading <- matrix(data=NA, nrow=headinglength, ncol=length(participants))
    
    for (ppidx in c(1:length(participants))) {
      
      ppno <- participants[ppidx]
      
      pptraj <- subtraj[which(subtraj$participant == ppno),]
      
      trials <- unique(pptraj$trial)
      
      headings <- c()
      
      for (trial in trials) {
        
        trialdf <- pptraj[which(pptraj$trial == trial),]
        
        passes <- unique(trialdf$pass)
        
        for (pass in passes) {
          
          passdf <- trialdf[which(trialdf$pass == pass),]
          #print(range(passdf$sample))
          
          X <- passdf$handX
          Y <- passdf$handY
          
          H <-  ( atan2(diff(Y),diff(X)) / pi ) * 180
          # print(length(H))
          
          headings <- c( headings, H )
          
        }
        
      }

      avgPPHeading <- colMeans(matrix(data=headings, ncol=headinglength, nrow=length(headings)/headinglength, byrow=TRUE))

      averageHeading[,ppidx] <- avgPPHeading
      
    }
    
    #print(averageHeading)
    
    # calculate the 95% CI for every sample:
    CI <- apply( averageHeading, MARGIN=c(1), FUN=getConfidenceInterval )
    
    # plot the CI:
    CIcol <- c(lightblue$t,orange$t)[pause+1]
    polX <- c(CI[1,],rev(CI[2,])) / 180
    Y <- (seq(c(-14,-22)[pause+1],26) / 60) + 0.5
    polY <- c(Y,rev(Y))
    polygon(polX,polY,col=CIcol,border=NA)
    
    # calculate the mean:
    AVG <- apply( averageHeading, MARGIN=c(1), FUN=mean ) / 180
    
    # plot the mean:
    AVGcol <- c(blue$s, red$s)[pause+1]
    lines(AVG,Y,col=AVGcol)
    
    
    axis(side=1,at=(c(45,90,180)/180),labels=c('45','90','180'))
    axis(side=2,at=c(0.00, 0.25, 0.50, 0.75, 1.00),labels=c('-1000','-500','0','500','1000'))
    
  }
  
  

  
  
  
  
  # #trajectories <- trajectories[which(abs(trajectories$sample) <= fullSamples),]
  # 
  # # the dataset needs to be trimmed, and normalized:
  # post <- trajectories[which(trajectories$sample >= 0),]
  # 
  # pre <- trajectories[which( trajectories$pause == 1 &
  #                            trajectories$sample <= 0),]
  # #pre$sample <- pre$sample * -1
  # 
  # post$post <- 1
  # pre$post <- 0
  # 
  # trimmedTajectories <- rbind(post,pre)
  # 
  # participants <- unique(trimmedTajectories$participant)
  # 
  # lines(c(0.1,0.35,0.35,0.35,0.9),(c(45,45,180,45,45)*(0.8/180))+2.1,col=purple$s)
  # text(0.5,2.1,'jump reset',cex=0.7)
  # 
  # lines(c(0.1,0.35,0.35,0.9),(c(45,45,90,90)*(0.8/180))+1.1,col=purple$s)
  # text(0.5,1.1,'hit-the-wall',cex=0.7)
  # 
  # lines(c(1.1,1.9),c(0.1,0.1))
  # text(1.5,-0.1,'500 ms',cex=0.7)
  # 
  # lines(c(0.9,0.9),(c(45,135)*(0.8/180))+0.1)
  # text(0.8,90*(0.8/180),'90°',cex=0.7,adj=c(1,0))
  # 
  # text(c(4.5,4.5,4.5),c(2.9,1.9,0.9),c('pre-pause','post-pause','no pause'),cex=0.9)
  # 
  # for (ppidx in c(1:length(participants))) {
  #   
  #   for (Yoffset in c(0.1,1.1,2.1)) {
  #     lines(c(0.1,0.9)+ppidx,(c(90,90)*(0.8/180))+Yoffset,col='#999999')
  #     lines(c(0.1,0.9)+ppidx,(c(45,45)*(0.8/180))+Yoffset,col='#999999',lty=2)
  #   }
  #   
  #   ppTraj <- trimmedTajectories[which(trimmedTajectories$participant == participants[ppidx]),]
  #   
  #   # there are three conditions:
  #   conditions <- list( 'prePause'  = list('post'=0, 'pause'=1, 'Yoffsets'=list(2.1), 'cols'=list('l'=orange$t, 'a'=orange$s)),
  #                       'postCross' = list('post'=1, 'pause'=0, 'Yoffsets'=list(0.1), 'cols'=list('l'=lightblue$t, 'a'=blue$s)),
  #                       'postPause' = list('post'=1, 'pause'=1, 'Yoffsets'=list(1.1), 'cols'=list('l'=red$t, 'a'=red$s)))
  #   
  #   for (condition in conditions) {
  #     
  #     #print(condition)
  #     
  #     conditiondf <- ppTraj[which(ppTraj$post  == condition[['post']] &
  #                                 ppTraj$pause == condition[['pause']]),]
  #     
  #     passes <- unique(conditiondf$pass)
  #     
  #     for (passno in passes) {
  #       
  #       passdf <- conditiondf[which(conditiondf$pass == passno),]
  #       
  #       trialnos <- unique(passdf$trial)
  #       
  #       for (trialno in trialnos) {
  #         
  #         trialdf <- passdf[which(passdf$trial == trialno),]
  #         
  #         X <- trialdf$handX
  #         Y <- trialdf$handY
  #         
  #         if (condition[['post']] == 0) {
  #           XYdf <- rotateCoordinates(data.frame(rev(X),rev(Y)),180)
  #           X <- XYdf$rev.X
  #           Y <- XYdf$rev.Y
  #         }
  #         
  #         heading <- ( atan2(diff(Y),diff(X)) / pi ) * 180
  #         
  #         #print( heading )
  #         
  #         for (Yoffset in condition[['Yoffsets']]) {
  #           
  #           lines( seq(0.1,0.9,0.8/(length(heading)-1)) + ppidx, (heading*(0.8/180)) + Yoffset, col=condition[['cols']]$l )
  #                   
  #         }
  #         
  #       }
  #       
  #     }
  #       
  #   }
  #   
  # }
  

  if (target %in% c('svg')) {
    dev.off()
  }

}


examplePlot <- function(target='inline') {
  
  seed       <- 1937
  proportion <- c(0.15, 0.80)
  
  # participants: 1, 2, 9, 10, 11, 12, 13
  examplePP  <- 13
  
  
  trajectories <- read.csv(file='data/reset_pause/trajectories.csv', stringsAsFactors = F)
  rawTrajectories <- trajectories
  trajectories <- normalizeTrajectoryX(trajectories)
  
  colors <- getColors()
  
  orange    <- colors$orange
  red       <- colors$yorkred
  purple    <- colors$purple
  blue      <- colors$blue
  lightblue <- colors$lightblue
  
  if (target == 'svg') {
    svglite::svglite('doc/examplePlot-exp1b.svg', width=8, height=7)
  }
  
  layout( matrix(c(1,1,3,3,5,5,7,7,9,9,1,1,3,3,5,5,7,7,9,9,2,2,4,4,6,6,8,8,10,10,2,2,4,4,6,6,8,8,10,10,11,11,11,11,11,12,12,12,12,12,11,11,11,11,11,12,12,12,12,12,11,11,11,11,11,12,12,12,12,12,11,11,11,11,11,12,12,12,12,12,11,11,11,11,11,12,12,12,12,12), nrow=9,ncol=10,byrow=TRUE) )
  par(mar=c(4.5,4.1,2.1,0.1))
  
  
  
  # plot example participant raw data:
  
  ppdf <- trajectories[which(trajectories$participant == examplePP),]
  
  for (pause in c(1,0)) {
    
    if (pause == 1) {
      main <- sprintf('participant %d', examplePP)
      ylab <- 'with pause'
    } else {
      main <- ''
      ylab <- 'no pause'
    }
    
    plot(-1000,-1000,main=main,xlab='',ylab=ylab,xlim=c(0,1),ylim=c(0,1),ax=F,bty='n')
    
    lineno <- 0
    lines(c(0.2,0.8),c(lineno+0.4,lineno+0.4), col='#999999', lty=2)
    lines(c(0.5,0.5),c(lineno+0.1,lineno+0.8), col='#999999')
    
    Yoffset <- 0.4
    if (pause == 1) {
      color <- orange$t
      maxsample <- 25 # 53
      avgcol <- red$s
    } else {
      color <- lightblue$t
      maxsample <- 25 # 47
      avgcol <- blue$s
    }
    
    pausedf <- ppdf[which(ppdf$pause == pause),]
    
    trials <- unique(pausedf$trial)
    
    for (trial in trials) {
      
      trialdf <- pausedf[which(pausedf$trial == trial),]
      
      passes <- unique(trialdf$pass)
      
      for (pass in passes) {
        
        passdf <- trialdf[which(trialdf$pass == pass),]
        
        lines(passdf$handX+0.5, passdf$handY+Yoffset, col=color)
        
      }
      
    }
    
    pausedft <- pausedf[which(abs(pausedf$sample) < maxsample),]
    avgReach <- aggregate(cbind(handX, handY) ~ sample, data=pausedft, FUN=mean, na.rm=TRUE)
    lines(avgReach$handX+0.5, avgReach$handY+Yoffset, col=avgcol)
    
  }
  
  
  
  
  # now plot example participant with resets added
  
  trajectories <- injectResets(rawTrajectories, proportion=proportion, approx=TRUE, seed=seed, type='wall', onesided=TRUE)
  trajectories <- normalizeTrajectoryX(trajectories)
  
  # plot example participant raw data:
  
  ppdf <- trajectories[which(trajectories$participant == examplePP),]
  
  for (pause in c(1,0)) {
    
    main <- sprintf('add %0.0f%% resets', proportion[pause+1]*100)
    ylab <- ''

    plot(-1000,-1000,main=main,xlab='',ylab=ylab,xlim=c(0,1),ylim=c(0,1),ax=F,bty='n')
    
    lineno <- 0
    lines(c(0.2,0.8),c(lineno+0.4,lineno+0.4), col='#999999', lty=2)
    lines(c(0.5,0.5),c(lineno+0.1,lineno+0.8), col='#999999')
    
    Yoffset <- 0.4
    if (pause == 1) {
      color <- orange$t
      maxsample <- 25 # 53
      avgcol <- red$s
    } else {
      color <- lightblue$t
      maxsample <- 25 # 47
      avgcol <- blue$s
    }
    
    pausedf <- ppdf[which(ppdf$pause == pause),]
    
    trials <- unique(pausedf$trial)
    
    for (trial in trials) {
      
      trialdf <- pausedf[which(pausedf$trial == trial),]
      
      passes <- unique(trialdf$pass)
      
      for (pass in passes) {
        
        passdf <- trialdf[which(trialdf$pass == pass),]
        
        lines(passdf$handX+0.5, passdf$handY+Yoffset, col=color)
        
      }
      
    }
    
    pausedft <- pausedf[which(abs(pausedf$sample) < maxsample),]
    avgReach <- aggregate(cbind(handX, handY) ~ sample, data=pausedft, FUN=mean, na.rm=TRUE)
    lines(avgReach$handX+0.5, avgReach$handY+Yoffset, col=avgcol)
    
  }
  
  participants <- unique(trajectories$participant)
  
  
  
  
  # plot all average trajectories:
  
  for (pause in c(1,0)) {
    
    main <- ''
    if (pause == 1) { main='all mean paths' }
    
    plot(-1000,-1000,main=main,xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),ax=F,bty='n')
    
    # we get only the data for the pause condition we want now:
    pausedf <- trajectories[which(trajectories$pause == pause),]
    
    for (participant in participants) {
      
      # and then select only the data for the current participant:
      ppdf <- pausedf[which(pausedf$participant == participant),]
      
      lineno <- 0
      lines(c(0.2,0.8),c(lineno+0.4,lineno+0.4), col='#999999', lty=2)
      lines(c(0.5,0.5),c(lineno+0.1,lineno+0.8), col='#999999')
      
      Yoffset <- 0.4
      if (pause == 1) {
        color <- orange$t
        maxsample <- 25 # 53
        avgcol <- red$s
      } else {
        color <- lightblue$t
        maxsample <- 25 # 47
        avgcol <- blue$s
      }
      
      pausedft <- ppdf[which(abs(ppdf$sample) < maxsample),]
      avgReach <- aggregate(cbind(handX, handY) ~ sample, data=pausedft, FUN=mean, na.rm=TRUE)
      lines(avgReach$handX+0.5, avgReach$handY+Yoffset, col=avgcol)
      
    }
    
  }
  
  
  # plot average heading
  
  for (pause in c(1,0)) {
    
    main <- ''
    if (pause == 1) { main <- 'mean heading' }
    
    # create the subplot:
    plot(-1000,-1000,main=main,ylim=c(0,1),xlim=c(0,1),ylab='heading [°]',xlab='time [ms]',asp=1,bty='n',ax=F)
    
    lines(c(0,1),c(.5,.5),col='#999999')
    #text(0.51,0,'no illusion',srt=90,adj=c(0,1))
    
    lines(c(.5,.5),c(0,1),col='#999999',lty=2)
    #text(1,.51,c('cross point','pause offset')[pause+1],adj=c(1,0))
    
    if (pause == 1) {
      lines(rep((-8/60)+0.5,2),c(0,1),col='#999999',lty=2)
      #text(1,(-8/60)+0.51,'pause onset',adj=c(1,0))
    }
    
    # we only want trajectories for the current subplot: with or without pause
    subtraj <- trajectories[which(trajectories$pause == pause),]
    
    # we limit the average to the minimum sample we have for every participant:
    subtraj <- subtraj[which(subtraj$sample > c(-16,-24)[pause+1] & subtraj$sample < 27),]
    
    # we'll get samples-1 heading directions:
    headinglength <- c(41,49)[pause+1]
    
    # we make a matrix for the average heading for every participant:
    averageHeading <- matrix(data=NA, nrow=headinglength, ncol=length(participants))
    
    for (ppidx in c(1:length(participants))) {
      
      ppno <- participants[ppidx]
      
      pptraj <- subtraj[which(subtraj$participant == ppno),]
      
      trials <- unique(pptraj$trial)
      
      headings <- c()
      
      for (trial in trials) {
        
        trialdf <- pptraj[which(pptraj$trial == trial),]
        
        passes <- unique(trialdf$pass)
        
        for (pass in passes) {
          
          passdf <- trialdf[which(trialdf$pass == pass),]
          #print(range(passdf$sample))
          
          X <- passdf$handX
          Y <- passdf$handY
          
          H <-  ( atan2(diff(Y),diff(X)) / pi ) * 180
          # print(length(H))
          
          headings <- c( headings, H )
          
        }
        
      }
      
      avgPPHeading <- colMeans(matrix(data=headings, ncol=headinglength, nrow=length(headings)/headinglength, byrow=TRUE))
      
      averageHeading[,ppidx] <- avgPPHeading
      
    }
    
    #print(averageHeading)
    
    # calculate the 95% CI for every sample:
    CI <- apply( averageHeading, MARGIN=c(1), FUN=getConfidenceInterval )
    
    # plot the CI:
    CIcol <- c(lightblue$t,orange$t)[pause+1]
    polX <- c(CI[1,],rev(CI[2,])) / 180
    Y <- (seq(c(-14,-22)[pause+1],26) / 60) + 0.5
    polY <- c(Y,rev(Y))
    polygon(polY,polX,col=CIcol,border=NA)
    
    # calculate the mean:
    AVG <- apply( averageHeading, MARGIN=c(1), FUN=mean ) / 180
    
    # plot the mean:
    AVGcol <- c(blue$s, red$s)[pause+1]
    lines(Y,AVG,col=AVGcol)
    
    
    # axis(side=1,at=(c(45,90,180)/180),labels=c('45','90','180'))
    # axis(side=2,at=c(0.00, 0.25, 0.50, 0.75, 1.00),labels=c('-1000','-500','0','500','1000'))
    
    axis(side=2,at=(c(0,90,180)/180),labels=c('0','90','180'))
    axis(side=1,at=c(0.00, 0.50, 1.00),labels=c('-1000','0','1000'))
    
  }
  
  
  
  
  # plot average yaw
  
  for (pause in c(1,0)) {
    
    main <- ''
    if (pause == 1) { main <- 'absolute yaw' }
    
    # create the subplot:
    plot(-1000,-1000,main=main,ylim=c(0,1),xlim=c(0,1),ylab='mean yaw [°]',xlab='time [ms]',asp=1,bty='n',ax=F)
    
    lines(c(.5,.5),c(0,1),col='#999999',lty=2)
    # cross point / and of pause
    
    # we only want trajectories for the current subplot: with or without pause
    subtraj <- trajectories[which(trajectories$pause == pause),]
    
    # we limit the average to the minimum sample we have for every participant:
    subtraj <- subtraj[which(subtraj$sample > c(-16,-24)[pause+1] & subtraj$sample < 27),]
    
    # we'll get samples-2 yaws:
    yawlength <- c(40,48)[pause+1]
    
    # we make a matrix for the average yaw for every participant:
    averageYaw <- matrix(data=NA, nrow=yawlength, ncol=length(participants))
    
    for (ppidx in c(1:length(participants))) {
      
      ppno <- participants[ppidx]
      
      pptraj <- subtraj[which(subtraj$participant == ppno),]
      
      trials <- unique(pptraj$trial)
      
      yaws <- c()
      
      for (trial in trials) {
        
        trialdf <- pptraj[which(pptraj$trial == trial),]
        
        passes <- unique(trialdf$pass)
        
        for (pass in passes) {
          
          passdf <- trialdf[which(trialdf$pass == pass),]
          #print(range(passdf$sample))
          
          X <- passdf$handX
          Y <- passdf$handY
          
          H <-  abs( diff( ( atan2(diff(Y),diff(X)) / pi ) * 180 ) )
          # print(length(H))
          
          yaws <- c( yaws, H )
          
        }
        
      }
      
      avgPPyaw <- colMeans(matrix(data=yaws, ncol=yawlength, nrow=length(yaws)/yawlength, byrow=TRUE))
      
      averageYaw[,ppidx] <- avgPPyaw
      
    }
    
    # calculate the 95% CI for every sample:
    CI <- apply( averageYaw, MARGIN=c(1), FUN=getConfidenceInterval )
    
    # plot the CI:
    CIcol <- c(lightblue$t,orange$t)[pause+1]
    polY <- c(CI[1,],rev(CI[2,])) / 20
    X <- (seq(c(-14,-22)[pause+1],25) / 60) + 0.5
    polX <- c(X,rev(X))
    polygon(polX,polY,col=CIcol,border=NA)
    
    # calculate the mean:
    AVG <- apply( averageYaw, MARGIN=c(1), FUN=mean ) / 20
    
    # plot the mean:
    AVGcol <- c(blue$s, red$s)[pause+1]
    lines(X,AVG,col=AVGcol)
    
    
    # axis(side=1,at=(c(45,90,180)/180),labels=c('45','90','180'))
    # axis(side=2,at=c(0.00, 0.25, 0.50, 0.75, 1.00),labels=c('-1000','-500','0','500','1000'))
    
    axis(side=2,at=(c(0,10,20)/20),labels=c('0','10','20'))
    axis(side=1,at=c(0.00, 0.50, 1.00),labels=c('-1000','0','1000'))
    
  }
  
  
  
  
  # plot average yaw
  
  trajectories <- rawTrajectories
  
  for (pause in c(1,0)) {
    
    if ( pause == 1 ) { main <- 'yaw: with pause' }
    if ( pause == 0 ) { main <- 'yaw: no pause' }
    
    # create the subplot:
    plot(-1000,-1000,main=main,ylim=c(0,1),xlim=c(0,1),ylab='mean absolute yaw [°]',xlab='time [ms]',asp=1,bty='n',ax=F)
    
    lines(c(.5,.5),c(0,1),col='#999999',lty=2)
    # cross point / and of pause
    
    # we only want trajectories for the current subplot: with or without pause
    subtraj <- trajectories[which(trajectories$pause == pause),]
    
    # we limit the average to the minimum sample we have for every participant:
    subtraj <- subtraj[which(subtraj$sample > c(-16,-24)[pause+1] & subtraj$sample < 27),]
    
    # we'll get samples-2 yaws:
    yawlength <- c(40,48)[pause+1]
    
    # we make a matrix for the average yaw for every participant:
    averageYaw <- matrix(data=NA, nrow=yawlength, ncol=length(participants))
    
    for (ppidx in c(1:length(participants))) {
      
      ppno <- participants[ppidx]
      
      pptraj <- subtraj[which(subtraj$participant == ppno),]
      
      trials <- unique(pptraj$trial)
      
      yaws <- c()
      
      for (trial in trials) {
        
        trialdf <- pptraj[which(pptraj$trial == trial),]
        
        passes <- unique(trialdf$pass)
        
        for (pass in passes) {
          
          passdf <- trialdf[which(trialdf$pass == pass),]
          #print(range(passdf$sample))
          
          X <- passdf$handX
          Y <- passdf$handY
          
          H <-  abs( diff( ( atan2(diff(Y),diff(X)) / pi ) * 180 ) )
          # print(length(H))
          
          yaws <- c( yaws, H )
          
        }
        
      }
      
      avgPPyaw <- colMeans(matrix(data=yaws, ncol=yawlength, nrow=length(yaws)/yawlength, byrow=TRUE))
      
      averageYaw[,ppidx] <- avgPPyaw
      
    }
    
    # calculate the 95% CI for every sample:
    CI <- apply( averageYaw, MARGIN=c(1), FUN=getConfidenceInterval )
    
    # plot the CI:
    CIcol <- c(lightblue$t,orange$t)[pause+1]
    polY <- c(CI[1,],rev(CI[2,])) / 20
    X <- (seq(c(-14,-22)[pause+1],25) / 60) + 0.5
    polX <- c(X,rev(X))
    polygon(polX,polY,col=CIcol,border=NA)
    
    # calculate the mean:
    AVG <- apply( averageYaw, MARGIN=c(1), FUN=mean ) / 20
    
    # plot the mean:
    AVGcol <- c(blue$s, red$s)[pause+1]
    lines(X,AVG,col=AVGcol)
    
    
    # axis(side=1,at=(c(45,90,180)/180),labels=c('45','90','180'))
    # axis(side=2,at=c(0.00, 0.25, 0.50, 0.75, 1.00),labels=c('-1000','-500','0','500','1000'))
    
    axis(side=2,at=(c(0,10,20)/20),labels=c('0','10','20'))
    axis(side=1,at=c(0.00, 0.50, 1.00),labels=c('-1000','0','1000'))
    
  }
  
  
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}


# Code Graveyard -----

# getReachDirections <- function() {
#   
#   trajectories <- read.csv('data/reset_pause/trajectories.csv', stringsAsFactors = F)
#   
#   participants <- unique(trajectories$participant)
#   
#   participant <- c()
#   trial       <- c()
#   pass        <- c()
#   pause       <- c()
#   predir      <- c()
#   postdir     <- c()
#   
#   for (ppno in participants) {
#     
#     ppdf <- trajectories[which(trajectories$participant == ppno),]
#     
#     trials <- unique(ppdf$trial)
#     
#     for (trialn in trials) {
#       
#       trialdf <- ppdf[which(ppdf$trial == trialn),]
#       
#       handX <- trialdf$handX
#       handY <- trialdf$handY
#       
#       cross.idx <- which(handX == 0 & handY == 0)[1]
#       
#       pre.dir  <- atan2(diff(handY[c(1,cross.idx-1)]), diff(handX[c(1,cross.idx-1)]))
#       post.dir <- atan2(diff(handY[c(cross.idx+1,length(handY))]), diff(handX[c(cross.idx+1,length(handX))]))
#       
#       participant <- c(participant, ppno)
#       trial       <- c(trial,       trialn)
#       pass        <- c(pass,        trialdf$pass[1])
#       pause       <- c(pause,       trialdf$pause[1])
#       predir      <- c(predir,      pre.dir)
#       postdir     <- c(postdir,     post.dir)
#       
#     }
#     
#   }
#   
#   write.csv( data.frame(participant,
#                         trial,
#                         pass,
#                         pause,
#                         predir,
#                         postdir),
#              file='data/reset_pause/avg_reach_dir.csv',
#              quote=F,
#              row.names=F)
#   
# }



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
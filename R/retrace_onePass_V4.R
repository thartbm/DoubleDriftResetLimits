library('svglite')
library('ez')
library('segmented')

source('R/common.R')
#source('R/models.R')

# Data handling -----

oldPreProcessOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), overwrite=FALSE) {
  
  if (file.exists('data/onepass_V4/onePass_V4_re-trace.csv') & !overwrite) {
    return()
  }
  
  # angle_deg <- seq(5,50,0.5)
  # angle_rad <- (angle_deg / 180) * pi
  # cos.a <- cos(angle_rad)
  # sin.a <- sin(angle_rad)
  # #slope <- sin.a / cos.a
  # slope <- cos.a / sin.a
  # 
  # modeldf <- data.frame(angle_deg, angle_rad, cos.a, sin.a, slope)
  # 
  # Lx_MSE <- c()
  # Ly_MSE <- c()
  # LxLy_MSE <- c()
  
  internalMovements <- c(2,3,4)
  externalMovements <- rev(c(.125, .167))
  
  #for (task in c('arrow','re-trace')) {
  for (task in c('re-trace')) {
    
    # we will generate data files for both tasks, with these columns:
    # except that there is no bound in the arrow task
    participant <- c()
    trial <- c()
    internalspeed <- c()
    internaldirection <- c()
    externalspeed <- c()
    fixationside <- c()
    initialdirection <- c()
    illusionstrength <- c()
    boundX <-c()
    boundY <-c()
    boundXraw <- c()
    boundYraw <- c()
    
    # ***************************
    # 
    
    #pdf(file='doc/individual_participants_trials.pdf',width=12,height=8)
    
    #layout(matrix(c(1,2,3,4,5,6),byrow=TRUE,ncol=3,nrow=2))
    
    for (participant.idx in c(1:length(participants))) {
      
      # plot(-1000,-1000,
      #      main=sprintf('participant %d',participants[participant.idx]),xlab='',ylab='',
      #      xlim=c(-4.5,10),ylim=c(-.5,14),
      #      bty='n',ax=F)
      
      # lines(x=c(0,0),y=c(0,13.5),col='blue')
      
      ppno <- participants[participant.idx]
      
      df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',ppno))
      
      tasktrials <- unique(df$trial[df$taskname == task])
      
      taskdf <- df[which(df$trial_no %in% tasktrials),]
      
      for (EM.idx in c(1:length(externalMovements))) {
        
        for (IM.idx in c(1:length(internalMovements))) {
          
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
              
              # we should never be here
              
              percept <- trialdf$percept[1]
              
              if (trialdf$internalMovement[1] < 0) percept <- 90 - (percept - 90)
              
              x <- c(0,cos((percept/180)*pi)) * 0.6
              y <- c(0,sin((percept/180)*pi)) * 0.6
              
              initialdirection <- c(initialdirection, 90 - percept)
              illusionstrength <- c(illusionstrength, 90 - percept)
              boundX <- c(boundX, NA)
              boundY <- c(boundY, NA)
              boundXraw <- c(boundXraw, NA)
              boundYraw <- c(boundYraw, NA)
              
            } else {
              
              # these are the relevant bits of code:
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              step.idx <- which(trialdf$step == step)
              
              x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0)
              y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5)
              x <- x - x[1]
              y <- y - y[1]
              oldprop <- max(y) # this puts the Y-coordinate at the end of 4s or 3s
              prop <- sqrt(x[length(x)]^2 + y[length(y)]^2) # now we got it as d to end of trajectory
              #print(c(oldprop,prop))
              x <- x / prop
              y <- y / prop
              t <- trialdf$time_ms[step.idx]
              
              # flip for internal motion direction
              if (trialdf$internalMovement[1] < 0) x <- -x
              
              lines((x-x[1])*13.5,(y-y[1])*13.5,col=rgb(127,127,127,44, maxColorValue = 255))
              
              # this has to be redone once the reset point is found!
              point <- which(sqrt(x^2 + y^2) > 0.15)[1]
              percept <- (atan2(y[point], x[point]) / pi) * 180
              initialdirection <- c(initialdirection, 90 - percept)
              
              # why do this and also the smoothed spline?
              # to test if we should be doing the splines at all?
              # and the splines find the "more correct" point... hmmm...
              boundary <- which(diff(x) < 0)[1]
              
              if (is.na(boundary)) {
                
                boundX <- c(boundX, NA)
                boundY <- c(boundY, NA)
                boundXraw <- c(boundXraw, NA)
                boundYraw <- c(boundYraw, NA)
                illusionstrength <- c(illusionstrength, NA)
                
              } else {
                
                smspl <- smooth.spline(t, x, spar=.25)
                x_p <- predict(smspl$fit, t)$y
                
                # these are sign changes from positive to negative
                localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
                # distance of local max has to be more tha (0.1) but the trajectory is scaled to 0-0.6 (??? why)
                lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
                localmaxima <- localmaxima[which(lmd > 0.1)]
                # the maxima can also not be very close to the end of the trajectory:
                lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
                localmaxima <- localmaxima[which(lmd > 0.01)]
                # do we have any left?
                if (length(localmaxima) > 0) {
                  
                  # store the first local maximum as reset point:
                  
                  # points(x[localmaxima[1]]*13.5,y[localmaxima[1]]*13.5,col='purple')
                  
                  boundX <- c(boundX, x[localmaxima[1]])
                  boundY <- c(boundY, y[localmaxima[1]])
                  boundXraw <- c(boundXraw, prop * x[localmaxima[1]])
                  boundYraw <- c(boundYraw, prop * y[localmaxima[1]])
                  
                  # **********************************
                  # ACTUAL HALFWAY POINTS HERE:
                  #print(sqrt(sum(IDcoords^2)))
                  hwd <- sqrt(sum(c(x[localmaxima[1]],y[localmaxima[1]])^2)) / 2
                  hwp <- which(sqrt(x^2 + y^2) > hwd)[1]
                  hwpercept <- (atan2(y[hwp], x[hwp]) / pi) * 180
                  illusionstrength <- c(illusionstrength, 90 - hwpercept)
                  
                  #print(percept - hwpercept)
                  boundary <- localmaxima[1]
                  
                } else {
                  boundX <- c(boundX, NA)
                  boundY <- c(boundY, NA)
                  boundXraw <- c(boundXraw, NA)
                  boundYraw <- c(boundYraw, NA)
                  illusionstrength <- c(illusionstrength, NA)
                }
                
              }
              
            } 
            
          }
          
          # done all trials, can now draw distributions of end points
          
          # if (task %in% c('track', 're-trace')) {
          #   
          #   print(Xbounds)
          #   
          #   Xavg <- mean(Xbounds, na.rm=TRUE)
          #   Xstd <- sd(Xbounds, na.rm=TRUE)
          #   Yavg <- mean(Ybounds, na.rm=TRUE)
          #   Ystd <- sd(Ybounds, na.rm=TRUE)
          #   
          #   print(c(Xavg,Xstd,Yavg,Ystd))
          #   
          #   XdistrX <- seq(-0.1, 0.6,  .01)
          #   XdistrY <- dnorm(XdistrX,mean=Xavg,sd=Xstd) / dnorm(c(Xavg),mean=Xavg,sd=Xstd)
          #   XdistrY <- XdistrY / 10
          #   
          #   YdistrY <- seq(0.0, 0.70, .01)
          #   YdistrX <- dnorm(YdistrY,mean=Yavg,sd=Ystd) / dnorm(c(Yavg),mean=Yavg,sd=Ystd)
          #   YdistrX <- YdistrX / 10
          #   
          #   lines(XdistrX+(participant.idx*2)-2+EM.idx,XdistrY+IM.idx-1.1,col='#0fd2e2ff')
          #   lines(YdistrX+(participant.idx*2)-2.2+EM.idx,YdistrY+IM.idx-0.9,col='#0fd2e2ff')
          #   
          #   
          #   # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC')
          #   
          # }
          
        }
        
      }
      
    }  
    #   ppno <- participants[participant.idx]
    #   ppdat.idx <- which(participant == ppno & !is.na(illusionstrength))
    #   ppdat <- data.frame('trial.idx'=ppdat.idx,
    #                       'initial.dir'=initialdirection[ppdat.idx],
    #                       'illusion.strength'=illusionstrength[ppdat.idx],
    #                       'X'=boundX[ppdat.idx],
    #                       'Y'=boundY[ppdat.idx],
    #                       'Xraw'=boundXraw[ppdat.idx],
    #                       'Yraw'=boundYraw[ppdat.idx],
    #                       'speed'=round(0.5/externalspeed[ppdat.idx]),
    #                       'sin.a'=sin( (illusionstrength[ppdat.idx]/180)*pi ),
    #                       'cos.a'=cos( (illusionstrength[ppdat.idx]/180)*pi ) )
    #   
    #   #print(  summary(binned.trials$illusion.strength)  )
    #   
    #   ppdat <- ppdat[which(!is.na(ppdat$X)),]
    #   ppdat <- ppdat[which(ppdat$illusion.strength > 5),]
    #   #ppdat$slope <- ppdat$sin.a / ppdat$cos.a
    #   ppdat$slope <- ppdat$cos.a / ppdat$sin.a
    #   ppdat <- ppdat[which(is.finite(ppdat$slope) & ppdat$slope > 0),]
    #   
    #   #print(ppdat)
    #   
    #   sortedIS <- sort(ppdat$illusion.strength)
    #   #print(sortedIS)
    #   bins <- seq(5, sortedIS[length(sortedIS)-6], length.out=11)
    #   binned.trials <- ppdat
    #   binned.trials$bin <- NA
    #   for (binno in c(1:(length(bins)-1))) {
    #     
    #     bin.trial.idx <- which(binned.trials$illusion.strength > bins[binno] & binned.trials$illusion.strength < bins[binno+1])
    #     
    #     binned.trials$bin[bin.trial.idx] <- binno
    #     
    #   }
    #   
    #   #print(length(which(is.na(binned.trials$bin))))
    #   
    #   binned.trials <- binned.trials[which(!is.na(binned.trials$bin)),]
    #   # print(binned.trials)
    #   
    #   binned.trials <- aggregate(cbind(X, Y, Xraw, Yraw) ~ bin, data=binned.trials, FUN=mean)
    #   #print(binned.trials)
    #   
    #   # ****************
    #   # plot raw & binned average reset points
    #   
    #   plot(-1000,-1000,
    #        main=sprintf('participant %d (raw)',participants[participant.idx]),xlab='',ylab='',
    #        xlim=c(-4.5,10),ylim=c(-.5,14),
    #        bty='n',ax=F)
    #   
    #   lines(x=c(0,0),y=c(0,13.5),col='blue')
    #   
    #   
    #   points(binned.trials$Xraw*13.5, binned.trials$Yraw*13.5, col='purple')
    #   
    #   # **************************
    #   # plot normalized & binned average reset points
    #   
    #   plot(-1000,-1000,
    #        main=sprintf('participant %d (normalized)',participants[participant.idx]),xlab='',ylab='',
    #        xlim=c(-4.5,10),ylim=c(-.5,14),
    #        bty='n',ax=F)
    #   
    #   lines(x=c(0,0),y=c(0,13.5),col='blue')
    #   
    #   points(binned.trials$X*13.5, binned.trials$Y*13.5, col='purple')
    #   
    #   # ****************************
    #   # plot RT over A
    #   
    #   ppdat$RT <- sqrt((ppdat$X*13.5)^2 + (ppdat$Y*13.5)^2) / ppdat$speed
    #   
    #   plot(-1000,-1000,
    #        main=sprintf('single limit models (participant %d)',participants[participant.idx]),
    #        xlab='',ylab='',
    #        xlim=c(5,50),ylim=c(0,14),
    #        bty='n',ax=F)
    #   
    #   points(ppdat$illusion.strength, ppdat$Y*13.5, col='blue')
    #   points(ppdat$illusion.strength, ppdat$X*13.5, col='red')
    #   
    #   title(xlab='illusion strength')
    #   title(ylab='coordinates [cm]')
    #   
    #   # ********************
    #   # DO MODELS!
    #   
    #   ppdat$X <- ppdat$X * 13.5
    #   ppdat$Y <- ppdat$Y * 13.5
    #   
    #   # print(dim(ppdat)) SIX MORE TRIALS.... ?
    #   
    #   singleFits <- fitSingleLimitModels(df=ppdat)
    #   doubleFit <- fitTwoLimitModel(df=ppdat)
    #   #print(singleFits)
    #   
    #   XlimFitted <- XlimResets(par=singleFits$Xlim$par, data=modeldf)
    #   A <- as.numeric(modeldf$angle_deg)
    #   RT <- as.numeric(XlimFitted$Y)
    #   
    #   lines(A, RT, col='blue')
    #   
    #   speeds <- 13.5/c(4, 3)
    #   
    #   for (speedno in c(1:length(speeds))) {
    #     
    #     speeddf <- modeldf
    #     speeddf$speed <- speeds[speedno]
    #     
    #     YlimFitted <- YlimResets(par=singleFits$Ylim$par, data=speeddf)
    #     X <- as.numeric(YlimFitted$X)
    #     # print(YlimFitted)
    #     # print(c(length(A),length(X)))
    #     
    #     
    #     lines(A, X, col='red', lty=speedno)
    #     
    #   }      
    #   
    #   Lx_MSE <- c(Lx_MSE, singleFits$Xlim$MSE)
    #   Ly_MSE <- c(Ly_MSE, singleFits$Ylim$MSE)
    #   LxLy_MSE <- c(LxLy_MSE, doubleFit$MSE)
    #   
    #   
    #   MSEs <- c(singleFits$Xlim$MSE, singleFits$Ylim$MSE)
    #   AICs <- AIC(MSE=MSEs, k=c(1,1), N=1)
    #   rLL <- relativeLikelihood(AICs)
    #   
    #   labels <- c(sprintf('Y << Lx (MSE:%0.1f, AIC: %0.1f, rLL: %0.3f)', singleFits$Xlim$MSE['Lx'], AICs['Lx'], rLL['Lx']),
    #               sprintf('X << Ly (MSE:%0.1f, AIC: %0.1f, rLL: %0.3f)', singleFits$Ylim$MSE['Ly'], AICs['Ly'], rLL['Ly']))
    #   
    #   legend(10,14,legend = labels, col=c('blue','red'), box.col='white', box.lwd=0, pch=1, lty=1, bg=rgb(1,1,1,0.75))
    #   
    #   
    #   axis(side=1,at=seq(5,50,15))
    #   axis(side=2,at=seq(0,13.5,length.out = 4))
    #   
    #   # *******************
    #   # DOUBLE LIMIT MODEL?
    #   
    #   
    #   plot(-1000,-1000,
    #        main=sprintf('two-limit model (participant %d)',participants[participant.idx]),
    #        xlab='',ylab='',
    #        xlim=c(5,50),ylim=c(0,14),
    #        bty='n',ax=F)
    #   
    #   points(ppdat$illusion.strength, ppdat$Y, col='blue')
    #   points(ppdat$illusion.strength, ppdat$X, col='red')
    #   
    #   title(xlab='illusion strength')
    #   title(ylab='coordinates [cm]')
    #   
    #   for (speedno in c(1:length(speeds))) {
    #     
    #     speeddf <- modeldf
    #     speeddf$speed <- speeds[speedno]
    #     
    #     XYfit <- twoLimResets(par=doubleFit$par, data=speeddf)
    #     
    #     X <- as.numeric(XYfit$X)
    #     Y <- as.numeric(XYfit$Y)
    # 
    #     lines(A, Y, col='blue', lty=speedno)
    #     lines(A, X, col='red', lty=speedno)
    #     
    #   }     
    #   
    #   MSEs <- c(MSEs, 'Lx.Ly'=doubleFit$MSE)
    #   AICs <- AIC(MSE=MSEs, k=c(1,1,2), N=1)
    #   #print(AICs)
    #   rLL <- relativeLikelihood(AICs)
    #   
    #   
    #   legend(10,14,legend=c(sprintf('Lx.Ly (MSE: %0.1f, AIC: %0.1f, rLL: %0.3f)', doubleFit$MSE, AICs[3], rLL[3])),
    #          box.col='white', box.lwd=0, bg=rgb(1,1,1,0.75))
    #   
    #   axis(side=1,at=seq(5,50,15))
    #   axis(side=2,at=seq(0,13.5,length.out = 4))
    #   
    #   # *************************
    #   # Reset time distribution
    #   
    #   plot(-1000,-1000,
    #        main=sprintf('reset time distribution (participant %d)',participants[participant.idx]),
    #        xlab='',ylab='',
    #        xlim=c(0,5),ylim=c(-0.5,1.5),
    #        bty='n',ax=F)
    #   
    #   title(xlab='reset time [s]', line=2.5)
    #   title(ylab='relative denisty', line=2.5)
    #   
    #   ResetTimes <- sqrt(ppdat$X^2 + ppdat$Y^2) / ppdat$speed
    #   
    #   points(ResetTimes, rep(-0.25, length(ResetTimes)))
    #   
    #   distr <- density(ResetTimes, n=501 , from=0, to=5)
    #   lines(seq(0, 5, length.out = 501), distr$y)
    #   
    #   axis(side=1, at=c(0:4))
    #   axis(side=2, at=c(0,1))
    #   
    #   
    # }
    # 
    # dev.off()
    
    # write out csv file:
    
    # participant <- c()
    # internalspeed <- c()
    # externalspeed <- c()
    # fixationside <- c()
    # initialdirection <- c()
    # boundX <-c()
    # boundY <-c()
    
    df <- data.frame(participant, trial, internalspeed, internaldirection, externalspeed, fixationside, initialdirection, illusionstrength, boundX, boundY)
    write.csv(df, file=sprintf('data/onePass_V4/onePass_V4_%s.csv', task), quote=F, row.names=F)
    
  }
  
  # Lx_MSE <- unname(Lx_MSE)
  # Ly_MSE <- unname(Ly_MSE)
  # LxLy_MSE <- unname(LxLy_MSE)
  # 
  # Lx_MSE <- mean(Lx_MSE)
  # Ly_MSE <- mean(Ly_MSE)
  # LxLy_MSE <- mean(LxLy_MSE)
  # MSEs <- c('Lx'=Lx_MSE, 'Ly'=Ly_MSE, 'Lx.Ly'=LxLy_MSE)
  # print(MSEs)
  # AICs <- AIC(MSE=MSEs, k=c(9,9,18), N=9)
  # print(AICs)
  # rLL <- relativeLikelihood(AICs)
  # print(rLL)
  
  
}


preProcessOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), 
                                 overwrite = TRUE,
                                 RPFUN=get97.5percMaxX1_resetpoint,
                                 IDFUN=getDirectionHalfway) {
  
  

  if (file.exists('data/onepass_V4/onePass_V4_re-trace.csv') & !overwrite) {
    return()
  } else {
    cat('extracting reset points from trajectories...\n')
  }

  internalMovements <- c(2,3,4)
  externalMovements <- rev(c(.125, .167))
  
  task <- 're-trace'
  
  participant <- c()
  trial <- c()
  internalspeed <- c()
  internaldirection <- c()
  externalspeed <- c()
  fixationside <- c()
  initialdirection <- c()
  resetX <-c()
  resetY <-c()
  
  # loop throug the participants (they have separate data files)
  for (participant.idx in c(1:length(participants))) {
    
    # get the participant number
    ppno <- participants[participant.idx]
    
    # load participants data file:
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',ppno))
    
    # get the numbers of trials that are in the re-trace condition
    trials <- unique(df$trial[df$taskname == task])
    
    # leave only the re-trace trials in memory:
    taskdf <- df[which(df$trial_no %in% trials),]
    
    # loop through the trials:
    for (trialno in trials) {
      
      # get a data frame for only the current trial:
      trialdf <- taskdf[taskdf$trial_no == trialno,]
      
      # add some straightforward variables to the output data frame:
      participant <- c(participant, ppno)
      trial <- c(trial, trialno)
      internalspeed <- c(internalspeed, trialdf$internalMovement[1])
      internaldirection <- c(internaldirection, ifelse(trialdf$internalMovement[1] > 0, 1, -1))
      externalspeed <- c(externalspeed, trialdf$externalMovement[1])
      fixationside <- c(fixationside, trialdf$fixationside[1])
      
      # only step 99 was retracing (the preceding ones are for stimulus presentation)
      step <- 99
      step.idx <- which(trialdf$step == step)
      
      # get the relevant samples X and Y coordinates and scale to centimeters:
      x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 13.5
      y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 13.5
      
      # start at origin:
      x <- x - x[1]
      y <- y - y[1]
      
      # get time as well (for filters, ended up not using it):
      t <- trialdf$time_ms[step.idx]
      
      # flip to normalize internal motion direction
      if (trialdf$internalMovement[1] < 0) x <- -x
      
      # GET THE RESET POINT
      # which function is used to get the reset point is a function argument:
      resetPoint <- RPFUN(x,y,t,verbosity=verbosity) 
      
      # store the reset point in the output data vectors:
      resetX <-c(resetX, resetPoint$X)
      resetY <-c(resetY, resetPoint$Y)
      
      
      # GET THE INITIAL DIRECTION
      # the function is also an argument to this function:
      ID <- IDFUN(x, y, resetPoint)
      
      # store in output:
      initialdirection <- c(initialdirection, ID)
      
    }
    
  }
  
  # illusion strength depends on initial direction:
  illusionstrength <- 90 - ((initialdirection/pi)*180)
  
  # put output vectors in data frame:
  df <- data.frame(participant, 
                   trial, 
                   internalspeed, 
                   internaldirection, 
                   externalspeed, 
                   fixationside, 
                   resetX, 
                   resetY,
                   initialdirection, 
                   illusionstrength)
  
  # write data frame to disk:
  write.csv(df, file=sprintf('data/onePass_V4/onePass_V4_%s.csv', task), quote=F, row.names=F)
  
}

get97.5percMaxX1_resetpoint <- function(x,y,t,verbosity=0) {
  
  # x <- pdf$x
  # y <- pdf$y
  
  # remove samples after maximum Y (no backtracking)
  max_y_idx <- which.max(y)
  x <- x[c(1:max_y_idx)]
  y <- y[c(1:max_y_idx)]
  
  # remove samples with Y below 0 (have to go forward-ish)
  lo_y_idx <- which(y < 0)
  if (length(lo_y_idx) > 0) {
    ok_y_idx <- c(lo_y_idx[length(lo_y_idx)]:length(y))
    x <- x[ok_y_idx]
    y <- y[ok_y_idx]
  }
  
  
  # for people with a lot of samples, downsample severely
  if (length(x) > 75) {
    trajectory <- resampleDistance(x=x,y=y,distance=0.9)
    x <- trajectory$x
    y <- trajectory$y
  }
  
  
  # smooth the remaining signal
  sx <- ksmooth(x = c(1:length(x)),
                y = x,
                kernel = 'normal',
                bandwidth = 5,
                x.points = c(1:length(x)))$y
  sy <- ksmooth(x = c(1:length(x)),
                y = y,
                kernel = 'normal',
                bandwidth = 5,
                x.points = c(1:length(y)))$y
  
  # remove duplicate samples, if any are left:
  nondupes <- which(sx[-length(sx)] != sx[-1])
  sx <- sx[nondupes]
  sy <- sy[nondupes]
  
  # remove all points closer than 0.4 cm to the origin:
  d_idx <- which((sx^2 + sy^2)> 0.4)
  
  sx <- sx[d_idx]
  sy <- sy[d_idx]
  
  # # remove all points beyond the maximum y coordinate:
  # my_idx <- which.max(sy)
  # if (length(my_idx) > 0) {
  #   sx <- sx[c(1:my_idx)]
  #   sy <- sy[c(1:my_idx)]
  # }
  # (this was done earlier)
  
  
  # lines(sx,sy,col='blue')
  # #rp_idx <- which.max(pdf$sx) # returns the overall maximum
  
  local_maxima <- which( diff(sign(diff(sx))) == -2 )+1
  #print(local_maxima)
  #print(length(local_maxima))
  
  RPx <- NA
  RPy <- NA
  if (length(local_maxima) > 0) {
    #print(local_maxima)
    RPidx <- which( sx > ( 0.975 * sx[local_maxima[1]] ) )[1] - 1
    if (RPidx > 0) {
      RPx <- sx[RPidx]
      RPy <- sy[RPidx]
    }
    #print(c(RPx,RPy))
    # points(RPx,RPy,col='green',cex=3)
    
  }
  
  resetPoint = list('X'=RPx,
                    'Y'=RPy)
  # if (verbosity) {
  #   print(resetPoint)
  #   print(sx)
  #   print(c(length(sx),RPidx))
  # }
  
  return(resetPoint)
  
}



getSplineReversalResetPoint <- function(x,y,t) {
  
  # we smooth & interpolate the x coordinates:
  smspl <- smooth.spline(t, x, spar=.25)
  x_p <- predict(smspl$fit, t)$y
  
  # these are sign changes from positive to negative
  localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
  # distance of local max has to be more tha (0.1) but the trajectory is scaled to 0-0.6 (??? why)
  lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
  localmaxima <- localmaxima[which(lmd >= 0.4)]
  # the maxima can also not be very close to the end of the trajectory:
  lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
  localmaxima <- localmaxima[which(lmd >= 0.4)]
  # do we have any left?
  if (length(localmaxima) > 0) {
    
    # store the first local maximum as reset point:
    X <- x[localmaxima[1]]
    Y <- y[localmaxima[1]]
    
    resetPoint = list('X'=X,
                      'Y'=Y)
    
  } else {
    
    resetPoint = list('X'=NA,
                      'Y'=NA)
    
  }
  
  return(resetPoint)
  
}

getMultilinearResetPoint <- function(x,y,t) {
  
  # THIS FUNCTION WAS NOT FINISHED AS IT WAS GETTING BAROQUE
  
  # make data frame and switch coordinates:
  df <- data.frame('x'=y,
                   'y'=x,
                   't'=t)  # don't really need t, right?
  
  # starting point for segmented linear regression:
  start.lm <- lm(y ~ x, data=df)
  
  predict.lm( object = start.lm )
  
  breakpoints <- 1
  
  seg.lm <- segmented::segmented( start.lm, 
                                  seg.Z   = ~x, 
                                  npsi    = breakpoints, # 1 break point
                                  control = seg.control( display = FALSE ) )
  
  predict.segmented()
  
  # this test indicates that (when p<.05) there MAY be an additional breakpoint
  # davies.test(start.lm)
  
  # here put a while-loop:
  # add a segment as long as that explains the data better
  # probably use AIC -> rel log likelihood, as criterion for "better"
  
}


get2ndDerInflectResetPoint <- function(x,y,t) {
  
  # THIS SEEMS MORE PRINCIPLED BUT REQUIRES A LOT OF SMOOTHING
  # MIGHT WORK WITH THE PREPROCESSING FROM OTHER FUNCTIONS
  # RIGHT NOW IT IS UNFINISHED!
  
  # smooth with a median filter:
  xx <- as.numeric( runmed(x=x, k=7) )
  yy <- as.numeric( runmed(x=y, k=7) )
  
  # remove consecutive duplicates:
  idx <- which(xx[-length(xx)] != xx[-1] & yy[-length(yy)] != yy[-1])
  xxx <- xx[idx]
  yyy <- yy[idx]
  
  # dx	dy	dx/dy	d(dx/dy)	d(dx/dy)/dy
  
  der2 <- diff( diff(xxx) / diff(yyy) ) / diff(yyy)[-1]
  
  rp_idx <- which(diff(sign(diff( der2 ))) == -2) + 2
  rp_idx <- rp_idx[which(yyy[rp_idx] > 0)][1]
  
  resetPoint <- c('x'=xxx[rp_idx],
                  'y'=yyy[rp_idx])
  
  return(resetPoint)
  
  #plot(x,y,asp=1,type='l')
  #points(xx,yy,col='blue')
  #points(xxx,yyy,col='red')
  
  
}


# here are two functions that give a measure of initial direction
# each of which can serve as an estimate of illusion strength
# (the first actually calls the second)

getDirectionHalfway <- function(x,y, resetPoint, fraction=0.5) {
  
  # halfway point:
  cutoff <- fraction * sqrt(resetPoint$X^2 + resetPoint$Y^2)
  
  # use this as a cutoff:
  return(getDirectionAtCutoff(x=x,
                              y=y,
                              cutoff=cutoff))
  
}



getDirectionAtCutoff <- function(x,y, cutoff) {
  
  # get the distance of each trajectory sample from the origin
  d   <- sqrt(x^2 + y^2)
  # get the first sample beyond (or on) the cutoff
  idx <- which(d >= cutoff)[1]
  
  #create a resetpoint list with X and Y properties:
  resetPoint <- list('X'=x[idx],
                     'Y'=y[idx])
  
  # what is the direction at this point:
  direction <- atan2(y[idx], x[idx])
  
  # return direction:
  return(direction)
  
}



summarizeTraceBoundsV4 <- function() {
  
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)
  
  df$internalspeed <- abs(df$internalspeed)
  
  df2 <- read.csv('data/onePass_V4/onePass_V4_arrow.csv', stringsAsFactors = F)
  
  # participants <- unique(df$participant)
  # 
  # internalspeeds <- unique(df$internalspeed)
  # externalspeeds <- unique(df$externalspeed)
  
  aggdf <- aggregate(resetX ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  names(aggdf)[which(names(aggdf) == 'resetX')] <- 'resetX_mean'
  
  tempdf <- aggregate(resetX ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['resetX_sd'] <- tempdf$resetX
  
  tempdf <- aggregate(resetY ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  aggdf['resetY_mean'] <- tempdf$resetY
  
  tempdf <- aggregate(resetY ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['resetY_sd'] <- tempdf$resetY
  
  # tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  # aggdf['initialdirection_mean'] <- tempdf$initialdirection
  # 
  # tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  # aggdf['initialdirection_sd'] <- tempdf$initialdirection
  tempdf <- aggregate(illusionstrength ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  aggdf['initialdirection_mean'] <- tempdf$illusionstrength
  
  tempdf <- aggregate(illusionstrength ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['initialdirection_sd'] <- tempdf$illusionstrength
  
  # tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=mean, na.rm=TRUE)
  # aggdf['arrowdirection_mean'] <- tempdf2$initialdirection
  # 
  # tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=sd, na.rm=TRUE)
  # aggdf['arrowdirection_sd'] <- tempdf2$initialdirection
  
  return(aggdf)
  
}


getCVRdata <- function() {
  
  
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
    
    ppdf <- read.csv(sprintf('data/CVRdemo/CVRdemo_OPV4_resets_p%04d.csv', ppno), stringsAsFactors=F)
    
    trialnos <- unique(ppdf$trial_no)
    
    for (trialno in trialnos) {
      
      trialdf <- ppdf[which(ppdf$trial_no == trialno & ppdf$step == 99),]
      
      X <- trialdf$handx_pix / 38.814814815 # is this pixels per centimeter? yes
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
        angle <- 90 - ((atan2(Y[indirpoint], X[indirpoint]) / pi) * 180)
        initialdirection <- c(initialdirection, angle)
        resetX <- c(resetX, resetpoint[1])
        resetY <- c(resetY, resetpoint[2])
        

      } else {
        
        initialdirection <- c(initialdirection, NA)
        resetX <-c(resetX, NA)
        resetY <-c(resetY, NA)
        
      }

    }

  }
  
  df <- data.frame(participant,trial,internalspeed,internaldirection,fixationside,initialdirection,resetX,resetY)
  
  df$X     <- df$resetX # cm
  df$Y     <- df$resetY # cm
  df$speed <- 13.5/4 # external speed in cm/s
  df$RT    <- sqrt(df$X^2 + df$Y^2) / df$speed
  
  df$angle <- ((90-df$initialdirection)/180) * pi
  df$sin.a <- sin(df$angle)
  df$cos.a <- cos(df$angle)
  df$slope <- df$sin.a / df$cos.a
    
  df$Vi    <- abs(df$internalspeed)
  df$Ve    <- df$speed
  
  df <- df[,c('participant','X', 'Y', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve')]
  
  df <- df[which(!is.na(df$X)),]
  
  return(df)
  
}



# Figures -----

plotResetPoints <- function(target='inline') {
  
  source('R/models.R')
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_trajectories_resetpoints.pdf',onefile=TRUE,width=7.5,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/Fig4_trajectories_resetpoints.svg',width=7.5,height=4)
  }
  
  par(mar=c(3.5, 3.5, 2.5, 0.5))
  
  colors <- getColors()
  
  # there will be three plots:
  # 1: example participant
  # 2: 2D overview of average reset points
  # 3: Cavangh & Tse comparison
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T))
  
  # # # # # # # # # # # # # # # # # # 
  # 
  # Reset point detection / initial direction
  # 
  # # # # # # # # # # # # # # # # # # 
  
  plot(-1000,-1000,
       main='reset detection',xlab='',ylab='',
       xlim=c(-1.5,7.5)/13.5, ylim=c(-1,13.5)/13.5,
       bty='n',ax=F, asp=1)
  
  title(xlab='cm', line=2.5)
  title(ylab='cm', line=2.5)
  
  df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',1))
  tasktrials <- unique(df$trial[df$taskname == 're-trace'])
  taskdf <- df[which(df$trial_no %in% tasktrials),]
  
  #str(taskdf)
  trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == 0.125 &
                                           abs(taskdf$internalMovement) == 3)])
  
  # reasonable trials:
  # 175
  # 101, 155, 175, 245
  
  trialdf <- taskdf[taskdf$trial_no == 175,]
  
  step.idx <- which(trialdf$step == 99) # only for re-trace
  
  x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
  y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
  t <- trialdf$time_ms[step.idx]
  
  if (trialdf$internalMovement[1] < 0) x <- -x
  
  # point <- which(sqrt(x^2 + y^2) > 0.15)[1] # WHERE IS THIS POINT?
  # percept <- (atan2(y[point], x[point]) / pi) * 180
  #initialdirection <- c(initialdirection, 90 - percept)
  
  # why do this and also the smoothed spline?
  # this only says that at some point X is going back to the midline
  # but it's not a good estimate of where that happensreasonable trials:
  # 175
  # 101, 155, 175, 245
  
  trialdf <- taskdf[taskdf$trial_no == 175,]
  
  step.idx <- which(trialdf$step == 99) # only for re-trace
  
  x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
  y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
  t <- trialdf$time_ms[step.idx]
  
  if (trialdf$internalMovement[1] < 0) x <- -x
  
  # point <- which(sqrt(x^2 + y^2) > 0.15)[1] # WHERE IS THIS POINT?
  # percept <- (atan2(y[point], x[point]) / pi) * 180
  #initialdirection <- c(initialdirection, 90 - percept)
  
  # why do this and also the smoothed spline?
  # this only says that at some point X is going back to the midline
  # but it's not a good estimate of where that happens
  boundary <- which(diff(x) < 0)[1]
  
  if (is.na(boundary)) {
    # no reset point detected!
    # lines(x,y,col='#66666699')
    lines(x,y,col=colors[['lightblue']]$s)
  } else {
    
    smspl <- smooth.spline(t, x, spar=.25)
    x_p <- predict(smspl$fit, t)$y
    
    localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
    
    lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
    localmaxima <- localmaxima[which(lmd > 0.1)]
    
    lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
    localmaxima <- localmaxima[which(lmd > 0.01)]
    
    if (length(localmaxima) > 0) {
      
      boundX <- x[localmaxima[1]]
      boundY <- y[localmaxima[1]]
      

      boundary <- localmaxima[1]
      # lines(x[1:boundary],y[1:boundary],col='#b400e4ff')
      lines(x[1:boundary],y[1:boundary],col=colors[['orange']]$s,lw=3)
      lines(x[boundary:length(x)],y[boundary:length(x)],col='#CCCCCCFF',lw=3)
      points(boundX,boundY,col=colors[['orange']]$s,cex=2,pch=16)
      
      # illustrate the X,Y coordinates of the reset point?
      lines(rep(boundX,2),c(0,boundY),col=colors[['lightblue']]$s, lw=2, lty=2)
      lines(c(0,boundX),rep(boundY,2),col=colors[['lightblue']]$s, lw=2, lty=2)
      text(boundX+0.05,boundY+0.05,sprintf('reset point:\n(%0.1f, %0.1f)',boundX*13.5,boundY*13.5),adj=c(0,0))
      
      lines(c(0,boundX),c(0,boundY),col=colors[['yorkred']]$s, lw=2, lty=2)
      hd <- (sqrt(boundX^2 + boundY^2))/2
      idx <- which(sqrt(x^2 + y^2) > hd)[1]
      hd <- sqrt(x[idx]^2 + y[idx]^2)
      points(x[idx],y[idx],col=colors[['yorkred']]$s, pch=16, cex=2)
      
      ird <- atan2(y[idx],x[idx])
      arcX <- cos(seq(ird, pi/2, length.out=30)) * hd
      arcY <- sin(seq(ird, pi/2, length.out=30)) * hd
      lines(arcX,arcY,col=colors[['yorkred']]$s, lw=2, lty=1)
      lines(c(0,0,x[idx]*1.7),c(arcY[30]*1.7,0,y[idx]*1.7),col=colors[['yorkred']]$s, lw=2, lty=1)
      
      text(1/13.5,4.2/13.5,'a',col=colors[['yorkred']]$s,cex=2)

    }
    
  }
  
  
  
  
  axis(side=1,at=c(0,4)/13.5,labels=c('0','4'))
  axis(side=2,at=c(0,4.5,9,13.5)/13.5,labels=c('0.0','4.5','9.0','13.5'))
  
  
  # # # # # # # # # # # # # # # # # # 
  # 
  # EXAMPLE PARTICIPANT PLOT:
  #
  # # # # # # # # # # # # # # # # # # 
    
  plot(-1000,-1000,
       main='participant 4 (3 cps, 4 s)',xlab='',ylab='',
       xlim=c(-3,6)/13.5, ylim=c(-1,13.5)/13.5,
       bty='n',ax=F, asp=1)
  
  # xlim=c(-1,8), ylim=c(-1,13.5),
  
  title(xlab='cm', line=2.5)
  title(ylab='cm', line=2.5)
  
  df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',4))
  tasktrials <- unique(df$trial[df$taskname == 're-trace'])
  taskdf <- df[which(df$trial_no %in% tasktrials),]
  
  trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == 0.125 &
                                           abs(taskdf$internalMovement) == 3)])
  
  # loop through those trials:
  for (trialno in trials) {
    
    trialdf <- taskdf[taskdf$trial_no == trialno,]
    
    step.idx <- which(trialdf$step == 99) # only for re-trace
    
    x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
    y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
    t <- trialdf$time_ms[step.idx]
    
    if (trialdf$internalMovement[1] < 0) x <- -x
    
    # point <- which(sqrt(x^2 + y^2) > 0.15)[1] # WHERE IS THIS POINT?
    # percept <- (atan2(y[point], x[point]) / pi) * 180
    #initialdirection <- c(initialdirection, 90 - percept)
    
    # why do this and also the smoothed spline?
    # this only says that at some point X is going back to the midline
    # but it's not a good estimate of where that happens
    boundary <- which(diff(x) < 0)[1]
    
    if (is.na(boundary)) {
      # no reset point detected!
      # lines(x,y,col='#66666699')
      lines(x,y,col=colors[['lightblue']]$s)
    } else {
      
      smspl <- smooth.spline(t, x, spar=.25)
      x_p <- predict(smspl$fit, t)$y
      
      localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
      
      lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
      localmaxima <- localmaxima[which(lmd > 0.1)]
      
      lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
      localmaxima <- localmaxima[which(lmd > 0.01)]
      
      if (length(localmaxima) > 0) {
        
        boundX <- x[localmaxima[1]]
        boundY <- y[localmaxima[1]]
        
        # points(boundX,boundY,col='#b400e4ff')
        points(boundX,boundY,col=colors[['purple']]$s)
        
        boundary <- localmaxima[1]
        # lines(x[1:boundary],y[1:boundary],col='#b400e4ff')
        lines(x[1:boundary],y[1:boundary],col=colors[['purple']]$s)
        lines(x[boundary:length(x)],y[boundary:length(x)],col='#CCCCCCFF')
        
      } else {
        # lines(x,y,col='#66666699')
        lines(x,y,col=colors[['lightblue']]$s)
      }
      
    }
    
  }
  
  axis(side=1,at=c(0,4)/13.5,labels=c('0','4'))
  axis(side=2,at=c(0,4.5,9,13.5)/13.5,labels=c('0.0','4.5','9.0','13.5'))
  
  
  # # # # # # # # # # # # # # # # # # # # 
  #
  #   plot all individual reset points:
  #
  # # # # # # # # # # # # # # # # # # # # 
  
  colors=getColors()
  
  #layout(matrix(c(1,1,2,1,1,3),nrow=2,ncol=3,byrow=TRUE))
  
  # get resets per trial, and remove trials without resets
  df <- getDataTrials()
  df <- df[which(!is.na(df$X)),]
  #df <- df[which(df$X < 8 & df$X > 0),]
  df <- df[which(df$X > 0),]
  df <- df[which(df$Y < 13.5 & df$Y > 0),]
  #df <- df[which(df$Y > 0),]
  
  # get the number of resets per participants, for weighting:
  ntrials <- table(df$participant)
  
  # set the weights per participant:
  weights = rep(NA, dim(df)[1])
  for (participant in unique(df$participant)) {
    weights[which(df$participant == participant)] <- 1 / ntrials[sprintf('%d',participant)]
  }
  
  # get 2d density matrix:
  dens2d <- density2D(x=df$X,
                      y=df$Y,
                      weights=weights,
                      n=c(8/.1, 13.5/.1),
                      from=c(-.5,0),
                      to=c(8,13.5),
                      bw=0.4
  )
  
  # print(dens2d$x)
  # print(dens2d$y)
  
  # image(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #       main=sprintf('distribution of %d reset points', dim(df)[1]),
  #       xlim=c(-1,8), ylim=c(-1,13.5),
  #       xlab='',ylab='',
  #       asp=1, ax=F, bty='n',
  #       col=gray.colors(n=13,start=1.0, end=0.5))
  plot(x=df$X, y=df$Y,
       main=sprintf('distribution of reset points'),
       pch=1, col='#33333322',
       xlim=c(-1,8), ylim=c(-1,13.5),
       xlab='',ylab='',
       asp=1, ax=F, bty='n')
  
  title(xlab='cm', line=2.5)
  title(ylab='cm', line=2.5)
  
  
  nlevels <- 9
  top <- max(dens2d$z)
  levels <- seq(top/nlevels,top,top/nlevels)
  levels <- levels[1:(length(levels)-1)]
  contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
          add=TRUE,
          levels=levels,
          asp=1, ax=F, bty='n',
          #col=gray.colors(n=13,start=0.9, end=0),
          col=hcl.colors(n=nlevels, palette='Peach', rev=TRUE),
          drawlabels=FALSE, lw=2)
  
  # SunsetDark is cool
  
  # filled.contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #                xlim=c(0,8), ylim=c(0,13.5),
  #                xlab='',ylab='',
  #                #add=TRUE,
  #                levels=seq(top/nlevels,top,top/nlevels), 
  #                asp=1, ax=F, bty='n',
  #                #col=gray.colors(n=13,start=0.9, end=0),
  #                col=hcl.colors(n=nlevels, palette='SunsetDark', rev=TRUE))
  
  
  
  # ADD LIMITS TO FIGURE:
  
  # bs_fits <- read.csv('data/bootstrapped_fits.csv', stringsAsFactors = FALSE)
  # 
  # 
  # X_qs <- quantile(bs_fits$Xlim_X, probs = c(0.025, 0.500, 0.975))
  # #print(X_qs)
  # 
  # polygon(x=c(X_qs[c(1,3)],rev(X_qs[c(1,3)])),
  #         y=c(0,0,13.5,13.5),
  #         col=colors$purple$t,
  #         border = NA)
  # lines(x=c(X_qs[2],X_qs[2]),y=c(0,13.5),
  #       col=colors$purple$s)
  # 
  # 
  # # TIME LIMIT
  # 
  # T_qs <- quantile(bs_fits$Tlim_T, probs = c(0.025, 0.500, 0.975))
  # 
  # arch_X <- cos(seq(0,pi/2,pi/180))
  # arch_Y <- sin(seq(0,pi/2,pi/180))
  # 
  # speeds <- 13.5/c(3,4)
  # 
  # for (speed_no in c(1:length(speeds))) {
  #   speed <- speeds[speed_no]
  #   pX <- c(arch_X * T_qs[1] * speed, rev(arch_X * T_qs[3] * speed))
  #   pY <- c(arch_Y * T_qs[1] * speed, rev(arch_Y * T_qs[3] * speed))
  #   polygon(pX,pY,
  #           col=c(colors$blue$t,colors$blue$t)[speed_no],
  #           border = NA)
  #   lines(x = arch_X * T_qs[2] * speed,
  #         y = arch_Y * T_qs[2] * speed,
  #         col=c(colors$blue$s,colors$blue$s)[speed_no],
  #         lty=speed_no)
  #   
  # }
  
  
  axis(side=1, at=c(0,4,8))
  axis(side=2, at=seq(0,13.5,length.out = 4))
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
  
}
  
  
  
plotIllusionStrength <- function(target='inline') {
  
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_illusionstrength.pdf',onefile=TRUE,width=7.5,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/Fig4_illusionstrength.svg',width=7.5,height=4)
  }
  
  layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = T),width=c(1.7,1.3))
  
  par(mar=c(3.5, 3.5, 2.5, 0.5))
  
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #
  #  COMPARISON WITH CAVANAGH & TSE (2019) DATA / MODEL
  #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  colors <- getColors()
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  
  
  #df <- getTimeNormalizedData(illusionMinimum = 0)
  
  df <- getData54()
  
  avg_df <- aggregate(angle ~ Vi + speed + participant, data=df, FUN=mean)
  avg_df <- aggregate(angle ~ Vi + speed, data=avg_df, FUN=mean)
  
  
  participants <- unique(df$participant)
  p_k <- c()
  for (participant in participants) {
    
    pdf <- df[which(df$participant == participant),]
    avg_pdf <- aggregate(angle ~ Vi + speed, data=pdf, FUN=mean)
    avg_p_xcoords <- atan((avg_pdf$Vi / 0.58) / (avg_pdf$speed))
    
    X <- (avg_p_xcoords/pi)*180
    Y <- 90 - ((as.numeric(unlist(avg_pdf$angle))/pi)*180)
    linmod <- lm(Y ~ X - 1)
    p_k <- c( p_k, summary(linmod)$coefficients['X','Estimate'] )
    
  }
  
  bs_k <- rowMeans( matrix(sample(p_k, size=1000*length(participants),replace = TRUE), nrow = 1000, ncol=length(participants)) )
  CI95 <- quantile(bs_k, probs=c(0.025,0.50,0.975))
  
  Vi <- df$Vi / 0.58 # in cm/s 
  Ve <- df$speed # in cm/s
  
  xcoords <- atan(Vi / Ve)
  
  avg_xcoords <- atan((avg_df$Vi / 0.58) / (avg_df$speed))
  
  idxE3 <- which(df$Ve == 0.167)
  idxE4 <- which(df$Ve == 0.125)
  
  avg_idxE3 <- which(avg_df$speed == 13.5/3)
  avg_idxE4 <- which(avg_df$speed == 13.5/4)
  
  plot(-1000,-1000,
       main='illusion strength',xlab='',ylab='',
       xlim=c(0,5*(pi/12)),ylim=c(0,46),
       bty='n',ax=F)
  
  title(xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')), line=2.5)
  title(ylab='illusion strength [°]', line=2.5)
  
  
  polygon(x = c(0,rep(5*(pi/12),2),0),
          y = c(0,(75 * CI95[c(1,3)]),0), # bootstrapped confidence interval for k
          col=colors$lightblue$t,
          border=NA)
  
  angles <- c(0,5*(pi/12))
  #lines(angles,(angles/pi)*180,col='gray',lty=2)
  lines(angles,0.74*((angles/pi)*180),col='black',lty=1)
  
  #xcoords <- atan(internalspeed / externalspeed)
  
  # get the best k for this data:
  X <- (avg_xcoords/pi)*180
  Y <- 90 - ((as.numeric(unlist(avg_df$angle))/pi)*180)
  linmod <- lm(Y ~ X - 1)
  slope <- summary(linmod)$coefficients['X','Estimate']
  
  # plot that as a line:
  lines(angles,slope*((angles/pi)*180),col=colors$lightblue$s,lty=1)
  
  points(avg_xcoords[avg_idxE3], 90 - ((avg_df$angle[avg_idxE3]/pi)*180), col=colors$purple$s, pch=16)
  points(avg_xcoords[avg_idxE4], 90 - ((avg_df$angle[avg_idxE4]/pi)*180), col=colors$orange$s, pch=16)
  
  legend(x=0, y=45, 
         legend = c('group means (3 s)', 'group means (4 s)', 'Heller et al. (2021)', sprintf('K=%0.2f',slope)), 
         pch=c(16,16,NA,NA), col=c(colors$purple$s, colors$orange$s, 'black', colors$lightblue$s), 
         lty = c(0,0,1,1),
         bty='n', cex=0.8)
  
  #axis(side=1,at=seq(0,3*(pi/8),pi/8),labels=c('0',expression(pi/8),expression(pi/4),expression(3*pi/8)))
  axis(side=1,at=seq(0,5*(pi/12),pi/12),labels=sprintf('%d',seq(0,75,15)))
  axis(side=2,at=seq(0,45,15))
  
  
  # # # # # # # # # # # # # # # # # # # # 
  #
  #   model limits:
  #
  # # # # # # # # # # # # # # # # # # # # 
  
  colors=getColors()
  
  #layout(matrix(c(1,1,2,1,1,3),nrow=2,ncol=3,byrow=TRUE))
  
  # get resets per trial, and remove trials without resets
  df <- getDataTrials()
  df <- df[which(!is.na(df$X)),]
  #df <- df[which(df$X < 8 & df$X > 0),]
  df <- df[which(df$X > 0),]
  df <- df[which(df$Y < 13.5 & df$Y > 0),]
  #df <- df[which(df$Y > 0),]
  
  # get the number of resets per participants, for weighting:
  ntrials <- table(df$participant)
  
  # set the weights per participant:
  weights = rep(NA, dim(df)[1])
  for (participant in unique(df$participant)) {
    weights[which(df$participant == participant)] <- 1 / ntrials[sprintf('%d',participant)]
  }
  
  # # get 2d density matrix:
  # dens2d <- density2D(x=df$X,
  #                     y=df$Y,
  #                     weights=weights,
  #                     n=c(8/.1, 13.5/.1),
  #                     from=c(-.5,0),
  #                     to=c(8,13.5),
  #                     bw=0.4
  # )
  
  # print(dens2d$x)
  # print(dens2d$y)
  
  # image(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #       main=sprintf('distribution of %d reset points', dim(df)[1]),
  #       xlim=c(-1,8), ylim=c(-1,13.5),
  #       xlab='',ylab='',
  #       asp=1, ax=F, bty='n',
  #       col=gray.colors(n=13,start=1.0, end=0.5))
  plot(x=df$X, y=df$Y,
       main=sprintf('model reset limits', dim(df)[1]),
       pch=1, col='#33333322',
       xlim=c(-1,11), ylim=c(-1,13.5),
       xlab='',ylab='',
       asp=1, ax=F, bty='n')
  
  title(xlab='cm', line=2.5)
  title(ylab='cm', line=2.5)
  
  
  # nlevels <- 9
  # top <- max(dens2d$z)
  # levels <- seq(top/nlevels,top,top/nlevels)
  # levels <- levels[1:(length(levels)-1)]
  # contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #         add=TRUE,
  #         levels=levels,
  #         asp=1, ax=F, bty='n',
  #         #col=gray.colors(n=13,start=0.9, end=0),
  #         col=hcl.colors(n=nlevels, palette='Peach', rev=TRUE),
  #         drawlabels=FALSE, lw=2)
  
  # SunsetDark is cool
  
  # filled.contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #                xlim=c(0,8), ylim=c(0,13.5),
  #                xlab='',ylab='',
  #                #add=TRUE,
  #                levels=seq(top/nlevels,top,top/nlevels), 
  #                asp=1, ax=F, bty='n',
  #                #col=gray.colors(n=13,start=0.9, end=0),
  #                col=hcl.colors(n=nlevels, palette='SunsetDark', rev=TRUE))
  
  
  
  # ADD LIMITS TO FIGURE:
  
  bs_fits <- read.csv('data/bootstrapped_fits.csv', stringsAsFactors = FALSE)
  
  
  X_qs <- quantile(bs_fits$Xlim_X, probs = c(0.025, 0.500, 0.975))
  #print(X_qs)
  
  polygon(x=c(X_qs[c(1,3)],rev(X_qs[c(1,3)])),
          y=c(0,0,13.5,13.5),
          col=colors$blue$t,
          border = NA)
  lines(x=c(X_qs[2],X_qs[2]),y=c(0,13.5),
        col=colors$blue$s)

  
  # TIME LIMIT
  
  T_qs <- quantile(bs_fits$Tlim_T, probs = c(0.025, 0.500, 0.975))
  
  arch_X <- cos(seq(0,pi/2,pi/180))
  arch_Y <- sin(seq(0,pi/2,pi/180))
  
  speeds <- 13.5/c(3,4)
  
  for (speed_no in c(1:length(speeds))) {
    speed <- speeds[speed_no]
    pX <- c(arch_X * T_qs[1] * speed, rev(arch_X * T_qs[3] * speed))
    pY <- c(arch_Y * T_qs[1] * speed, rev(arch_Y * T_qs[3] * speed))
    polygon(pX,pY,
            col=c(colors$yorkred$t,colors$yorkred$t)[speed_no],
            border = NA)
    lines(x = arch_X * T_qs[2] * speed,
          y = arch_Y * T_qs[2] * speed,
          col=c(colors$yorkred$s,colors$yorkred$s)[speed_no],
          lty=speed_no)

  }
  
  legend(3.5,14,c(sprintf('\nspatial: %0.1f cm\nor ~%0.1f dva', X_qs[2], X_qs[2]*(14/13.5)),
                sprintf('\ntemporal: %0.1f s\n(3 s passes)', T_qs[2]),
                '\ntemporal\n(4 s passes)'),lty=c(1,1,2),
         col=c(colors$blue$s,colors$yorkred$s,colors$yorkred$s),
         bty='n', cex=0.8, y.intersp=1.0, seg.len=1.5)
  
  axis(side=1, at=c(0,4,8))
  axis(side=2, at=seq(0,13.5,length.out = 4))
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


plot6points <- function() {
  
#  df <- getDataPavg()
  df <- getData54()
  
  colors <- getColors()
  
  adf <- aggregate(cbind(X,Y,RT,speed,angle) ~ Vi + Ve, data=df, FUN=mean)
  sdf <- aggregate(cbind(X,Y,RT,speed,angle) ~ Vi + Ve, data=df, FUN=sd)
  
  plot(-1000,-1000,main='N=6',
       xlab='',ylab='',
#       xlim=c(0,5),ylim=c(0,4.5),
       xlim=c(0,5),ylim=c(0,14),
       bty='n',ax=F)
  
  title(xlab='reset X [cm]')
  title(ylab='reset Y [cm]')
  
  for (rown in c(1:dim(adf)[1])) {
    
    # print(adf[rown,])
    col <- colors[[c('lightblue','purple','orange')[adf$Vi[rown]-1]]]
    pch <- c(1,19)[round(0.5/adf$Ve[rown])-2]
    lty <- c(2,1)[adf$Ve[rown]-2]
    print(lty)
    
    mX  <- adf$X[rown]
    mRT <- adf$RT[rown]
    mY  <- adf$Y[rown]
    sX  <- sdf$X[rown]
    sRT <- sdf$RT[rown]
    sY  <- sdf$Y[rown]
    
    # segments(mX-sX,mRT,mX+sX,mRT,col=col$t, lw=3, lty=lty)
    # segments(mX,mRT-sRT,mX,mRT+sRT,col=col$t, lw=3, lty=lty)
    
    idx <- which(df$Vi == adf$Vi[rown] & df$Ve == adf$Ve[rown])
    #CE <- confidenceEllipse(x=df$X[idx], y=df$RT[idx])
    CE <- confidenceEllipse(x=df$X[idx], y=df$Y[idx])
    lines(CE$poly$x, CE$poly$y, col=col$s, lty=lty)
    
    points(mX, mY, col=col$s, pch=pch, cex=1.5)
    
  }
  
  # segments(adf$X-sdf$X,adf$RT,adf$X+sdf$X,adf$RT,col=colors$purple$t)
  # segments(adf$X,adf$RT-sdf$RT,adf$X,adf$RT+sdf$RT,col=colors$purple$t)
  # 
  # points(adf$X, adf$RT, col=colors$purple$s)
  
  #legend(0,4.5,
  legend(0,13.5,
                legend=c('Vi=2','Vi=3','Vi=4','Ve=3','Ve=4'),
         col=c(colors$lightblue$s,
               colors$purple$s,
               colors$orange$s,
               'black',
               'gray'),
         pch=c(19,19,19,1,19),
         lty=c(2,2,2,2,1),
         seg.len = 3,
         bty='n')
  
  axis(side=1,at=c(0,1,2,3,4))
  #axis(side=2,at=seq(0,1.5,2,3.5))
  axis(side=2,at=seq(0,13.5,length.out = 4))
  
  
}


# Statistics -----

test6points <- function() {
  
  df <- getDataPavg()
  
  # MANOVA: no way to compare with ANOVAs
  #RPman <- manova(cbind(X, RT) ~ angle, data = df)
  # summary(RPman)
  
  # ANOVAs: have to use illusion strength as dependent variable...
  # RPaov <- aov(angle ~ X + RT, data = df)
  # RTaov <- aov(angle ~ RT, data = df)
  # Xaov <- aov(angle ~ X, data = df)
  # print(anova(RPaov, Xaov, test="Chisq"))
  
  # # linear models:
  # RPlm <- lm(angle ~ X + RT, data = df)
  # Xlm <- lm(angle ~ X, data = df)
  # RTlm <- lm(angle ~ RT, data = df)
  # 
  # cat('\ncompare: TIME+OFFSET to OFFSET only\n\n')
  # print(anova(RPlm, RTlm))
  # cat('\ncompare: TIME+OFFSET to TIME only\n\n')
  # print(anova(RPlm, Xlm))
  # cat('\ncompare: TIME only to OFFSET only\n\n')
  # print(anova(RTlm, Xlm))
  
  # prep for separate ANOVAs:
  
  df$condition <- ((round(0.5 / unique(df$Ve))-3) * 3)+1 + (df$Vi-2)
  
  
  df$Vi <- as.factor(df$Vi)
  df$Ve <- as.factor(df$Ve)
  df$participant <- as.factor(df$participant)
  df$condition <- as.factor(df$condition)
  
  
  cat('\n RESET TIME: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=RT,wid=participant,within=c(Vi,Ve),data=df,detailed = T)))
  cat('\n RESET Y: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=Y ,wid=participant,within=c(Vi,Ve),data=df,detailed = T)))
  cat('\n RESET X: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=X, wid=participant,within=c(Vi,Ve),data=df,detailed = T)))
  

  cat('\n RESET TIME: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=RT,wid=participant,within=condition,data=df,detailed = T)))
  cat('\n RESET Y: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=Y ,wid=participant,within=condition,data=df,detailed = T)))
  cat('\n RESET X: \n\n')
  print(ezANOVA.pes(ez::ezANOVA(dv=X, wid=participant,within=condition,data=df,detailed = T)))
  
  
}

#' ezANOVA wrapper to calculate 
#'
#'
#' @param ezANOVAResult ezANOVA result
#' @return ezANOVA with partial eta-squared
#' @details ezANOVA must be run with parameter 'detailed=T'.
#' @author Frank Papenmeier
#' @export
#'
ezANOVA.pes <- function (ezANOVAResult)
{
  if (is.null(ezANOVAResult$ANOVA$SSn) | is.null(ezANOVAResult$ANOVA$SSd))
  {
    stop("ezANOVA must be run with parameter 'detailed=T'.")
  }
  
  ezANOVAResult$ANOVA$pes <- ezANOVAResult$ANOVA$SSn/(ezANOVAResult$ANOVA$SSn+ezANOVAResult$ANOVA$SSd)
  
  return(ezANOVAResult)
}

# from my SMCL package:

confidenceEllipse <- function(x, y=NA, interval=.95, vectors=360) {
  
  # get the square root of the chi-squared value for the specified confidence interval:
  chisq.val <- sqrt(qchisq(p=interval, df=2))
  
  # get the covariance matrix of the data:
  if (is.matrix(x)) {
    covmat <- cov( x )
  } else {
    x <- matrix(c(x,y), ncol = 2, byrow = FALSE)
    covmat <- cov( x )
  }
  
  # get the centre of the ellipse:
  centre <- colMeans(x)
  
  # get the eigen decomposition of the covariance matrix
  ev <- eigen(covmat)
  
  # get eigenvalues and -vectors separately:
  eigenvalues <- ev$values
  eigenvectors <- ev$vectors
  
  # determine which is the maximum eigenvalue and -vector:
  max.EigVal.ind <- which.max(eigenvalues)
  
  max.EigVal <- eigenvalues[max.EigVal.ind]
  max.EigVec <- eigenvectors[,max.EigVal.ind]
  
  # and which are the minimum eigenvalue and -vector:
  min.EigVal.ind <- which.min(eigenvalues)
  min.EigVal <- eigenvalues[min.EigVal.ind]
  min.EigVec <- eigenvectors[,min.EigVal.ind]
  
  # calculate the angle of the largest eigen vector:
  phi = ( ( atan2(max.EigVec[2], max.EigVec[1]) %% (2*pi) ) / pi ) * 180;
  
  # ellipse angles:
  thetas <- seq(0,2*pi,length.out=vectors)
  
  # the semi-major and -minor axes:
  a <- chisq.val*sqrt(max.EigVal);
  b <- chisq.val*sqrt(min.EigVal);
  
  # get X and Y coordinates for the flat ellipse:
  X <- a*cos( thetas );
  Y <- b*sin( thetas );
  
  # rotate the ellipse:
  ellipse <- rotateCoordinates(df=data.frame(x=X, y=Y),angle=phi,origin=c(0,0))
  
  # re-centre:
  circumference <- data.frame('x' = ellipse$x + centre[1],
                              'y' = ellipse$y + centre[2])
  
  ellipse <- list()
  ellipse[['poly']] <- circumference
  ellipse[['major']] <- a
  ellipse[['minor']] <- b
  ellipse[['angle']] <- phi
  ellipse[['centre']] <- centre
  
  return(ellipse)
  
}

rotateCoordinates <- function(df,angle,origin=c(0,0)) {
  
  df.names <- names(df)
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix, and subtract origin
  coordinates <- sweep(as.matrix(df), 2, origin)
  
  # rotate the coordinates, add the origin back in
  df <- as.data.frame(sweep(coordinates %*% R, 2, origin*-1))
  
  # restore column names
  names(df) <- df.names
  
  # return the rotated coordinates
  return(df)
  
}


plot2nDerivateResetExamples <- function(participant = 4, trial = 37) {
  
  # load participant's data:
  df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',4))
  
  # select only re-tracing trials:
  tasktrials <- unique(df$trial[df$taskname == 're-trace'])
  taskdf <- df[which(df$trial_no %in% tasktrials),]
  
  # select only trials from one particular condition?
  trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == 0.125 &
                                           abs(taskdf$internalMovement) == 3)])
  trialsdf <- df[which(df$trial_no %in% trials),]
  trialdf <- trialsdf[which(trialsdf$trial_no == 37 & trialsdf$step == 99),]
  
  # get trajectory and convert to cm
  x <- (trialdf$handx_pix / 524) * 13.5
  y <- (trialdf$handy_pix / 524) * 13.5
  
  # start at (0,0)
  x <- x - x[1]
  y <- y - y[1]
  
  # flip so we always go to the right:
  if (trialdf$internalMovement[1] < 0) x <- -x
  
  # plot raw trajectory:
  layout(mat=matrix(c(1,2,2,4,1,3,3,4), nrow=2, ncol=4, byrow=TRUE))

  plot(x, y,
       main='raw trajectory')
  
  print(x)
  print(y)
  
  write.csv(data.frame(x,y,'first'=c(0,diff(x)/diff(y)),'second'=c(0,0,diff(diff(x)/diff(y)))),file = 'example-trajectory.csv',row.names = FALSE)
  
  print(diff(x)/diff(y))
  
  plot( diff(x)/diff(y),
        main='first derivative',
        type='l')
  
}


# reset point tests -----

getTrajectoryExamples <- function(participants = c(2,3,4,5,6,8,9,10,11), nsamples=1) {
  
  output <- NA
  
  for (participant in participants) {
    
    # load participant's data:
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',participant))
    
    # select only re-tracing trials:
    tasktrials <- unique(df$trial[df$taskname == 're-trace'])
    df <- df[which(df$trial_no %in% tasktrials),]
    
    # pick 3 random trials:
    trials <- sample(tasktrials, size=nsamples)
    
    trialsdf <- df[which(df$trial_no %in% trials & df$step == 99),]
    
    for (trial in trials) {
      
      trialdf <- df[which(df$trial_no == trial & df$step == 99),]
      
      # get trajectory and convert to cm
      x <- (trialdf$handx_pix / 524) * 13.5
      y <- (trialdf$handy_pix / 524) * 13.5
      
      # start at (0,0)
      x <- x - x[1]
      y <- y - y[1]
      
      # flip so we always go to the right:
      if (trialdf$internalMovement[1] < 0) x <- -x
      
      trajectory <- data.frame(x,y)
      trajectory$participant <- participant
      trajectory$trial <- trial
      
      if (is.data.frame(output)) {
        output <- rbind(output, trajectory)
      } else {
        output <- trajectory
      }
      
    }
    
  }

  #write.csv(output,file = 'example-trajectories.csv',row.names = FALSE)
  return(output)
  
}


plotTrajectoryMoments <- function() {
  
  nsamples <- 20
  trajectories <- getTrajectoryExamples(participants = c(2,3,4,5,6,8,9,10,11), nsamples=nsamples)
  # 9 trajectories
  
  pdf(file=sprintf('examples_%d.pdf',nsamples), width=8, height=11)
  
  mat <- matrix( data = c(1, 2, 5, 6, 9, 10, 1, 3, 5, 7, 9, 11, 1, 4, 5, 8, 9, 12, 13, 14, 17, 18, 21, 22, 13, 15, 17, 19, 21, 23, 13, 16, 17, 20, 21, 24, 25, 26, 29, 30, 33, 34, 25, 27, 29, 31, 33, 35, 25, 28, 29, 32, 33, 36),
                 nrow = 9, ncol = 6, byrow = TRUE)
  layout(mat=mat,widths=c(1,1.5,1,1.5,1,1.5))
  
  par(mar=c(3, 3, 3, 0) + 0.05)
  
  participants <- unique(trajectories$participant)
  
  for (samplen in c(1:nsamples)) {
    
    for (participant in participants) {
      
      pdf <- trajectories[which(trajectories$participant == participant),]
      trials <- unique(pdf$trial)
      trial <- trials[samplen]
      pdf <- pdf[which(pdf$trial == trial),]
      
      x <- pdf$x
      y <- pdf$y
      
      max_y_idx <- which.max(y)
      x <- x[c(1:max_y_idx)]
      y <- y[c(1:max_y_idx)]
      
      lo_y_idx <- which(y < 0)
      if (length(lo_y_idx) > 0) {
        ok_y_idx <- c(lo_y_idx[length(lo_y_idx)]:length(y))
        x <- x[ok_y_idx]
        y <- y[ok_y_idx]
      }
      
      plot(x, y,
           xlab='',ylab='',main='',
           bty='n',asp=1)
      
      # for people with a lot of samples, downsample severely
      if (length(x) > 75) {
        # idx <-as.integer(round(seq(1,length(sx),length.out = 20)))
        # sx <- sx[idx]
        # sy <- sy[idx]
        # print(length(sx))
        trajectory <- resampleDistance(x=x,y=y,distance=0.9)
        x <- trajectory$x
        y <- trajectory$y
      }
      
      
      #pdf$sx <- as.numeric( runmed(x=pdf$x, k=9) )
      sx <- ksmooth(x = c(1:length(x)),
                    y = x,
                    kernel = 'normal',
                    bandwidth = 5,
                    x.points = c(1:length(x)))$y
      sy <- ksmooth(x = c(1:length(x)),
                    y = y,
                    kernel = 'normal',
                    bandwidth = 5,
                    x.points = c(1:length(y)))$y
      
      # remove duplicates, if any:
      nondupes <- which(sx[-length(sx)] != sx[-1])
      sx <- sx[nondupes]
      sy <- sy[nondupes]
      
      # remove all points closer than 0.4 cm to the origin:
      d_idx <- which((sx^2 + sy^2)> 0.4)
      
      sx <- sx[d_idx]
      sy <- sy[d_idx]
      
      # remove all points beyond the maximum y coordinate:
      my_idx <- which.max(sy)
      
      if (length(my_idx) > 0) {
        sx <- sx[c(1:my_idx)]
        sy <- sy[c(1:my_idx)]
      }
      

      lines(sx,sy,col='blue')
      #rp_idx <- which.max(pdf$sx) # returns the overall maximum
      
      local_maxima <- which( diff(sign(diff(sx))) == -2 )+1
      #print(local_maxima)
      #print(length(local_maxima))
      
      if (length(local_maxima) > 0) {
        RPidx <- which( sx > ( 0.975 * sx[local_maxima[1]] ) )[1] - 1
        
        RPx <- sx[RPidx]
        RPy <- sy[RPidx]
        #print(c(RPx,RPy))
        points(RPx,RPy,col='green',cex=3)
        
      } else {
        text(0,13.5,'X',col='red',cex=3)
        #print('NO!')
      }
      
      
      if (length(sx) > 1) {
      plot(diff(sx),type='l',
           xlab='',ylab='',main='',
           bty='n')
      } else {
        plot.new()
      }
      
      if (length(sx) > 2) {
        plot(diff(diff(sx)),type='l',
             xlab='',ylab='',main='',
             bty='n')
      } else {
        plot.new()
      }
      
      plot.new()
      
    }
    
  }
  
  dev.off()
  
}

resampleDistance <- function(x,y,distance) {
  
  idx <- 1
  

  while (idx < length(x)) {
    # sqrt() takes a lot of time apparently, so we just don't use it:
    distance <- distance^2
    d <- (x-x[idx])^2 + (y-y[idx])^2
    
    select_idx <- which(d > distance | c(1:length(x)) <= idx)
    
    x <- x[select_idx]
    y <- y[select_idx]
    idx <- idx + 1
    
  }
  
  return(list('x'=x,
              'y'=y))
  
}

# Code graveyard -----

# preProcessOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), target='pdf') {
#   
#   internalMovements <- c(2,3,4)
#   externalMovements <- rev(c(.125, .167))
#   
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/onePass_V4_all_participants.pdf',onefile=TRUE,width=11,height=8)
#   }
#   if (target == 'svg') {
#     svglite(file='doc/onePass_V4_all_participants.svg',width=11,height=8)
#   }
#   
#   par(mfrow=c(2,1),mar=c(4,4,2,0.1))
#   
#   for (task in c('arrow','re-trace')) {
#     
#     # we will generate data files for both tasks, with these columns:
#     # except that there is no bound in the arrow task
#     participant <- c()
#     trial <- c()
#     internalspeed <- c()
#     internaldirection <- c()
#     externalspeed <- c()
#     fixationside <- c()
#     initialdirection <- c()
#     illusionstrength <- c()
#     boundX <-c()
#     boundY <-c()
#     
#     
#     plot(-1000,-1000,main=task,xlab='participant',ylab='internal motion',xlim=c(0,(length(participants)*2)+1),ylim=c(0,3),bty='n',ax=FALSE,asp=1)
#     
#     for (participant.idx in c(1:length(participants))) {
#       
#       ppno <- participants[participant.idx]
#       
#       df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',ppno))
#       
#       tasktrials <- unique(df$trial[df$taskname == task])
#       
#       taskdf <- df[which(df$trial_no %in% tasktrials),]
#       
#       for (EM.idx in c(1:length(externalMovements))) {
#         
#         for (IM.idx in c(1:length(internalMovements))) {
#           
#           if (task %in% c('track','re-trace')) {
#             lines(c(0,0)+(participant.idx*2)-2+EM.idx,c(0.1,0.9)+IM.idx-1,col='#000000',lty=3)
#           }
#           
#           trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == externalMovements[EM.idx] & abs(taskdf$internalMovement) == internalMovements[IM.idx])])
#           
#           Xbounds <- c()
#           Ybounds <- c()
#           
#           for (trialno in trials) {
#             
#             trialdf <- taskdf[taskdf$trial_no == trialno,]
#             
#             
#             participant <- c(participant, ppno)
#             trial <- c(trial, trialno)
#             internalspeed <- c(internalspeed, internalMovements[IM.idx])
#             internaldirection <- c(internaldirection, ifelse(trialdf$internalMovement[1] > 0, 1, -1))
#             externalspeed <- c(externalspeed, externalMovements[EM.idx])
#             fixationside <- c(fixationside, trialdf$fixationside[1])
#             
#             
#             
#             if (task %in% c('arrow','ruler')) {
#               
#               percept <- trialdf$percept[1]
#               
#               #if (task == 'arrow') percept <- 90 - (percept - 90)
#               
#               if (trialdf$internalMovement[1] < 0) percept <- 90 - (percept - 90)
#               
#               x <- c(0,cos((percept/180)*pi)) * 0.6
#               y <- c(0,sin((percept/180)*pi)) * 0.6
#               
#               lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#00000033')
#               
#               initialdirection <- c(initialdirection, 90 - percept)
#               illusionstrength <- c(illusionstrength, 90 - percept)
#               boundX <- c(boundX, NA)
#               boundY <- c(boundY, NA)
#               
#             } else {
#               
#               step <- list('track'=2,'re-trace'=99)[[task]]
#               
#               step.idx <- which(trialdf$step == step)
#               
#               # x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
#               # y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
#               
#               x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0)
#               y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5)
#               x <- x - x[1]
#               y <- y - y[1]
#               prop <- max(y)
#               x <- x / prop
#               y <- y / prop
#               # x <- x
#               # y <- y
#               t <- trialdf$time_ms[step.idx]
#               
#               # if (trial == 2 & participant == 1) {
#               #   print(step.idx)
#               #   # print(x)
#               #   # print(y)
#               # }
#               
#               if (trialdf$internalMovement[1] < 0) x <- -x
#               
#               # this has to be redone once the reset point is found!
#               point <- which(sqrt(x^2 + y^2) > 0.15)[1]
#               percept <- (atan2(y[point], x[point]) / pi) * 180
#               initialdirection <- c(initialdirection, 90 - percept)
#               # IDcoords <- c(x[point],y[point])
#               
#               # why do this and also the smoothed spline?
#               # to test if we should be doing the splines at all?
#               # and the splines find the "more correct" point... hmmm...
#               boundary <- which(diff(x) < 0)[1]
#               
#               if (is.na(boundary)) {
#                 lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
#                 
#                 boundX <- c(boundX, NA)
#                 boundY <- c(boundY, NA)
#                 illusionstrength <- c(illusionstrength, NA)
#                 
#               } else {
#                 
#                 #lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
#                 #lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
#                 # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
#                 
#                 smspl <- smooth.spline(t, x, spar=.25)
#                 x_p <- predict(smspl$fit, t)$y
#                 
#                 # these are sign changes from positive to negative
#                 localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
#                 # distance of local max has to be more tha (0.1) but the trajectory is scaled to 0-0.6 (??? why)
#                 lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
#                 localmaxima <- localmaxima[which(lmd > 0.1)]
#                 # the maxima can also not be very close to the end of the trajectory:
#                 lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
#                 localmaxima <- localmaxima[which(lmd > 0.01)]
#                 # do we have any left?
#                 if (length(localmaxima) > 0) {
#                   
#                   #   points(x[localmaxima[1]]+participant-0.5,IM.idx-0.1,col='#FF0000')
#                   #   points(participant-0.7,y[localmaxima[1]]+IM.idx-0.9,col='#FF0000')
#                   
#                   # store the first local maximum as reset point:
#                   # Xbounds <- c(Xbounds, x[localmaxima[1]])
#                   # Ybounds <- c(Ybounds, y[localmaxima[1]])
#                   
#                   boundX <- c(boundX, x[localmaxima[1]])
#                   boundY <- c(boundY, y[localmaxima[1]])
#                   
#                   # **********************************
#                   # ACTUAL HALFWAY POINTS HERE:
#                   #print(sqrt(sum(IDcoords^2)))
#                   hwd <- sqrt(sum(c(x[localmaxima[1]],y[localmaxima[1]])^2)) / 2
#                   hwp <- which(sqrt(x^2 + y^2) > hwd)[1]
#                   hwpercept <- (atan2(y[hwp], x[hwp]) / pi) * 180
#                   illusionstrength <- c(illusionstrength, 90 - hwpercept)
#                   
#                   #print(percept - hwpercept)
#                   
#                   points(x[localmaxima[1]]+(participant.idx*2)-2+EM.idx,y[localmaxima[1]]+IM.idx-0.9,col='#b400e4ff')
#                   
#                   boundary <- localmaxima[1]
#                   lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
#                   lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
#                   
#                   
#                   
#                 } else {
#                   lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
#                   boundX <- c(boundX, NA)
#                   boundY <- c(boundY, NA)
#                   illusionstrength <- c(illusionstrength, NA)
#                 }
#                 
#               }
#               
#             } 
#             
#           }
#           
#           # done all trials, can now draw distributions of end points
#           
#           # if (task %in% c('track', 're-trace')) {
#           #   
#           #   print(Xbounds)
#           #   
#           #   Xavg <- mean(Xbounds, na.rm=TRUE)
#           #   Xstd <- sd(Xbounds, na.rm=TRUE)
#           #   Yavg <- mean(Ybounds, na.rm=TRUE)
#           #   Ystd <- sd(Ybounds, na.rm=TRUE)
#           #   
#           #   print(c(Xavg,Xstd,Yavg,Ystd))
#           #   
#           #   XdistrX <- seq(-0.1, 0.6,  .01)
#           #   XdistrY <- dnorm(XdistrX,mean=Xavg,sd=Xstd) / dnorm(c(Xavg),mean=Xavg,sd=Xstd)
#           #   XdistrY <- XdistrY / 10
#           #   
#           #   YdistrY <- seq(0.0, 0.70, .01)
#           #   YdistrX <- dnorm(YdistrY,mean=Yavg,sd=Ystd) / dnorm(c(Yavg),mean=Yavg,sd=Ystd)
#           #   YdistrX <- YdistrX / 10
#           #   
#           #   lines(XdistrX+(participant.idx*2)-2+EM.idx,XdistrY+IM.idx-1.1,col='#0fd2e2ff')
#           #   lines(YdistrX+(participant.idx*2)-2.2+EM.idx,YdistrY+IM.idx-0.9,col='#0fd2e2ff')
#           #   
#           #   
#           #   # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC')
#           #   
#           # }
#           
#         }
#         
#       }
#       
#     }
#     
#     axis(side=1,at=(c(1:length(participants)*2)-0.5),labels=c(1:length(participants)))
#     axis(side=2,at=c(0.5,1.5,2.5),labels=c('2','3','4'))
#     
#     # write out csv file:
#     
#     # participant <- c()
#     # internalspeed <- c()
#     # externalspeed <- c()
#     # fixationside <- c()
#     # initialdirection <- c()
#     # boundX <-c()
#     # boundY <-c()
#     
#     df <- data.frame(participant, trial, internalspeed, internaldirection, externalspeed, fixationside, initialdirection, illusionstrength, boundX, boundY)
#     write.csv(df, file=sprintf('data/onePass_V4/onePass_V4_%s.csv', task), quote=F, row.names=F)
#     
#   }
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }


# oldPlotData <- function(target='inline') {
#   
#   
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
#   }
#   if (target == 'svg') {
#     svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
#   }
#   
#   par(mar=c(3.5, 3.5, 2.5, 0.5))
#   
#   colors <- getColors()
#   
#   # there will be three plots:
#   # 1: example participant
#   # 2: 2D overview of average reset points
#   # 3: Cavangh & Tse comparison
#   
#   layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T), widths=c(1.2,1,1.7))
#   
#   # **********************************
#   # EXAMPLE PARTICIPANT PLOT:
#   
#   plot(-1000,-1000,
#        main='participant 4, 3 cps, 4 s',xlab='',ylab='',
#        xlim=c(-.25,.5), ylim=c(-.1,.9),
#        bty='n',ax=F, asp=1)
#   
#   title(xlab='cm', line=2.5)
#   title(ylab='cm', line=2.5)
#   
#   df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',4))
#   tasktrials <- unique(df$trial[df$taskname == 're-trace'])
#   taskdf <- df[which(df$trial_no %in% tasktrials),]
#   
#   trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == 0.125 &
#                                            abs(taskdf$internalMovement) == 3)])
#   
#   # loop through those trials:
#   for (trialno in trials) {
#     
#     trialdf <- taskdf[taskdf$trial_no == trialno,]
#     
#     step.idx <- which(trialdf$step == 99) # only for re-trace
#     
#     x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
#     y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
#     t <- trialdf$time_ms[step.idx]
#     
#     if (trialdf$internalMovement[1] < 0) x <- -x
#     
#     # point <- which(sqrt(x^2 + y^2) > 0.15)[1] # WHERE IS THIS POINT?
#     # percept <- (atan2(y[point], x[point]) / pi) * 180
#     #initialdirection <- c(initialdirection, 90 - percept)
#     
#     # why do this and also the smoothed spline?
#     # this only says that at some point X is going back to the midline
#     # but it's not a good estimate of where that happens
#     boundary <- which(diff(x) < 0)[1]
#     
#     if (is.na(boundary)) {
#       # no reset point detected!
#       # lines(x,y,col='#66666699')
#       lines(x,y,col=colors[['lightblue']]$s)
#     } else {
#       
#       smspl <- smooth.spline(t, x, spar=.25)
#       x_p <- predict(smspl$fit, t)$y
#       
#       localmaxima <- which(diff(sign(diff(x_p)))==-2)+1
#       
#       lmd <- sqrt(x[localmaxima]^2 + y[localmaxima]^2)
#       localmaxima <- localmaxima[which(lmd > 0.1)]
#       
#       lmd <- sqrt((x[localmaxima]-x[length(x)])^2 + (y[localmaxima]-y[length(y)])^2)
#       localmaxima <- localmaxima[which(lmd > 0.01)]
#       
#       if (length(localmaxima) > 0) {
#         
#         boundX <- x[localmaxima[1]]
#         boundY <- y[localmaxima[1]]
#         
#         # points(boundX,boundY,col='#b400e4ff')
#         points(boundX,boundY,col=colors[['purple']]$s)
#         
#         boundary <- localmaxima[1]
#         # lines(x[1:boundary],y[1:boundary],col='#b400e4ff')
#         lines(x[1:boundary],y[1:boundary],col=colors[['purple']]$s)
#         lines(x[boundary:length(x)],y[boundary:length(x)],col='#CCCCCCFF')
#         
#       } else {
#         # lines(x,y,col='#66666699')
#         lines(x,y,col=colors[['lightblue']]$s)
#       }
#       
#     }
#     
#   }
#   
#   axis(side=1,at=c(0,2,4)/13.5,labels=c('0','2','4'))
#   axis(side=2,at=c(0,4,8,12)/13.5,labels=c('0','4','8','12'))
#   
#   
#   # ***********************************************
#   # 2D overview of reset points:
#   
#   #df <- summarizeTraceBoundsV4()
#   
#   df <- getData54()
#   
#   
#   # PANEL A: raw spatial coordinates:
#   
#   # scatter of reset points
#   plot(df$X, df$Y, main='reset point coordinates', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   title(xlab='horizontal reset coordinate [cm]', line=2.5)
#   title(ylab='vertical reset coordinate [cm]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(df$X)
#   text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(df$Y)
#   text(6,medY+0.5,sprintf('%0.1f cm',medY))
#   lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(-2,0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_cm <- seq(0,13.5,length.out = 4)
#   axis(side=2, at=ytick_cm, labels=ytick_cm)
#   
#   # ***********************************************************
#   # COMPARISON WITH CAVANAGH & TSE (2019) DATA / MODEL
#   
#   colors <- getColors()
#   solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
#   transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
#   
#   
#   #df <- getTimeNormalizedData(illusionMinimum = 0)
#   
#   df <- getData54()
#   
#   avg_df <- aggregate(angle ~ Vi + speed + participant, data=df, FUN=mean)
#   avg_df <- aggregate(angle ~ Vi + speed, data=avg_df, FUN=mean)
#   
#   
#   participants <- unique(df$participant)
#   p_k <- c()
#   for (participant in participants) {
#     
#     pdf <- df[which(df$participant == participant),]
#     avg_pdf <- aggregate(angle ~ Vi + speed, data=pdf, FUN=mean)
#     avg_p_xcoords <- atan((avg_pdf$Vi / 0.58) / (avg_pdf$speed))
#     
#     X <- (avg_p_xcoords/pi)*180
#     Y <- 90 - ((as.numeric(unlist(avg_pdf$angle))/pi)*180)
#     linmod <- lm(Y ~ X - 1)
#     p_k <- c( p_k, summary(linmod)$coefficients['X','Estimate'] )
#     
#   }
#   
#   bs_k <- rowMeans( matrix(sample(p_k, size=1000*length(participants),replace = TRUE), nrow = 1000, ncol=length(participants)) )
#   CI95 <- quantile(bs_k, probs=c(0.025,0.50,0.975))
#   
#   Vi <- df$Vi / 0.58 # in cm/s 
#   Ve <- df$speed # in cm/s
#   
#   xcoords <- atan(Vi / Ve)
#   
#   avg_xcoords <- atan((avg_df$Vi / 0.58) / (avg_df$speed))
#   
#   idxE3 <- which(df$Ve == 0.167)
#   idxE4 <- which(df$Ve == 0.125)
#   
#   avg_idxE3 <- which(avg_df$speed == 13.5/3)
#   avg_idxE4 <- which(avg_df$speed == 13.5/4)
#   
#   plot(-1000,-1000,
#        main='illusion strength',xlab='',ylab='',
#        xlim=c(0,5*(pi/12)),ylim=c(0,45),
#        bty='n',ax=F)
#   
#   title(xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')), line=2.5)
#   title(ylab='illusion strength [°]', line=2.5)
#   
#   polygon(x = c(0,rep(5*(pi/12),2),0),
#           y = c(0,(75 * CI95[c(1,3)]),0),
#           col=colors$purple$t,
#           border=NA)
#   
#   angles <- c(0,5*(pi/12))
#   lines(angles,(angles/pi)*180,col='gray',lty=2)
#   lines(angles,0.81*((angles/pi)*180),col='black',lty=1)
#   
#   #xcoords <- atan(internalspeed / externalspeed)
#   
#   # get the best k for this data:
#   X <- (avg_xcoords/pi)*180
#   Y <- 90 - ((as.numeric(unlist(avg_df$angle))/pi)*180)
#   linmod <- lm(Y ~ X - 1)
#   slope <- summary(linmod)$coefficients['X','Estimate']
#   
#   # plot that as a line:
#   lines(angles,slope*((angles/pi)*180),col=colors$purple$s,lty=1)
#   
#   # for (ppno in unique(df$participant)) {
#   #   
#   #   idx <- which(df$participant == ppno)
#   #   
#   #   X <- (xcoords[idx]/pi)*180
#   #   Y <- as.numeric(unlist(df$initialdirection_mean[idx]))
#   #   linmod <- lm(Y ~ X - 1)
#   #   slope <- summary(linmod)$coefficients['X','Estimate']
#   #   cat(sprintf('participant: %d: %0.2f\n', ppno, slope))
#   #   
#   # }
#   
#   # points(xcoords[idxE3], 90 - ((df$angle[idxE3]/pi)*180), col=colors$blue$t, pch=16)
#   # points(xcoords[idxE4], 90 - ((df$angle[idxE4]/pi)*180), col=colors$yorkred$t, pch=16)
#   
#   points(avg_xcoords[avg_idxE3], 90 - ((avg_df$angle[avg_idxE3]/pi)*180), col=colors$blue$s, pch=1)
#   points(avg_xcoords[avg_idxE4], 90 - ((avg_df$angle[avg_idxE4]/pi)*180), col=colors$yorkred$s, pch=1)
#   
#   legend(x=0, y=45, 
#          legend = c('9 participants (3 s)', '9 participants (4 s)', 'group averages', 'K=1', 'K=0.81', sprintf('K=%0.2f',slope)), 
#          pch=c(16,16,1,NA,NA,NA), col=c(colors$blue$t, colors$yorkred$t, 'black', 'gray', 'black', colors$purple$s), 
#          lty = c(0,0,0,2,1,1),
#          bty='n')
#   
#   #axis(side=1,at=seq(0,3*(pi/8),pi/8),labels=c('0',expression(pi/8),expression(pi/4),expression(3*pi/8)))
#   axis(side=1,at=seq(0,5*(pi/12),pi/12),labels=sprintf('%d',seq(0,75,15)))
#   axis(side=2,at=seq(0,45,15))
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }


# plotNData <- function(target='inline') {
#   
#   
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
#   }
#   if (target == 'svg') {
#     svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
#   }
#   
#   par(mar=c(3.5, 3.5, 2.5, 0.5))
#   
#   colors <- getColors()
#   
#   # there will be three plots:
#   # 1: example participant
#   # 2: 2D overview of average reset points
#   # 3: Cavangh & Tse comparison
#   
#   layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T))
#   
#   
#   
#   # ***********************************************
#   # 2D overview of reset points:
#   
#   #df <- summarizeTraceBoundsV4()
#   
#   df <- getDataPavg()
#   
#   
#   # PANEL A: raw spatial coordinates:
#   
#   # scatter of reset points
#   plot(-1000, -1000, main='participant X condition [N=54]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   idxE4 <- which(df$speed > (13.5/3.5))
#   idxE3 <- which(df$speed < (13.5/3.5))
#   
#   points(df$X[idxE4], df$Y[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(df$X[idxE3], df$Y[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   title(xlab='horizontal reset coordinate [cm]', line=2.5)
#   title(ylab='vertical reset coordinate [cm]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(df$X)
#   text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(df$Y)
#   text(6,medY+0.5,sprintf('%0.1f cm',medY))
#   lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(-2,0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_cm <- seq(0,13.5,length.out = 4)
#   axis(side=2, at=ytick_cm, labels=ytick_cm)
#   
#   # B: condition
#   
#   cdf <- aggregate(cbind(X,Y) ~ Vi + Ve + speed, data=df, FUN=mean)
#   
#   
#   # scatter of reset points
#   plot(-1000,-1000, main='condition [N=6]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   idxE4 <- which(cdf$speed > (13.5/3.5))
#   idxE3 <- which(cdf$speed < (13.5/3.5))
#   
#   points(cdf$X[idxE4], cdf$Y[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(cdf$X[idxE3], cdf$Y[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   title(xlab='horizontal reset coordinate [cm]', line=2.5)
#   title(ylab='vertical reset coordinate [cm]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   #segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   #segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(cdf$X)
#   text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(cdf$Y)
#   text(6,medY+0.5,sprintf('%0.1f cm',medY))
#   lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(-2,0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_cm <- seq(0,13.5,length.out = 4)
#   axis(side=2, at=ytick_cm, labels=ytick_cm)
#   
#   
#   
#   
#   
#   
#   
#   
#   # C: participant averages
# 
#   ppdf <- aggregate(cbind(X,Y,RT) ~ participant + speed, data=df, FUN=mean)
#   
#   # scatter of reset points
#   plot(-1000, -1000, main='participant [N=9]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   
#   idxE4 <- which(ppdf$speed > (13.5/3.5))
#   idxE3 <- which(ppdf$speed < (13.5/3.5))
#   
#   points(ppdf$X[idxE4], ppdf$Y[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(ppdf$X[idxE3], ppdf$Y[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   
#   
#   title(xlab='horizontal reset coordinate [cm]', line=2.5)
#   title(ylab='vertical reset coordinate [cm]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(ppdf$X)
#   text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(ppdf$Y)
#   text(6,medY+0.5,sprintf('%0.1f cm',medY))
#   lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(-2,0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_cm <- seq(0,13.5,length.out = 4)
#   axis(side=2, at=ytick_cm, labels=ytick_cm)
#   
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }




# plotNRTData <- function(target='inline') {
#   
#   
#   if (target == 'pdf') {
#     cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
#   }
#   if (target == 'svg') {
#     svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
#   }
#   
#   par(mar=c(3.5, 3.5, 2.5, 0.5))
#   
#   colors <- getColors()
#   
#   # there will be three plots:
#   # 1: example participant
#   # 2: 2D overview of average reset points
#   # 3: Cavangh & Tse comparison
#   
#   layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T))
#   
#   
#   
#   # ***********************************************
#   # 2D overview of reset points:
#   
#   #df <- summarizeTraceBoundsV4()
#   
#   #df <- getData()
#   df <- getDataPavg()
#   
#   # PANEL A: raw spatial coordinates:
#   
#   # scatter of reset points
#   plot(-1000, -1000, main='participant X condition [N=54]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   idxE4 <- which(df$speed > (13.5/3.5))
#   idxE3 <- which(df$speed < (13.5/3.5))
#   
#   points(df$X[idxE4], df$RT[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(df$X[idxE3], df$RT[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   title(xlab='reset offset [cm]', line=2.5)
#   title(ylab='reset time [s]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(df$X)
#   text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(df$RT)
#   text(4.2,medY-0.1,sprintf('%0.1f s',medY))
#   lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_s <- c(0:4)
#   axis(side=2, at=ytick_s, labels=ytick_s)
#   
#   # B: condition
#   
#   cdf <- aggregate(cbind(X,Y,RT) ~ Vi + Ve + speed, data=df, FUN=mean)
#   
#   
#   # scatter of reset points
#   plot(-1000,-1000, main='condition [N=6]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   idxE4 <- which(cdf$speed > (13.5/3.5))
#   idxE3 <- which(cdf$speed < (13.5/3.5))
#   
#   points(cdf$X[idxE4], cdf$RT[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(cdf$X[idxE3], cdf$RT[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   title(xlab='reset offset [cm]', line=2.5)
#   title(ylab='reset time [s]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   #segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   #segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(cdf$X)
#   text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(cdf$RT)
#   text(4.2,medY-0.1,sprintf('%0.1f s',medY))
#   lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   
#   legend(medX+.2,4,c('4 s passes','3 s passes'),pch=c(1,19), col=colors[['purple']]$s, bty='n')
#   
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_s <- c(0:4)
#   axis(side=2, at=ytick_s, labels=ytick_s)
#   
#   
#   
#   
#   
#   
#   
#   
#   # C: participant averages
#   
#   ppdf <- aggregate(cbind(X,Y,RT) ~ participant + speed, data=df, FUN=mean)
#   
#   # scatter of reset points
#   plot(-1000, -1000, main='participant [N=9 or N=18?]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
#   
#   
#   idxE4 <- which(ppdf$speed > (13.5/3.5))
#   idxE3 <- which(ppdf$speed < (13.5/3.5))
#   
#   points(ppdf$X[idxE4], ppdf$RT[idxE4], pch=1,  col=colors[['purple']]$s)
#   points(ppdf$X[idxE3], ppdf$RT[idxE3], pch=19, col=colors[['purple']]$s)
#   
#   
#   
#   title(xlab='reset offset [cm]', line=2.5)
#   title(ylab='reset time [s]',  line=2.5)
#   #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
#   
#   
#   
#   # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
#   # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
#   
#   # path of gabor:
#   #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
#   arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
#   
#   # X coordinates:
#   medX <- mean(ppdf$X)
#   text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
#   lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
#   
#   # Y coordinates:
#   medY <- mean(ppdf$RT)
#   text(4.2,medY-0.1,sprintf('%0.1f s',medY))
#   lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
#   
#   # sensible tick marks on the axes:
#   # xtick_cm <- c(-1,1,3)
#   xtick_cm <- c(0,2,4)
#   axis(side=1, at=xtick_cm, labels=xtick_cm)
#   ytick_s <- c(0:4)
#   axis(side=2, at=ytick_s, labels=ytick_s)
#   
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }







# plotModels <- function(target='inline') {
#   
#   colors <- getColors()
#   
#   if (target == 'pdf') {
#     cairo_pdf(filename='model_figure.pdf',onefile=TRUE,width=8,height=5.25)
#   }
#   if (target == 'svg') {
#     svglite(file='model_figure.svg',width=8,height=5.25)
#   }
#   
#   par(mar=c(3.4, 3.4, 2.1, 3.75))
#   layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T), widths = c(1,1,1))
#   
#   #df <- getTimeNormalizedData()
#   df <- getData54()
#   
#   # ***************************************
#   # PLOT SINGLE LIMIT MODELS: TIME LIMIT
#   
#   fit <- fitSingleLimitModels(df=df)
#   
#   directions <- c(10:40)
#   raddirections <- ((90 - directions) / 180) * pi
#   slope <- sin(raddirections) / cos(raddirections)
#   
#   # PANEL B: x coords from time limit
#   
#   #plot(df$initialdirection_mean, df$boundX_mean*13.5, main=sprintf('time limit (%0.1f s)', fit$par['Ly']*(4/13.5)), xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,10), col=colors[['blue']]$s)
#   plot(90-((df$angle/pi)*180), df$X, col=colors[['blue']]$s, 
#        main='time limit', xlab='', ylab='', 
#        xlim=c(5,45), ylim=c(0,13.5), 
#        bty='n', ax=F)
#   
#   title(xlab='illusion strength [°]', line=2.4, cex=0.8)
#   title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
#   
#   #lines(x=range(directions),y=rep(fit$par['Lx'],2),col=colors[['yorkred']]$s,lty=1)
#   
#   #fittedX <- resetXfromYlim(fit$par,slopes)$X
#   #lines(directions,fittedX,col=colors[['blue']]$s)
#   
#   sin.a <- sin(raddirections)
#   cos.a <- cos(raddirections)
#   
#   data <- data.frame(sin.a, cos.a, slope)
#   
#   for (spd in c(3.375, 4.5)) {
#     data$speed <- spd
#     fittedX <- YlimResets(fit$Ylim$par,data=data)
#     lines(directions,fittedX$X,col=colors[['blue']]$s)
#   }
#   
#   # median models
#   # XfromMedY <- resetXfromYlim(c('Ly'=medY*13.5),slopes)$X
#   # lines(directions,XfromMedY,col=colors[['blue']]$s, lty=2)
#   
#   legend(15,10,c('reset distance','reset time'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset coordinate:',bty='n')
#   
#   
#   axis(side=1, at=c(10,20,30,40))
#   axis(side=2, at=c(0,2,4,6,8,10,12),las=1)
#   
#   # ******************************************
#   # SINGLE LIMIT MODEL: SPACE LIMIT
#   
#   # PANEL C: Y coords from space limit
#   
#   #plot(df$initialdirection_mean, df$boundY_mean*4, main=sprintf('space limit (%0.1f cm)', fit$par['Lx']), xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,4), col=colors[['yorkred']]$s)
#   plot(90-((df$angle/pi)*180), df$RT, col=colors[['yorkred']]$s,
#        main='space limit', xlab='', ylab='',
#        xlim=c(5,45), ylim=c(0,4), 
#        bty='n', ax=F)
#   
#   title(xlab='illusion strength [°]', line=2.4, cex=0.8)
#   title(ylab='reset time [s]', line=2.4, cex=0.8)
#   
#   #print( (fit$par['Ly']/13.5)*4 )
#   #lines(x=range(directions),y=rep((fit$par['Ly']/13.5)*4,2),col=colors[['blue']]$s)
#   
#   # fittedY <- resetYfromXlim(fit$par,slopes)$Y
#   # lines(directions,fittedY/4,col=colors[['yorkred']]$s)
#   
#   
#   fittedY <- XlimResets(fit$Xlim$par,data=data)
#   for (spd in c(3.375,4.5)) {
#     resetTime <- sqrt(fittedY$Y^2 + fittedY$X^2) / spd
#     lines(directions,resetTime,col=colors[['yorkred']]$s)
#   }
#   
#   
#   
#   # median models
#   # YfromMedX <- resetYfromXlim(c('Lx'=medX*4),slopes)$Y
#   # lines(directions,YfromMedX,col=colors[['yorkred']]$s, lty=2)
#   
#   axis(side=1, at=c(10,20,30,40))
#   axis(side=2, at=c(0,1,2,3,4),las=1)
#   
#   # ***********************************************
#   # COMBINED LIMIT MODEL
#   
#   plot(-1000,-1000,main='combined limits',
#        xlab='',ylab='',
#        xlim=c(5,45),ylim=c(0,1),
#        bty='n',ax=F)
#   
#   title(xlab='illusion strength [°]', line=2.4, cex=0.8)
#   title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
#   
#   points(90-((df$angle/pi)*180),df$X/13.5,col=colors[['blue']]$s)
#   points(90-((df$angle/pi)*180),df$RT/4,col=colors[['yorkred']]$s)
#   
#   
#   #par <- fitResetModelSeq(slopes=df$slope, X=df$boundX_mean*13.5, Y=df$boundY_mean*13.5)
#   fit <- fitTwoLimitModel(df)
#   
#   for (spd in c(3.375, 4.5)) {
#     data$speed <- spd
#     fitted <- twoLimResets(par=fit$par, data=data)
#     lines(directions,fitted$X/13.5,col=colors[['blue']]$s)
#     resetTime <- sqrt(fitted$Y^2 + fitted$X^2) / spd
#     lines(directions,resetTime/4,col=colors[['yorkred']]$s)
#   }
#   
#   #legend(25,0.8,c('X','Y'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset\ncoordinate:',bty='n')
#   
#   axis(1,at=seq(10,40,10))
#   axis(2,at=seq(0,12,2)/13.5,labels=c('0','2','4','6','8','10','12'),las=1)
#   axis(4,at=seq(0,4,1)/4,labels=c('0','1','2','3','4'),las=1)
#   
#   mtext('reset time [s]',side=4,line=2,cex=0.7)
#   
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
#   
# }

# plotIndividual6points <- function() {
#   
#   df <- getDataPavg()
#   
#   pdf(file='doc/individual_6_points.pdf', width=8,height=8)
#   
#   layout(matrix(c(1:9),byrow=TRUE,ncol=3,nrow=3))
#   
#   for (ppno in unique(df$participant)) {
# 
#     plot(-1000,-1000,
#          main=sprintf('participant %d',ppno),
#          xlim=c(0,5),ylim=c(0,5),bty='n',ax=F,
#          xlab='reset offset [cm]',ylab='reset time [s]')
#     
#     for (speed in unique(df$speed)) {
#       
#       color = list('3.375'='orange','4.500'='purple')[[sprintf('%0.3f',speed)]]
#       df.idx <- which(df$participant == ppno & df$speed == speed)
#       
#       points(df$X[df.idx], df$RT[df.idx],col=color)
# 
#     }
#     
#     if (ppno == 11) {
#       legend(0,5,legend=c('4 s passes', '3 s passes'),col=c('purple','orange'),bty='n',pch=1)
#     }
#     
#     axis(side=1,at=c(0:4))
#     axis(side=2,at=c(0:4))
#     
#   }
#   
#   dev.off()
#   
# }


source('R/common.R')
library('ez')

getParticipantAverageDeviations <- function(participants = c(2,3,4,5,6,8,9,10,11)) {
  
  devdf <- NA
  
  for (ppno in participants) {
    
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv', ppno), stringsAsFactors = F)
    
    internalMotions <- sort(unique(abs(df$internalMovement)))
    
    ppdf <- NA
    
    for (im in internalMotions) {
      
      imdf <- df[which(abs(df$internalMovement) == im),]
      
      # normalize direction:
      idx <- which(imdf$internalMovement < 0)
      imdf$handx_pix[idx] <- -1 * imdf$handx_pix[idx]
      
      # get only the re-tracing samples:
      imdf <- imdf[which(imdf$step == 99),]
      
      # loop through all trials:
      trialnos <- unique(imdf$trial_no)
      
      time_s <- c()
      hordev <- c()
      
      for (trialno in trialnos) {
        
        # get out the part of the data for this trial:
        trialdf <- imdf[which(imdf$trial_no == trialno),]
        naidx <- which(!is.na(trialdf$handx_pix) | !is.na(trialdf$handy_pix))
        trialdf <- trialdf[naidx,]
        
        # if there are very few samples (by accident) skip the trial:
        if (dim(trialdf)[1] < 50) {
          next
        }
        
        # get the X and Y coordinates:
        x <- trialdf$handx_pix
        y <- trialdf$handy_pix
        
        if (trialdf$externalMovement[1] == 0.125) {
          duration <- 4
        } else {
          duration <- 3
        }
        
        #print(any(is.na(y)))
        t <- y - min(y)
        x <- x - min(x)
        #print(any(is.na(t)))
        #print(max(t))
        
        use_cm <- TRUE
        
        if (use_cm) {
          #x <- (x / 524.) * 13.5
          x <- (x / max(t)) * 13.5
          x <- x / 0.7298552 # cm/d on the centre of the track
        } else {
          # express x in seconds? counter intuitive, but can be scaled with the Y 
          x <- (x / max(t)) * duration # scale x to y (assuming y spans more pixels)
        }
        
        # t should be scaled always
        t <- (t / max(t)) * duration # scale y (=t) to duration too
        #print(any(is.na(t)))
        
        # remove anything that is beyond 3 seconds:
        tidx <- which(t <= 3)
        x <- x[tidx]
        t <- t[tidx]
        
        t_p <- seq(0,3,length.out = 100)
        
        #print(max(t_p)/max(t))
        #cat(sprintf('pp: %d, trial: %d\n',ppno, trialno))
        
        smspl <- smooth.spline(t, x, spar=.10, tol=1e-4) # low smoothing tol=1e-4
        x_p <- predict(smspl$fit, t_p)$y
        
        x_p <- x_p - x_p[1]
        
        time_s <- c(time_s, t_p)
        hordev <- c(hordev, x_p)
        
      } # end loop through trials
      
      ppimdf <- aggregate(hordev ~ time_s, data=data.frame(time_s, hordev), FUN=mean)
      ppimdf$participant <- ppno
      ppimdf$internalmotion <- im
      
      if (is.data.frame(ppdf)) {
        ppdf <- rbind(ppdf, ppimdf)
      } else {
        ppdf <- ppimdf
      }
      
      
    } # end loop through internalMotions           
    
    if (is.data.frame(devdf)) {
      devdf <- rbind(devdf, ppdf)
    } else {
      devdf <- ppdf
    }
    
  } # end loop through participants
  
  return(devdf)
  
} # end function

plotDeviations <- function() {
  
  colors <- getColors()
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  
  devdf <- getParticipantAverageDeviations()
  
  layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow=TRUE))
  
  for (im in c(2,3,4)) {
    
    plot(x=c(0,0),y=c(0,3),type='l',lty=3,col='#000000',main=sprintf('internal motion %d',im), xlim=c(-1,4), ylim=c(-.5,3.5), xlab='percept deviation [dva]', ylab='time [s]', bty='n', ax=F)
    
    #PSEs <- c(1.00, 0.83, 0.66, 1.65, 1.50, 1.20)
    
    # path length: 2 DVA
    # the unit is not centimeters, though?
    points(c(1.00, 0.83, 0.66),c(1,2,3),col='#999999',pch=16,cex=2)
    lines(c(1.00, 0.83, 0.66),c(1,2,3),col='#999999',lw=3,lty=2)
    text(x=.66,y=3.2,'2')
    # path length: 4 DVA
    points(c(1.65, 1.50, 1.20),c(1,2,3),col='#999999',pch=16,cex=2)
    lines(c(1.65, 1.50, 1.20),c(1,2,3),col='#999999',lw=3,lty=2)
    text(x=1.2,y=3.2,'4')
    
    
    imdf <- devdf[which(devdf$internalmotion == im),]
    
    participants <- unique(imdf$participant)
    
    for (ppno in participants) {
      
      ppdf <- imdf[which(imdf$participant == ppno),]
      
      lines(x=ppdf$hordev, y=ppdf$time_s, col=transp[[sprintf('%d',im)]])
      
    }
    
    conddf <- aggregate(hordev ~ time_s, data=imdf, FUN=mean)
    
    lines(x=conddf$hordev, y=conddf$time_s, col=solids[[sprintf('%d',im)]])
    
    conddf <- conddf[which(conddf$time_s %in% c(1,2,3)),]
    points(conddf$hordev, conddf$time_s, col=solids[[sprintf('%d',im)]], cex=2)
    
    axis(side=2, at=c(0,1,2,3))
    axis(side=1, at=c(0,1,2,3))
    
  }
  
}

plotSplitDeviations <- function() {
  
  colors <- getColors()
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  
  devdf <- getParticipantAverageDeviations()
  
  layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3, byrow=TRUE))
  
  for (direction in c(-1,1)) {
    
    for (im in c(2,3,4)) {
      
      plot(x=c(0,0),y=c(0,3),type='l',col='#999999',main=sprintf('internal motion %d',im), xlim=c(-.5,2), ylim=c(-.5,3.5), xlab='percept deviation', ylab='time [s]', asp=1, bty='n', ax=F)
      
      imdf <- devdf[which(devdf$internalmotion == im),]
      
      participants <- unique(imdf$participant)
      
      compliant <- c()
      
      for (ppno in participants) {
        
        ppdf <- imdf[which(imdf$participant == ppno),]
        
        linreg <- lm(hordev ~ time_s, data=ppdf[which(ppdf$time_s >= .5),])
        if (direction *linreg$coefficients[['time_s']] > 0) {
          
          lines(x=ppdf$hordev, y=ppdf$time_s, col=transp[[sprintf('%d',im)]])
          
          compliant <- c(compliant, ppno)
          
        }
        
      }
      
      conddf <- aggregate(hordev ~ time_s, data=imdf[which(imdf$participant %in% compliant),], FUN=mean)
      
      lines(x=conddf$hordev, y=conddf$time_s, col=solids[[sprintf('%d',im)]])
      
      axis(side=2, at=c(0,1,2,3))
      
    }
    
  }
  
}


plotResetYdistribution <- function() {
  
  colors <- getColors()
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  
  # read file with all detected resets
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)
  
  # remove trials without resets
  df <- df[which(!is.na(df$boundX)),]
  
  # normalize for time:
  idx3 <- which(df$externalspeed == 0.167)
  idx4 <- which(df$externalspeed == 0.125)
  df$boundY[idx3] <- df$boundY[idx3] * 3
  df$boundY[idx4] <- df$boundY[idx4] * 4
  
  # we create one one plot per absolute internal speed
  # create a layout matrix:
  layout(matrix(c(1,2,3),nrow=1,ncol=3))
  
  for (im in c(2,3,4)) {
    
    plot(-1000,-1000,xlim=c(0,3),ylim=c(0,1.6),xlab='time [s]', ylab='relative density',ax=F,bty='n',main=sprintf('internal motion %d',im))
    
    for (ppno in unique(df$participant)) {
      
      subdf <- df[which(df$participant == ppno & df$internalspeed == im),]
      kd <- density(subdf$boundY, bw=0.2, n=151, from=0, to=3)
      lines(kd$x, kd$y, col=transp[[sprintf('%d',im)]])
      
    }
    
    imdf <- df[which(df$internalspeed == im),]
    kd <- density(imdf$boundY, bw=0.2, n=151, from=0, to=3)
    lines(kd$x, kd$y, col=solids[[sprintf('%d',im)]])
    
    axis(side=1,at=c(0,1,2,3))
    axis(side=2,at=c(0,0.5,1.0,1.5))
    
  }
  
}

timeANOVA <- function() {
  
  devdf <- getParticipantAverageDeviations()
  
  devdf <- devdf[which(devdf$time_s %in% c(1,2,3)),]
  
  devdf$participant <- as.factor(devdf$participant)
  devdf$internalmotion <- as.factor(devdf$internalmotion)
  devdf$time_s <- as.factor(devdf$time_s)
  
  print(ezANOVA(data=devdf, dv=hordev, wid=participant, within=c(time_s,internalmotion)))
  
}
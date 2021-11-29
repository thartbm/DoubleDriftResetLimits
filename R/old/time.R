
source('R/common.R')
library('ez')

# Comparing trajectories with PSE/time model ====

getParticipantAverageDeviations <- function(participants = c(2,3,4,5,6,8,9,10,11), getDistribution=FALSE) {
  
  # data frame: horizontal deviation ~ participant * IM * EM * time 
  # for plotting & ANOVA
  devdf <- NA
  
  # interpolation time vector:
  t_p <- seq(0,3,length.out = 151)
  
  if (getDistribution) {
  # array for distributions:
    ddistr <- array(data=NA, dim=c(3,2,length(participants),3,101))
  }
  
  for (ppno in c(1:length(participants))) {
    
    ppid <- participants[ppno]
    
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv', ppid), stringsAsFactors = F)
    
    df$externalMovement[which(df$externalMovement == 0.125)] <- 4
    df$externalMovement[which(df$externalMovement == 0.167)] <- 3
    
    internalMotions <- sort(unique(abs(df$internalMovement)))
    externalMotions <- sort(unique(abs(df$externalMovement)))
    
    ppdf <- NA
    
    for (exno in c(1:length(externalMotions))) {
      
      ex <- externalMotions[exno]
      
      for (imno in c(1:length(internalMotions))) {
        
        im <- internalMotions[imno]
        
        imdf <- df[which(abs(df$internalMovement) == im & df$externalMovement == ex),]
        
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
          
          duration <- trialdf$externalMovement[1]
          
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
          
          #print(max(t_p)/max(t))
          #cat(sprintf('pp: %d, trial: %d\n',ppno, trialno))
          
          smspl <- smooth.spline(t, x, spar=.10, tol=1e-4) # low smoothing tol=1e-4
          x_p <- predict(smspl$fit, t_p)$y
          
          x_p <- x_p - x_p[1]
          
          time_s <- c(time_s, t_p)
          hordev <- c(hordev, x_p)
          
        } # end loop through trials
        
        ppimdf <- aggregate(hordev ~ time_s, data=data.frame(time_s, hordev), FUN=mean)
        ppimdf$participant <- ppid
        ppimdf$internalmotion <- im
        ppimdf$externalmotion <- ex
        
        if (is.data.frame(ppdf)) {
          ppdf <- rbind(ppdf, ppimdf)
        } else {
          ppdf <- ppimdf
        }
        
        # DISTRIBUTIONS TOO!
        
        if (getDistribution) {
          # get all 
          disdf <- data.frame(time_s, hordev)
          for (timepoint in c(1,2,3)) {
            
            tp_devs <- disdf[which(disdf$time_s == timepoint),]
            
            iepd <- density(disdf[which(disdf$time_s == timepoint),]$hordev,
                            kernel='gaussian',
                            n=101,from=-1,to=4)$y
            
            # dimnames = c('im','ex','pp','hordev')
            ddistr[imno, exno, ppno, timepoint, ] <- iepd
            
          }
        }
        
      } # end loop through internalMotions           
      
    } # end loop through externalMotions
    
    if (is.data.frame(devdf)) {
      devdf <- rbind(devdf, ppdf)
    } else {
      devdf <- ppdf
    }
    
  } # end loop through participants
  
  if (getDistribution) {
    return(list('devdf'=devdf, 'ddistr'=ddistr))
  } else {
    return(devdf)
  }
  
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

timeANOVA <- function(IMfactor=FALSE, EXfactor=FALSE) {
  
  devdf <- getParticipantAverageDeviations()
  
  devdf <- devdf[which(devdf$time_s %in% c(1,2,3)),]
  
  devdf$participant <- as.factor(devdf$participant)
  devdf$internalmotion <- as.factor(devdf$internalmotion)
  devdf$externalmotion <- as.factor(devdf$externalmotion)
  devdf$time_s <- as.factor(devdf$time_s)
  
  if (IMfactor & EXfactor) {
    print(ezANOVA(data=devdf, dv=hordev, wid=participant, within=c(time_s,internalmotion,externalmotion)))
    return()
  }
  if (IMfactor) {
    print(ezANOVA(data=devdf, dv=hordev, wid=participant, within=c(time_s,internalmotion)))
    return()
  }
  if (EXfactor) {
    print(ezANOVA(data=devdf, dv=hordev, wid=participant, within=c(time_s,externalmotion)))
    return()
  }
  print(ezANOVA(data=devdf, dv=hordev, wid=participant, within=c(time_s)))

  
  
}

# Sirui's model ====

fitResetProbability <- function() {
  
  # these are the 6 conditions:
  conditions <- expand.grid(
    'duration'   = c( 1, 2, 3 ),
    'path_length' = c( 2, 4 )
  )
  
  # these are the average PSEs:
  PSEs <- c(1.00, 0.83, 0.66, 1.65, 1.50, 1.20)
  
  par <- c(0.01)
  
  fit <- optim(par          = par,
               fn           = timeLimitMSE, 
               conditions   = conditions,
               runs         = 5000,
               PSEs         = PSEs,
               method       = 'Brent',
               lower        = c(0),
               upper        = c(1))
  
  print(fit)
  
}

timeLimitMSE <- function(par, conditions, PSEs, slice=0.01, runs=1000) {
  
  MSE <- c()
  
  for (cn in dim(conditions)[1]) {
    
    duration <- conditions$duration[cn]
    path_length <- conditions$path_length[cn]
    
    predictedPSEs <- bootstrapPSEs(par=par, duration=duration, path_length=path_length, slice=slice, runs=runs)
    
    MSE <- c(MSE, (mean(predictedPSEs) - PSEs[cn])^2)
    
  }
  
  return(mean(MSE))
  
}

bootstrapPSEs <- function(par, duration=1, path_length=1, slice=0.01, runs=1000) {
  
  # repeat for a sufficciently large number of runs:
  allPSEs <- sapply(rep(list(par),runs),FUN=generateOnePSE, slice=slice, duration=duration, path_length=path_length)
  
  return(allPSEs)
  
}

generateOnePSE <- function(par, slice=0.01, duration=1, path_length=1, returnTrajectory=FALSE) {
  
  reset_probability <- par[1]
  
  # any other model parameters?
  
  steps <- duration / slice
  timesteps <- c(1:steps)
  
  reset_time <- 0
  # number_of_resets <- 0
  
  drift <- rep(NA,steps)
  
  for (time in timesteps) {
    
    # where is the stimulus drifting of to?
    current_drift <- (path_length / sqrt(2)) * (time - reset_time)/(steps-1)
    
    # criterion for reset:
    # depends on reset_probability only (multiplier is always 1)
    #cutoff <- reset_probability * ((current_drift / path_length)^0)
    
    #if (cutoff > runif(1)) {
    if (reset_probability > runif(1)) {
        # reset time!
      reset_time       <- time

    }
    
    # store current position
    drift[time] <- current_drift
    
  }
  
  if (returnTrajectory) {
    # in case we want the whole trajectory:
    return(drift)
  } else {
    # last drift location should correspond to PSE:
    return(drift[steps])
  }
  
}

# Continuous PSE from simple model ====

# Can we get something like the trajectories from the simple model Sirui made?
# Should be possible... not sure how accurate it is, or if it is the right thing to do

getTimeModelTrajectories <- function(durations=c(1,2,3), path_lengths=c(2,4), runs=2000) {
  
  conditions <- expand.grid(duration=durations, path_length=path_lengths)
  
  trajectories = list()
  
  for (condn in c(1:dim(conditions)[1])) {
    
    x <- c()
    y <- c()
    
    for (run in c(1:runs)) {
      path <- generateOnePSE( par              = c(.00684), # James Bond factor
                              slice            = 0.01,
                              duration         = conditions[condn,'duration'],
                              path_length      = conditions[condn,'path_length'],
                              returnTrajectory = TRUE)
      
      # simulated paths need to be stored
      x <- c(x,path)
      y <- c(y,seq(0.01,conditions[condn,'duration'],0.01))
      
    }
    
    df <- data.frame(x,y)
    
    trajectory_dist <- hist2d(x=df, edges=list(seq(0,4,.05),seq(0,conditions[condn,'duration'],0.01)))
    trajectory_mean <- aggregate(x ~ y, data=df, FUN=mean)
    
    duration_str <- sprintf('%d',conditions[condn,'duration'])
    path_length_str <- sprintf('%d',conditions[condn,'path_length'])
    
    if (duration_str %notin% names(trajectories)) {
      trajectories[[duration_str]] <- list()
    } 
    trajectories[[duration_str]][[path_length_str]] <- list()
    
    trajectories[[duration_str]][[path_length_str]][['trajectory_dist']] <- trajectory_dist
    trajectories[[duration_str]][[path_length_str]][['trajectory_mean']] <- trajectory_mean
    
  }
  
  return(trajectories)
  
}

plotAverageTrajectories <- function() {
  
  colors <- getColors()
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  
  trace <- getParticipantAverageDeviations(getDistribution = TRUE)
  devdf <- trace[['devdf']]
  distr <- trace[['ddistr']]
  tmdev <- getTimeModelTrajectories()
  
  layout(matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE))
  
  for (path_length in c('2','4')) {
    
    plot(x=c(0,0),y=c(0,3),type='l',lty=3,col='#000000',main=sprintf('%s-dva\nmodel average paths',path_length), xlim=c(-1,4), ylim=c(-.5,3.5), xlab='percept deviation [dva]', ylab='time [s]', bty='n', ax=F)
    
    for (duration in c('1', '2', '3')) {
      
      nrows <- dim(tmdev[[duration]][[path_length]][['trajectory_dist']][['freq2D']])[2]
      
      dist <- tmdev[[duration]][[path_length]][['trajectory_dist']][['freq2D']][,nrows]
      dist <- (dist / max(dist, na.rm=TRUE)) / 2
      dist <- dist[which(dist > 0)]
      
      xedges <- tmdev[['1']][['2']][['trajectory_dist']][['x.edges']]
      xmid <- xedges[1:(length(xedges)-1)] + (diff(xedges)/2)
      xmid <- xmid[1:length(dist)]
      
      pX <- c(xmid, xmid[length(dist)], xmid[1])
      pY <- c(dist, 0, 0) + as.numeric(duration)
      
      #cat(sprintf('pX: %d, pY: %d\n', length(pX), length(pY) ))

      polygon(pX, pY, border=NA, col='#DDDDDD')
      #lines(x=xmid,y=dist+as.numeric(duration),col='#999999')
      
      this_traj <- tmdev[[duration]][[path_length]][['trajectory_mean']]
      
      #print(xmid[which.max(dist)])
      #print(this_traj$y[length(this_traj$y)])
      
      #cat(sprintf('dist peak: %0.3f, avg traj end: %0.3f\n',xmid[which.max(dist)],this_traj$x[length(this_traj$x)]))
      
      lines(this_traj$x, this_traj$y, type='l')
    }
    
    if (path_length == '2') {
      # path length: 2 DVA
      # the unit is not centimeters, though?
      #points(c(1.00, 0.83, 0.66),c(1,2,3),col='#999999',pch=16,cex=2)
      points(c(1.00, 0.83, 0.66),c(1,2,3),col=colors[['yorkred']]$s, cex=2 )
      #lines(c(1.00, 0.83, 0.66),c(1,2,3),col='#999999',lw=3,lty=2)
      #text(x=.66,y=3.2,'2')
    }
    if (path_length == '4') {
      # path length: 4 DVA
      #points(c(1.65, 1.50, 1.20),c(1,2,3),col='#999999',pch=16,cex=2)
      points(c(1.65, 1.50, 1.20),c(1,2,3),col=colors[['yorkred']]$s, cex=2 )
      #lines(c(1.65, 1.50, 1.20),c(1,2,3),col='#999999',lw=3,lty=2)
      #text(x=1.2,y=3.2,'4')
    }
    
    axis(side=2, at=c(0,1,2,3))
    axis(side=1, at=c(0,1,2,3))
    
    
  }
  
  for (exno in c(1,2)) {
    
    ex <- c(3,4)[exno]
    
    plot(-1000,-1000,col='#000000',main=sprintf('average re-traces\n%d s external speed', ex), xlim=c(-1,4), ylim=c(-.5,3.5), xlab='percept deviation [dva]', ylab='time [s]', bty='n', ax=F)
    
    for (timepoint in c(1,2,3)) {
      distros <- distr[ , exno, , timepoint, ]
      distro <- apply(distros,c(3),FUN=mean)
      #distro <- (distro / max(distro)) / 2
      
      pX <- c(seq(-1,4,.05), 4, -1)
      pY <- c(distro, 0, 0) + timepoint

      polygon(pX, pY, border=NA, col='#DDDDDD')
    }
    
    lines(x=c(0,0),y=c(0,3),lty=3,col='#000000')
    
    for (imno in c(1,2,3)) {
      
      im <- c(2,3,4)[imno]
      
      #cat(sprintf('pX: %d, pY: %d\n', length(pX), length(pY) ))
      
      
      
      #PSEs <- c(1.00, 0.83, 0.66, 1.65, 1.50, 1.20)
      
      # path length: 2 DVA
      # the unit is not centimeters, though?
      #points(c(1.00, 0.83, 0.66),c(1,2,3),col='#999999',pch=16,cex=2)
      #lines(c(1.00, 0.83, 0.66),c(1,2,3),col='#CCCCCC',lw=1,lty=2)
      #text(x=.66,y=3.2,'2')
      # path length: 4 DVA
      #points(c(1.65, 1.50, 1.20),c(1,2,3),col='#999999',pch=16,cex=2)
      #lines(c(1.65, 1.50, 1.20),c(1,2,3),col='#CCCCCC',lw=1,lty=2)
      #text(x=1.2,y=3.2,'4')
      
      
      imdf <- devdf[which(devdf$internalmotion == im & devdf$externalmotion == ex),]
      
      # participants <- unique(imdf$participant)
      # 
      # for (ppno in participants) {
      #   
      #   ppdf <- imdf[which(imdf$participant == ppno),]
      #   
      #   lines(x=ppdf$hordev, y=ppdf$time_s, col=transp[[sprintf('%d',im)]])
      #   
      # }
      
      conddf <- aggregate(hordev ~ time_s, data=imdf, FUN=mean)
      
      lines(x=conddf$hordev, y=conddf$time_s, col=solids[[sprintf('%d',im)]])
      
      conddf <- conddf[which(conddf$time_s %in% c(1,2,3)),]
      points(conddf$hordev, conddf$time_s, col=solids[[sprintf('%d',im)]], cex=2)
      
    }
    
    if (ex == 4) {
      legend(2.2,2,legend=sprintf('%d cps',c(2,3,4)),col=unlist(solids),title='internal\nmotion',bty='n',lty=1,lw=1,seg.len=1)
    }
    
    axis(side=2, at=c(0,1,2,3))
    axis(side=1, at=c(0,1,2,3))
    
    
  }
  
}
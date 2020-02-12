library('svglite')
library('ez')

source('R/common.R')
source('R/models.R')

# Data handling -----

preProcessOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), target='pdf') {
  
  internalMovements <- c(2,3,4)
  externalMovements <- rev(c(.125, .167))
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/onePass_V4_all_participants.pdf',onefile=TRUE,width=11,height=8)
  }
  if (target == 'svg') {
    svglite(file='doc/onePass_V4_all_participants.svg',width=11,height=8)
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
      
      df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',ppno))
      
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
    write.csv(df, file=sprintf('data/onePass_V4/onePass_V4_%s.csv', task), quote=F, row.names=F)
    
  }
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


summarizeTraceBoundsV4 <- function() {
  
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)
  
  df2 <- read.csv('data/onePass_V4/onePass_V4_arrow.csv', stringsAsFactors = F)
  
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


getTimeNormalizedData <- function(convertSpeed=TRUE, normalizeTime=TRUE, illusionMinimum=5, addSlopes=TRUE) {
  
  df <- summarizeTraceBoundsV4()
  
  # convert speed to pass duration
  # (from proportion of cycle per second)
  if (convertSpeed) {
    df$externalspeed[which(df$externalspeed == 0.125)] <- 4
    df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  }
  
  # get 3 seconds to last 4 seconds...?
  
  # 13.5 cm / 3 seconds?
  # (Y / 3) * 4
  # 13.5 cm / 4 seconds?
  # (Y / 4) * 4
  
  if (normalizeTime) {
    df$boundY_mean[which(df$externalspeed == 3)] <- ( 4/3 ) * df$boundY_mean[which(df$externalspeed == 3)]
    df$boundY_sd[which(df$externalspeed == 3)]   <- ( 4/3 ) * df$boundY_sd[which(df$externalspeed == 3)]
  }
  
  # only get substantial illusion strengths:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$initialdirection_mean > illusionMinimum),]
  }
  
  # add slopes conveniently:
  # (and illusion strength as angle)
  if (addSlopes) {
    df$direction <- ((90-df$initialdirection_mean)/180)*pi
    df$slope <- sin(df$direction) / cos(df$direction)
  }
  
  return(df)
  
}


getNormalizedHeading2Dhists <- function(segmentpoints=101, resetOnly=TRUE) {
  
  participants = c(2,3,4,5,6,8,9,10,11)
  
  conditions <- list( c(2, 0.125), 
                      c(2, 0.167),
                      c(3, 0.125), 
                      c(3, 0.167),
                      c(4, 0.125), 
                      c(4, 0.167) )
  
  freq2Ds <- list()
  freq2Ds[['hists']] <- list()
  freq2Ds[['edges']] <- list()
  freq2Ds[['avg']] <- list()
  
  for (participant in participants) {
    
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',participant))
    tasktrials <- unique(df$trial[df$taskname == 're-trace'])
    taskdf <- df[which(df$trial_no %in% tasktrials),]
    
    for (condition in conditions) {
      
      internal_speed <- condition[1]
      external_speed <- condition[2]
      
      trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == external_speed & abs(taskdf$internalMovement) == internal_speed)])
      
      conditiondf <- NA
      
      # loop through those trials:
      for (trialno in trials) {
        
        trialdf <- taskdf[taskdf$trial_no == trialno,]
        
        step.idx <- which(trialdf$step == 99) # only for re-trace
        
        # get out the data:
        x  <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
        y  <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
        # t  <- trialdf$time_ms[step.idx]
        
        if (taskdf$internalMovement[1] < 0) {
          x <- -x
        }
        
        boundary <- which(diff(x) < 0)[1]
        
        if (is.na(boundary)) next()
        
        cd <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
        
        # remove duplicate samples:
        del.idx <- which(diff(cd) == 0) + 1
        x  <- x[-del.idx]
        y  <- y[-del.idx]
        # t  <- t[-del.idx]
        cd <- cd[-del.idx]
        
        if (length(cd) < 2) next()
        # scale from 0 to 1:
        cd <- cd / max(cd)
        
        # interpolate to 101 points:
        # getSplinedTrajectory <- function(x, y, t, length.out=length(t), spar=0.01)
        
        # if (length(cd) == 0) next()
        splinedtrack <- getSplinedTrajectory(x=x, y=y, t=cd, length.out=segmentpoints, spar=0.1)
        x_n <- splinedtrack$x
        y_n <- splinedtrack$y
        h_n <- (c(0, atan2(diff(y_n),diff(x_n))) / pi ) * 180
        
        trackdf <- data.frame( 'participant' = rep(participant,segmentpoints),
                               'trial_no' = rep(trialno,segmentpoints),
                               'sample_no' = seq(1:segmentpoints),
                               'x' = x_n,
                               'y' = y_n,
                               'heading' = h_n)
        
        if (is.data.frame(conditiondf)) {
          conditiondf <- rbind(conditiondf, trackdf)
        } else {
          conditiondf <- trackdf
        }
        
      }
      # get the 2D histogram
      pfreq <- hist2d(x=conditiondf$heading, y=conditiondf$sample_no, nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
      avgDir <- aggregate(heading ~ sample_no, data = conditiondf, FUN=mean, na.rm=T)
      
      conditionname <- sprintf('%d-%0.3f',internal_speed,external_speed)
      
      if (conditionname %in% names(freq2Ds[['hists']])) {
        freq2Ds[['hists']][[conditionname]] <- freq2Ds[['hists']][[conditionname]] + (pfreq$freq2D / sum(pfreq$freq2D))
        freq2Ds[['avg']][[conditionname]] <- freq2Ds[['avg']][[conditionname]] + (avgDir / length(participants))
      } else {
        freq2Ds[['hists']][[conditionname]] <- (pfreq$freq2D / sum(pfreq$freq2D))
        freq2Ds[['avg']][[conditionname]] <- (avgDir / length(participants))
      }
      
    }
    
  }
  
  freq2Ds[['x.edges']] <- pfreq$x.edges
  freq2Ds[['y.edges']] <- pfreq$y.edges
  
  return(freq2Ds)
  
}


# Figures -----

plotExampleData <- function(target='inline') {
  
  graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/onePass_V4_boundaries.pdf',onefile=TRUE,width=8,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/onePass_V4_boundaries.svg',width=8,height=4)
  }
  
  colors <- getColors()
  
  f2ds <- getNormalizedHeading2Dhists()
  x.edges <- f2ds[['edges']]$x.edges
  y.edges <- f2ds[['edges']]$y.edges
  
  # what are we going to show?
  
  # for every condition: (internal + external speed)
  
  # - one example participants trajectories with reset points
  
  # - time normalized avg avg heading + avg heading distribution across all data
  
  conditions <- list( c(2, 0.125, 2), 
                      c(2, 0.167, 3),
                      c(3, 0.125, 4), 
                      c(3, 0.167, 5),
                      c(4, 0.125, 6), 
                      c(4, 0.167, 8) )
  
  layout(matrix(c(1:12), ncol=4, nrow=3, byrow=TRUE), widths=c(1,2,1,2))
  
  par(mar=c(4, 4, 2.1, 1.1))
  
  for (condition in conditions) {
    
    # first plot the one participants' example data:
    
    internal_speed <- condition[1]
    external_speed <- condition[2]
    
    example_participant <- condition[3]
    
    ylab <- ''
    if (external_speed == 0.125) {
      ylab <- sprintf('internal speed: %d', internal_speed)
    }
    
    plot(-1000,-1000,xlab='',ylab=ylab,main=sprintf('participant %d',example_participant),xlim=c(-0.2,0.3),ylim=c(0,1),bty='n',ax=FALSE,asp=1)
    
    # load data, select 
    df <- read.csv(sprintf('data/onePass_V4/onepass_V4_p%02d.csv',example_participant))
    tasktrials <- unique(df$trial[df$taskname == 're-trace'])
    taskdf <- df[which(df$trial_no %in% tasktrials),]
    trials <- unique(taskdf$trial_no[which(taskdf$externalMovement == external_speed & abs(taskdf$internalMovement) == internal_speed)])
    
    # loop through those trials:
    for (trialno in trials) {
      
      trialdf <- taskdf[taskdf$trial_no == trialno,]
      
      step.idx <- which(trialdf$step == 99) # only for re-trace
      
      x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
      y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
      t <- trialdf$time_ms[step.idx]
      
      if (trialdf$internalMovement[1] < 0) x <- -x
      
      point <- which(sqrt(x^2 + y^2) > 0.15)[1] # WHERE IS THIS POINT?
      percept <- (atan2(y[point], x[point]) / pi) * 180
      #initialdirection <- c(initialdirection, 90 - percept)
      
      # why do this and also the smoothed spline?
      # this only says that at some point X is going back to the midline
      # but it's not a good estimate of where that happens
      boundary <- which(diff(x) < 0)[1]
      
      if (is.na(boundary)) {
        # no reset point detected!
        # lines(x,y,col='#66666699')
        lines(x,y,col=colors[['lightblue']]$t)
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
          points(boundX,boundY,col=colors[['purple']]$t)
          
          boundary <- localmaxima[1]
          # lines(x[1:boundary],y[1:boundary],col='#b400e4ff')
          lines(x[1:boundary],y[1:boundary],col=colors[['purple']]$t)
          lines(x[boundary:length(x)],y[boundary:length(x)],col='#CCCCCC33')
          
        } else {
          # lines(x,y,col='#66666699')
          lines(x,y,col=colors[['lightblue']]$t)
        }
        
      }
      
    }
    
    
    conditionname <- sprintf('%d-%0.3f',internal_speed,external_speed)
    freq2D <- f2ds[['hists']][[conditionname]]
    
    xlab <- ''
    if (internal_speed == 4) {
      if (external_speed == 0.167) xlab <- 'external speed: 3'
      if (external_speed == 0.125) xlab <- 'external speed: 4'
    }
    
    plot(-1000,-1000,main='all',xlab=xlab,ylab='',xlim=c(-1,1),ylim=c(0,1),bty='n',ax=FALSE,asp=1)
    #print(f2ds$x.edges)
    polarHeatMap(x=x.edges,y=y.edges+30,z=freq2D,mincol=c(1,1,1),maxcol=c(0.06,0.82,0.88),xlim=NA,ylim=NA,xunit='degrees',border=NA,bordercol='white',resolution=1,alpha=1,overlay=TRUE,origin=c(0,0),scale=1,main='')
    
  }
  
  if (target %in% c('pdf', 'svg')) {
    dev.off()
  }
  
}

plotValidity <- function(target='inline') {
  
  #graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/onePass_V4_boundaries.pdf',onefile=TRUE,width=8,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/onePass_V4_boundaries.svg',width=8,height=4)
  }
  
  colors <- getColors()
  
  df <- getTimeNormalizedData()
  
  par(mfrow=c(1,2),mar=c(4.5, 4.5, 2.1, 0.1))
  
  #solids <- list('2'='#b400e4ff', '3'='#e51636ff', '4'='#ff8200ff')
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  #transp <- list('2'='#b400e42f', '3'='#e516362f', '4'='#ff82002f')
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  
  
  # PANEL A
  
  # Illusion strength (arrow direction) over speeds
  
  plot(-1000,-1000,main='initial percept and speed',xlab='external speed',ylab='line angle [deg]',xlim=c(2,4.25),ylim=c(0,30),bty='n',ax=F)
  
  for (internalspeed in c(2,3,4)) {
    
    meanID <- aggregate(arrowdirection_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
    
    confID <- aggregate(arrowdirection_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
    
    X <- c(confID$externalspeed, rev(confID$externalspeed))
    Y <- c(confID[,2][,1], rev(confID[,2][,2]))
    polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
    
    lines(meanID$externalspeed, meanID$arrowdirection_mean, col=solids[[sprintf('%d',internalspeed)]])
    
  }
  
  legend(2,30,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  
  axis(1,at=c(3,4))
  axis(2,at=seq(0,30,5),las=1)
  
  
  # PANEL B
  
  # Illusion strength validity: regression of angle to reset on angle of arrow
  
  plot(-1000,-1000,main='illusion strength measures',xlab='line angle [deg]',ylab='reset point angle [deg]',xlim=c(0,50),ylim=c(0,50),bty='n',ax=F,asp=1)
  
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df)
  
  # check outliers:
  
  strengthCookD <- cooks.distance(strengthLM)
  cutoff <- 4/((nrow(df)-length(strengthLM$coefficients)-2))
  outliers <- unique(which(strengthCookD > cutoff))
  df <- df[-outliers,]
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df)
  # print(summary(strengthLM))
  
  # weighted LM:
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df[-outliers,], weights = 1/sqrt(df[-outliers,]$arrowdirection_mean^2 + df[-outliers,]$initialdirection_mean^2))
  
  #log_strengthLM <- lm(log(initialdirection_mean) ~ arrowdirection_mean, data=df)
  #abline(strengthLM$coefficients, col='#b400e4ff')
  
  #lines(c(0,50),c(0,50),col='#999999')
  
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
  
  # add statistics to figure?
  
  
  
  #axis(1,at=seq(0,50,10))
  axis(2,at=seq(0,50,10))
  axis(1,at=seq(0,50,10))
  #axis(2,at=log(seq(10,40,10)),labels=c('10','20','30','40'))
  
  
  
  
  # # PANEL C
  # 
  # # reset X coordinates over conditions
  # 
  # plot(-1000,-1000,main='horizontal reset position and speed',xlab='external speed',ylab='horizontal reset',xlim=c(2.5,4.5),ylim=c(0,0.25),bty='n',ax=F)
  # 
  # for (internalspeed in c(2,3,4)) {
  #   
  #   meanX <- aggregate(boundX_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
  #   
  #   confX <- aggregate(boundX_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
  #   
  #   X <- c(confX$externalspeed, rev(confX$externalspeed))
  #   Y <- c(confX[,2][,1], rev(confX[,2][,2]))
  #   polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
  #   
  #   lines(meanX$externalspeed, meanX$boundX_mean, col=solids[[sprintf('%d',internalspeed)]])
  #   
  # }
  # 
  # legend(2.5,0.25,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  # 
  # axis(1,at=c(3,4))
  # axis(2,at=seq(0,0.25,0.05),las=1)
  # 
  # 
  # # PANEL D
  # 
  # # reset Y coordinates over conditions
  # 
  # plot(-1000,-1000,main='vertical reset position and speed',xlab='external speed',ylab='vertical reset',xlim=c(2.5,4.5),ylim=c(0,0.75),bty='n',ax=F)
  # 
  # for (internalspeed in c(2,3,4)) {
  #   
  #   meanY <- aggregate(boundY_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=mean, na.rm=T)
  #   
  #   confY <- aggregate(boundY_mean ~ externalspeed, data=df[which(df$internalspeed == internalspeed),], FUN=SMCL::getConfidenceInterval)
  #   
  #   X <- c(confY$externalspeed, rev(confY$externalspeed))
  #   Y <- c(confY[,2][,1], rev(confY[,2][,2]))
  #   polygon(x=X, y=Y, border=NA, col=transp[[sprintf('%d',internalspeed)]])
  #   
  #   lines(meanY$externalspeed, meanY$boundY_mean, col=solids[[sprintf('%d',internalspeed)]])
  #   
  # }
  # 
  # legend(2.5,0.75,c('2','3','4'),col=c('#b400e4ff', '#e51636ff', '#ff8200ff'), lty=1, title='internal speed:',bty='n')
  # 
  # axis(1,at=c(3,4))
  # axis(2,at=seq(0,0.75,0.15),las=1)
  

  
  if (target %in% c('svg', 'pdf')) {
    dev.off()
  }
  
}


plotResetPoints <- function(target='inline') {
  
  #graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/resetPointData.pdf',onefile=TRUE,width=7,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/resetPointData.svg',width=7,height=4)
  }
  
  colors = getColors()
  
  par(mar=c(4.5, 4.5, 2.1, 4.5))
  
  #layout(matrix(c(1,2,4,1,3,4), nrow = 2, ncol = 3, byrow = T), widths = c(1,1,2))
  layout(matrix(c(1,2,1,3), nrow = 2, ncol = 2, byrow = T))
  
  df <- getTimeNormalizedData()

  # PANEL A: raw spatial coordinates:
  
  # scatter of reset points
  plot(df$boundX_mean, df$boundY_mean, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  segments(x0=df$boundX_mean-df$boundX_sd, y0=df$boundY_mean, x1=df$boundX_mean+df$boundX_sd, y1=df$boundY_mean, col=colors[['purple']]$t)
  segments(x0=df$boundX_mean, y0=df$boundY_mean-df$boundY_sd, x1=df$boundX_mean, y1=df$boundY_mean+df$boundY_sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,1,length=0.25,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- median(df$boundX_mean)
  text(x=medX-0.065,y=1.075,labels=sprintf('%0.1f cm',medX*13.5))
  lines(rep(medX,2),c(0.05,1.05),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- median(df$boundY_mean)
  text(4/13.5,medY+0.05,sprintf('%0.1f s',medY*4))
  lines(c(-0.08,0.3),rep(medY,2),col=colors[['blue']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  xtick_cm <- c(-1,1,3)
  axis(side=1, at=xtick_cm/13.5, labels=xtick_cm)
  ytick_cm <- seq(0,4,length.out = 5)
  axis(side=2, at=ytick_cm/4, labels=ytick_cm)
  
  
  fit <- fitSeparateXYresetModels(directions=df$initialdirection_mean,
                                  X=df$boundX_mean * 13.5,
                                  Y=df$boundY_mean * 13.5)
  
  directions <- c(10:40)
  raddirections <- ((90 - directions) / 180) * pi
  slopes <- sin(raddirections) / cos(raddirections)
  
  # PANEL B: space limit from time
  
  plot(df$initialdirection_mean, df$boundX_mean*13.5, main=sprintf('time limit (%0.1f s)', fit$par['Ly']*(4/13.5)), xlab='illusion strength [deg]', ylab='X coordinates [cm]', bty='n', ax=F, xlim=c(5,45), ylim=c(0,10), col=colors[['blue']]$s)
  
  fittedX <- resetXfromYlim(fit$par,slopes)$X
  lines(directions,fittedX,col=colors[['blue']]$s)

  # XfromMedY <- resetXfromYlim(c('Ly'=medY*13.5),slopes)$X
  # lines(directions,fittedX,col=colors[['blue']]$s, lty=2)
  
  axis(side=1, at=c(10,20,30,40))
  axis(side=2, at=c(0,2,4,6,8,10))
  
  
  # PANEL C: time limit from space
  
  plot(df$initialdirection_mean, df$boundY_mean*4, main=sprintf('space limit (%0.1f cm)', fit$par['Lx']), xlab='illusion strength [deg]', ylab='Y coordinates [s]', bty='n', ax=F, xlim=c(5,45), ylim=c(0,4), col=colors[['yorkred']]$s)

  fittedY <- resetYfromXlim(fit$par,slopes)$Y
  lines(directions,fittedY/4,col=colors[['yorkred']]$s)
  
  YfromMedX <- resetYfromXlim(c('Lx'=medX*13.5),slopes)$Y
  #lines(directions,fittedY/4,col=colors[['yorkred']]$s, lty=2)
  
  
  axis(side=1, at=c(10,20,30,40))
  axis(side=2, at=c(0,1,2,3,4))
  
  
  if (target %in% c('pdf', 'svg')) {
    dev.off()
  }
  
}


plotInterdependentModel <- function(target='inline') {
  
  #graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V4_boundaries.pdf',onefile=TRUE,width=5,height=5)
  }
  if (target == 'svg') {
    svglite(file='onePass_V4_boundaries.svg',width=8,height=11)
  }
  
  par(mar=c(4.5, 4.5, 2.1, 4.5))
  
  df <- getTimeNormalizedData()
  
  colors <- getColors()
  
  
  plot(-1000,-1000,main='reset position and illusion strength',xlab='illusion strength (initial direction) [deg]',ylab='horizontal reset distance [cm]',xlim=c(5,45),ylim=c(0,1.1),bty='n',ax=F)
  
  
  
  # BETTER MODEL
  

  par <- fitResetModel(slopes=df$slope, X=df$boundX_mean*13.5, Y=df$boundY_mean*13.5)
  
  #coords <- data.frame(X=df$boundX_mean,Y=df$boundY_mean)
  #directions <- ((90 - df$initialdirection_mean) / 180) * pi
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  model_slopes <- sin(model_directions) / cos(model_directions)
  
  model_resets <- resetModel(par,slopes=model_slopes,verbose=FALSE)
  
  #directions <- 90 - ((directions / pi) * 180)
  
  #plot(-1000,-1000,xlim=c(0,45),ylim=c(0,.8),xlab='illusion strength [deg]',ylab='reset coordinate [normalized]')
  
  lines(model_angles,model_resets$X/13.5,col=colors[['blue']]$s)
  lines(model_angles,model_resets$Y/13.5,col=colors[['yorkred']]$s)
  
  # points(directions,coords$X,pch=16,col='#005de42f')
  # points(directions,coords$Y,pch=16,col='#e516362f')
  points(df$initialdirection_mean,df$boundX_mean,col=colors[['blue']]$s)
  points(df$initialdirection_mean,df$boundY_mean,col=colors[['yorkred']]$s)
  
  
  legend(30,0.75,c('X','Y'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset coordinate:',bty='n')
  
  axis(1,at=seq(10,40,10))
  #axis(2,at=seq(0,0.75,0.15),las=1)
  axis(2,at=seq(0,12,2)/13.5,labels=c('0','2','4','6','8','10','12'),las=1)
  #axis(4,at=seq(0,3,0.5)/4,labels=c('0.0','0.5','1.0','1.5','2.0','2.5','3.0'),las=1)
  axis(4,at=seq(0,4,1)/4,labels=c('0','1','2','3','4'),las=1)
  
  mtext('reset time [s]',side=4,line=3)
  
  par['Lx'] <- par['Lx'] * 13.5
  par['Ly'] <- par['Ly'] * 4
  # print(par)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


plotSequentialModel <- function(target='inline') {
  
  #graphics.off()
  
  if (target == 'pdf') {
    cairo_pdf(filename='onePass_V4_boundaries.pdf',onefile=TRUE,width=5,height=5)
  }
  if (target == 'svg') {
    svglite(file='onePass_V4_boundaries.svg',width=8,height=11)
  }
  
  par(mar=c(4.5, 4.5, 2.1, 4.5))
  
  df <- getTimeNormalizedData()

  colors <- getColors()
  

  plot(-1000,-1000,main='reset position and illusion strength',xlab='illusion strength (initial direction) [deg]',ylab='horizontal reset distance [cm]',xlim=c(5,45),ylim=c(0,1),bty='n',ax=F)
  
  

  
  # EVEN BETTER MODEL
  

  par <- fitResetModelSeq(slopes=df$slope, X=df$boundX_mean*13.5, Y=df$boundY_mean*13.5)
  
  print(par)
  
  #coords <- data.frame(X=df$boundX_mean,Y=df$boundY_mean)
  #directions <- ((90 - df$initialdirection_mean) / 180) * pi
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  model_slopes <- sin(model_directions) / cos(model_directions)
  model_resets <- resetModelSeq(par,slopes=model_slopes,verbose=TRUE)
  
  #print(model_resets)
  
  lines(model_angles,model_resets$X/13.5,col=colors[['blue']]$s)
  lines(model_angles,model_resets$Y/13.5,col=colors[['yorkred']]$s)
  
  points(df$initialdirection_mean,df$boundX_mean,col=colors[['blue']]$s)
  points(df$initialdirection_mean,df$boundY_mean,col=colors[['yorkred']]$s)
  
  
  legend(30,0.75,c('X','Y'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset coordinate:',bty='n')
  
  axis(1,at=seq(10,40,10))
  axis(2,at=seq(0,12,2)/13.5,labels=c('0','2','4','6','8','10','12'),las=1)
  axis(4,at=seq(0,4,1)/4,labels=c('0','1','2','3','4'),las=1)
  
  mtext('reset time [s]',side=4,line=3)
  
  # par['Lx'] <- par['Lx'] * 13.5
  # par['Ly'] <- par['Ly'] * (4/13.5)
  # print(par)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}

# Statistics -----

testValidity <- function(verbosity=0) {
  
  df <- summarizeTraceBoundsV4()
  df$externalspeed[which(df$externalspeed == 0.125)] <- 4
  df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  
  dfAOV <- df
  dfAOV$internalspeed <- as.factor(dfAOV$internalspeed)
  dfAOV$externalspeed <- as.factor(dfAOV$externalspeed)
  print(ez::ezANOVA(dv=arrowdirection_mean, wid=participant, data=dfAOV, within=c(internalspeed, externalspeed), type=3))
  
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df)
  
  if (verbosity > 0) {
    cat('**\n** regression on all data:\n**\n\n')
    print(summary(strengthLM))
  }
  
  # check for and remove outliers:
  strengthCookD <- cooks.distance(strengthLM)
  cutoff <- 4/((nrow(df)-length(strengthLM$coefficients)-2))
  outliers <- unique(which(strengthCookD > cutoff))
  # df <- df[-outliers,]
  
  # redo regression:
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df[-outliers,])
  if (verbosity > 0) {
    cat('**\n** regression with outliers removed:\n**\n\n')
    print(summary(strengthLM))
  }
  
  # notice that the variability in each measures goes up with values on the other measure
  # we probably want to weight the errors of the fit less when the points are further from the origin:
  strengthLM <- lm(initialdirection_mean ~ arrowdirection_mean, data=df[-outliers,], weights = 1/sqrt(df[-outliers,]$arrowdirection_mean^2 + df[-outliers,]$initialdirection_mean^2))
  cat('**\n** weighted regression with outliers removed:\n**\n\n')
  print(summary(strengthLM))
  
  # seems that an orthogonal distance regression would work even better, as the 
  # independent variable (very likely) also has measurement error here, but that is actually
  # hard to do (especially with weights) and not necessary
  
  # maybe the thing to look at is a PCA? the first component should be related to the diagonal, I guess...
  #PCA <- prcomp(~ initialdirection_mean + arrowdirection_mean, data=df[-outliers,])
  
}



# Code graveyard -----

# coordinateANOVAs <- function() {
#   
#   df <- summarizeTraceBoundsV4()
#   df$externalspeed[which(df$externalspeed == 0.125)] <- 4
#   df$externalspeed[which(df$externalspeed == 0.167)] <- 3
#   
#   dfAOV <- df
#   dfAOV$internalspeed <- as.factor(dfAOV$internalspeed)
#   dfAOV$externalspeed <- as.factor(dfAOV$externalspeed)
#   
#   cat('**\n** ANOVA predicting reset X coordinates from internal and external speed:\n**\n\n')
#   print(ez::ezANOVA(dv=boundX_mean, wid=participant, data=dfAOV, within=c(internalspeed, externalspeed), type=3))
#   
#   cat('**\n** ANOVA predicting reset Y coordinates from internal and external speed:\n**\n\n')
#   print(ez::ezANOVA(dv=boundY_mean, wid=participant, data=dfAOV, within=c(internalspeed, externalspeed), type=3))
#   
# }

# resetAngleANOVA <- function() {
#   
#   df <- summarizeTraceBoundsV4()
#   df$externalspeed[which(df$externalspeed == 0.125)] <- 4
#   df$externalspeed[which(df$externalspeed == 0.167)] <- 3
#   
#   dfAOV <- df
#   dfAOV$internalspeed <- as.factor(dfAOV$internalspeed)
#   dfAOV$externalspeed <- as.factor(dfAOV$externalspeed)
#   
#   cat('**\n** ANOVA predicting reset point angle from internal and external speed:\n**\n\n')
#   print(ez::ezANOVA(dv=initialdirection_mean, wid=participant, data=dfAOV, within=c(internalspeed, externalspeed), type=3))
#   
# }
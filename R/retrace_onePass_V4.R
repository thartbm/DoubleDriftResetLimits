library('svglite')
library('ez')

source('R/common.R')
#source('R/models.R')

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
    illusionstrength <- c()
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
              illusionstrength <- c(illusionstrength, 90 - percept)
              boundX <- c(boundX, NA)
              boundY <- c(boundY, NA)
              
            } else {
              
              step <- list('track'=2,'re-trace'=99)[[task]]
              
              step.idx <- which(trialdf$step == step)
              
              # x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0) * 0.6
              # y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5) * 0.6
              
              x <- ((trialdf$handx_pix[step.idx] / (524)) + 0.0)
              y <- ((trialdf$handy_pix[step.idx] / (524)) + 0.5)
              x <- x - x[1]
              y <- y - y[1]
              prop <- max(y)
              x <- x / prop
              y <- y / prop
              # x <- x
              # y <- y
              t <- trialdf$time_ms[step.idx]
              
              # if (trial == 2 & participant == 1) {
              #   print(step.idx)
              #   # print(x)
              #   # print(y)
              # }
              
              if (trialdf$internalMovement[1] < 0) x <- -x
              
              # this has to be redone once the reset point is found!
              point <- which(sqrt(x^2 + y^2) > 0.15)[1]
              percept <- (atan2(y[point], x[point]) / pi) * 180
              initialdirection <- c(initialdirection, 90 - percept)
              # IDcoords <- c(x[point],y[point])
              
              # why do this and also the smoothed spline?
              # to test if we should be doing the splines at all?
              # and the splines find the "more correct" point... hmmm...
              boundary <- which(diff(x) < 0)[1]
              
              if (is.na(boundary)) {
                lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
                
                boundX <- c(boundX, NA)
                boundY <- c(boundY, NA)
                illusionstrength <- c(illusionstrength, NA)
                
              } else {
                
                #lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
                #lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
                # lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
                
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
                  
                  #   points(x[localmaxima[1]]+participant-0.5,IM.idx-0.1,col='#FF0000')
                  #   points(participant-0.7,y[localmaxima[1]]+IM.idx-0.9,col='#FF0000')
                  
                  # store the first local maximum as reset point:
                  # Xbounds <- c(Xbounds, x[localmaxima[1]])
                  # Ybounds <- c(Ybounds, y[localmaxima[1]])
                  
                  boundX <- c(boundX, x[localmaxima[1]])
                  boundY <- c(boundY, y[localmaxima[1]])
                  
                  # **********************************
                  # ACTUAL HALFWAY POINTS HERE:
                  #print(sqrt(sum(IDcoords^2)))
                  hwd <- sqrt(sum(c(x[localmaxima[1]],y[localmaxima[1]])^2)) / 2
                  hwp <- which(sqrt(x^2 + y^2) > hwd)[1]
                  hwpercept <- (atan2(y[hwp], x[hwp]) / pi) * 180
                  illusionstrength <- c(illusionstrength, 90 - hwpercept)
                  
                  #print(percept - hwpercept)
                  
                  points(x[localmaxima[1]]+(participant.idx*2)-2+EM.idx,y[localmaxima[1]]+IM.idx-0.9,col='#b400e4ff')
                  
                  boundary <- localmaxima[1]
                  lines(x[1:boundary]+(participant.idx*2)-2+EM.idx,y[1:boundary]+IM.idx-0.9,col='#b400e4ff')
                  lines(x[boundary:length(x)]+(participant.idx*2)-2+EM.idx,y[boundary:length(x)]+IM.idx-0.9,col='#CCCCCC33')
                  
                  
                  
                } else {
                  lines(x+(participant.idx*2)-2+EM.idx,y+IM.idx-0.9,col='#66666699')
                  boundX <- c(boundX, NA)
                  boundY <- c(boundY, NA)
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
    
    df <- data.frame(participant, trial, internalspeed, internaldirection, externalspeed, fixationside, initialdirection, illusionstrength, boundX, boundY)
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
  
  # tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  # aggdf['initialdirection_mean'] <- tempdf$initialdirection
  # 
  # tempdf <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  # aggdf['initialdirection_sd'] <- tempdf$initialdirection
  tempdf <- aggregate(illusionstrength ~ participant + internalspeed + externalspeed, data=df, FUN=mean, na.rm=TRUE)
  aggdf['initialdirection_mean'] <- tempdf$illusionstrength
  
  tempdf <- aggregate(illusionstrength ~ participant + internalspeed + externalspeed, data=df, FUN=sd, na.rm=TRUE)
  aggdf['initialdirection_sd'] <- tempdf$illusionstrength
  
  tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=mean, na.rm=TRUE)
  aggdf['arrowdirection_mean'] <- tempdf2$initialdirection
  
  tempdf2 <- aggregate(initialdirection ~ participant + internalspeed + externalspeed, data=df2, FUN=sd, na.rm=TRUE)
  aggdf['arrowdirection_sd'] <- tempdf2$initialdirection
  
  return(aggdf)
  
}




# Figures -----



plotData <- function(target='inline') {
  
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
  }
  
  par(mar=c(3.5, 3.5, 2.5, 0.5))
  
  colors <- getColors()
  
  # there will be three plots:
  # 1: example participant
  # 2: 2D overview of average reset points
  # 3: Cavangh & Tse comparison
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T), widths=c(1.2,1,1.7))
  
  # **********************************
  # EXAMPLE PARTICIPANT PLOT:
  
  plot(-1000,-1000,
       main='participant 4, 3 cps, 4 s',xlab='',ylab='',
       xlim=c(-.25,.5), ylim=c(-.1,.9),
       bty='n',ax=F, asp=1)
  
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
  
  axis(side=1,at=c(0,2,4)/13.5,labels=c('0','2','4'))
  axis(side=2,at=c(0,4,8,12)/13.5,labels=c('0','4','8','12'))
  
  
  # ***********************************************
  # 2D overview of reset points:
  
  #df <- summarizeTraceBoundsV4()
  
  df <- getData()
  
  
  # PANEL A: raw spatial coordinates:
  
  # scatter of reset points
  plot(df$X, df$Y, main='reset point coordinates', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  title(xlab='horizontal reset coordinate [cm]', line=2.5)
  title(ylab='vertical reset coordinate [cm]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(df$X)
  text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(df$Y)
  text(6,medY+0.5,sprintf('%0.1f cm',medY))
  lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(-2,0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_cm <- seq(0,13.5,length.out = 4)
  axis(side=2, at=ytick_cm, labels=ytick_cm)
  
  # ***********************************************************
  # COMPARISON WITH CAVANAGH & TSE (2019) DATA / MODEL
  
  colors <- getColors()
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  
  
  #df <- getTimeNormalizedData(illusionMinimum = 0)
  
  df <- getData()
  
  avg_df <- aggregate(angle ~ Vi + speed, data=df, FUN=mean)
  
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
       xlim=c(0,5*(pi/12)),ylim=c(0,45),
       bty='n',ax=F)
  
  title(xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')), line=2.5)
  title(ylab='illusion strength [°]', line=2.5)
  
  angles <- seq(0,pi/2,.05)
  lines(angles,(angles/pi)*180,col='gray',lty=2)
  lines(angles,0.81*((angles/pi)*180),col='black',lty=1)
  
  #xcoords <- atan(internalspeed / externalspeed)
  
  # get the best k for this data:
  X <- (xcoords/pi)*180
  Y <- 90 - ((as.numeric(unlist(df$angle))/pi)*180)
  linmod <- lm(Y ~ X - 1)
  slope <- summary(linmod)$coefficients['X','Estimate']
  
  # plot that as a line:
  lines(angles,slope*((angles/pi)*180),col=colors$purple$s,lty=1)
  
  # for (ppno in unique(df$participant)) {
  #   
  #   idx <- which(df$participant == ppno)
  #   
  #   X <- (xcoords[idx]/pi)*180
  #   Y <- as.numeric(unlist(df$initialdirection_mean[idx]))
  #   linmod <- lm(Y ~ X - 1)
  #   slope <- summary(linmod)$coefficients['X','Estimate']
  #   cat(sprintf('participant: %d: %0.2f\n', ppno, slope))
  #   
  # }
  
  points(xcoords[idxE3], 90 - ((df$angle[idxE3]/pi)*180), col=colors$blue$t, pch=16)
  points(xcoords[idxE4], 90 - ((df$angle[idxE4]/pi)*180), col=colors$yorkred$t, pch=16)
  
  points(avg_xcoords[avg_idxE3], 90 - ((avg_df$angle[avg_idxE3]/pi)*180), col=colors$blue$s, pch=1)
  points(avg_xcoords[avg_idxE4], 90 - ((avg_df$angle[avg_idxE4]/pi)*180), col=colors$yorkred$s, pch=1)
  
  legend(x=0, y=45, 
         legend = c('9 participants (3 s)', '9 participants (4 s)', 'group averages', 'K=1', 'K=0.81', sprintf('K=%0.2f',slope)), 
         pch=c(16,16,1,NA,NA,NA), col=c(colors$blue$t, colors$yorkred$t, 'black', 'gray', 'black', colors$purple$s), 
         lty = c(0,0,0,2,1,1),
         bty='n')
  
  #axis(side=1,at=seq(0,3*(pi/8),pi/8),labels=c('0',expression(pi/8),expression(pi/4),expression(3*pi/8)))
  axis(side=1,at=seq(0,5*(pi/12),pi/12),labels=sprintf('%d',seq(0,75,15)))
  axis(side=2,at=seq(0,45,15))
  

  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


plotNData <- function(target='inline') {
  
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
  }
  
  par(mar=c(3.5, 3.5, 2.5, 0.5))
  
  colors <- getColors()
  
  # there will be three plots:
  # 1: example participant
  # 2: 2D overview of average reset points
  # 3: Cavangh & Tse comparison
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T))
  
  
  
  # ***********************************************
  # 2D overview of reset points:
  
  #df <- summarizeTraceBoundsV4()
  
  df <- getData()
  
  
  # PANEL A: raw spatial coordinates:
  
  # scatter of reset points
  plot(-1000, -1000, main='participant X condition [N=54]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  idxE4 <- which(df$speed > (13.5/3.5))
  idxE3 <- which(df$speed < (13.5/3.5))
  
  points(df$X[idxE4], df$Y[idxE4], pch=1,  col=colors[['purple']]$s)
  points(df$X[idxE3], df$Y[idxE3], pch=19, col=colors[['purple']]$s)
  
  title(xlab='horizontal reset coordinate [cm]', line=2.5)
  title(ylab='vertical reset coordinate [cm]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(df$X)
  text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(df$Y)
  text(6,medY+0.5,sprintf('%0.1f cm',medY))
  lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(-2,0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_cm <- seq(0,13.5,length.out = 4)
  axis(side=2, at=ytick_cm, labels=ytick_cm)
  
  # B: condition
  
  cdf <- aggregate(cbind(X,Y) ~ Vi + Ve + speed, data=df, FUN=mean)
  
  
  # scatter of reset points
  plot(-1000,-1000, main='condition [N=6]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  idxE4 <- which(cdf$speed > (13.5/3.5))
  idxE3 <- which(cdf$speed < (13.5/3.5))
  
  points(cdf$X[idxE4], cdf$Y[idxE4], pch=1,  col=colors[['purple']]$s)
  points(cdf$X[idxE3], cdf$Y[idxE3], pch=19, col=colors[['purple']]$s)
  
  title(xlab='horizontal reset coordinate [cm]', line=2.5)
  title(ylab='vertical reset coordinate [cm]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  #segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  #segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(cdf$X)
  text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(cdf$Y)
  text(6,medY+0.5,sprintf('%0.1f cm',medY))
  lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(-2,0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_cm <- seq(0,13.5,length.out = 4)
  axis(side=2, at=ytick_cm, labels=ytick_cm)
  
  
  
  
  
  
  
  
  # C: participant averages

  ppdf <- aggregate(cbind(X,Y,RT) ~ participant + speed, data=df, FUN=mean)
  
  # scatter of reset points
  plot(-1000, -1000, main='participant [N=9]', asp=1, xlim=c(-1, 6), ylim=c(-1, 14.5), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  
  idxE4 <- which(ppdf$speed > (13.5/3.5))
  idxE3 <- which(ppdf$speed < (13.5/3.5))
  
  points(ppdf$X[idxE4], ppdf$Y[idxE4], pch=1,  col=colors[['purple']]$s)
  points(ppdf$X[idxE3], ppdf$Y[idxE3], pch=19, col=colors[['purple']]$s)
  
  
  
  title(xlab='horizontal reset coordinate [cm]', line=2.5)
  title(ylab='vertical reset coordinate [cm]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,13.5,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(ppdf$X)
  text(x=medX+0.1,y=1.05,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.25,10.5),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(ppdf$Y)
  text(6,medY+0.5,sprintf('%0.1f cm',medY))
  lines(c(-0.5,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(-2,0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_cm <- seq(0,13.5,length.out = 4)
  axis(side=2, at=ytick_cm, labels=ytick_cm)
  
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}




plotNRTData <- function(target='inline') {
  
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/Fig4_trajectories_resetpoints_illusionstrength.pdf',onefile=TRUE,width=8,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/Fig4_trajectories_resetpoints_illusionstrength.svg',width=8,height=4)
  }
  
  par(mar=c(3.5, 3.5, 2.5, 0.5))
  
  colors <- getColors()
  
  # there will be three plots:
  # 1: example participant
  # 2: 2D overview of average reset points
  # 3: Cavangh & Tse comparison
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T))
  
  
  
  # ***********************************************
  # 2D overview of reset points:
  
  #df <- summarizeTraceBoundsV4()
  
  df <- getData()
  
  
  # PANEL A: raw spatial coordinates:
  
  # scatter of reset points
  plot(-1000, -1000, main='participant X condition [N=54]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  idxE4 <- which(df$speed > (13.5/3.5))
  idxE3 <- which(df$speed < (13.5/3.5))
  
  points(df$X[idxE4], df$RT[idxE4], pch=1,  col=colors[['purple']]$s)
  points(df$X[idxE3], df$RT[idxE3], pch=19, col=colors[['purple']]$s)
  
  title(xlab='reset offset [cm]', line=2.5)
  title(ylab='reset time [s]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(df$X)
  text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(df$RT)
  text(4.2,medY-0.1,sprintf('%0.1f s',medY))
  lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_s <- c(0:4)
  axis(side=2, at=ytick_s, labels=ytick_s)
  
  # B: condition
  
  cdf <- aggregate(cbind(X,Y,RT) ~ Vi + Ve + speed, data=df, FUN=mean)
  
  
  # scatter of reset points
  plot(-1000,-1000, main='condition [N=6]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  idxE4 <- which(cdf$speed > (13.5/3.5))
  idxE3 <- which(cdf$speed < (13.5/3.5))
  
  points(cdf$X[idxE4], cdf$RT[idxE4], pch=1,  col=colors[['purple']]$s)
  points(cdf$X[idxE3], cdf$RT[idxE3], pch=19, col=colors[['purple']]$s)
  
  title(xlab='reset offset [cm]', line=2.5)
  title(ylab='reset time [s]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  #segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  #segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(cdf$X)
  text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(cdf$RT)
  text(4.2,medY-0.1,sprintf('%0.1f s',medY))
  lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  
  legend(medX+.2,4,c('4 s passes','3 s passes'),pch=c(1,19), col=colors[['purple']]$s, bty='n')
  
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_s <- c(0:4)
  axis(side=2, at=ytick_s, labels=ytick_s)
  
  
  
  
  
  
  
  
  # C: participant averages
  
  ppdf <- aggregate(cbind(X,Y,RT) ~ participant + speed, data=df, FUN=mean)
  
  # scatter of reset points
  plot(-1000, -1000, main='participant [N=9 or N=18?]', xlim=c(-1, 6), ylim=c(-.25, 4.25), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  
  idxE4 <- which(ppdf$speed > (13.5/3.5))
  idxE3 <- which(ppdf$speed < (13.5/3.5))
  
  points(ppdf$X[idxE4], ppdf$RT[idxE4], pch=1,  col=colors[['purple']]$s)
  points(ppdf$X[idxE3], ppdf$RT[idxE3], pch=19, col=colors[['purple']]$s)
  
  
  
  title(xlab='reset offset [cm]', line=2.5)
  title(ylab='reset time [s]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  # segments(x0=df$X-df$X.sd, y0=df$Y, x1=df$X+df$X.sd, y1=df$Y, col=colors[['purple']]$t)
  # segments(x0=df$X, y0=df$Y-df$Y.sd, x1=df$X, y1=df$Y+df$Y.sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,4,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(ppdf$X)
  text(x=medX+0.025,y=.2,labels=sprintf('%0.1f cm',medX),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.2,3.75),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(ppdf$RT)
  text(4.2,medY-0.1,sprintf('%0.1f s',medY))
  lines(c(0.25,4.5),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(0,2,4)
  axis(side=1, at=xtick_cm, labels=xtick_cm)
  ytick_s <- c(0:4)
  axis(side=2, at=ytick_s, labels=ytick_s)
  
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}







plotModels <- function(target='inline') {
  
  colors <- getColors()
  
  if (target == 'pdf') {
    cairo_pdf(filename='model_figure.pdf',onefile=TRUE,width=8,height=5.25)
  }
  if (target == 'svg') {
    svglite(file='model_figure.svg',width=8,height=5.25)
  }
  
  par(mar=c(3.4, 3.4, 2.1, 3.75))
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = T), widths = c(1,1,1))
  
  #df <- getTimeNormalizedData()
  df <- getData()
  
  # ***************************************
  # PLOT SINGLE LIMIT MODELS: TIME LIMIT
  
  fit <- fitSingleLimitModels(df=df)
  
  directions <- c(10:40)
  raddirections <- ((90 - directions) / 180) * pi
  slope <- sin(raddirections) / cos(raddirections)
  
  # PANEL B: x coords from time limit
  
  #plot(df$initialdirection_mean, df$boundX_mean*13.5, main=sprintf('time limit (%0.1f s)', fit$par['Ly']*(4/13.5)), xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,10), col=colors[['blue']]$s)
  plot(90-((df$angle/pi)*180), df$X, col=colors[['blue']]$s, 
       main='time limit', xlab='', ylab='', 
       xlim=c(5,45), ylim=c(0,13.5), 
       bty='n', ax=F)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
  
  #lines(x=range(directions),y=rep(fit$par['Lx'],2),col=colors[['yorkred']]$s,lty=1)
  
  #fittedX <- resetXfromYlim(fit$par,slopes)$X
  #lines(directions,fittedX,col=colors[['blue']]$s)
  
  sin.a <- sin(raddirections)
  cos.a <- cos(raddirections)
  
  data <- data.frame(sin.a, cos.a, slope)
  
  for (spd in c(3.375, 4.5)) {
    data$speed <- spd
    fittedX <- YlimResets(fit$Ylim$par,data=data)
    lines(directions,fittedX$X,col=colors[['blue']]$s)
  }
  
  # median models
  # XfromMedY <- resetXfromYlim(c('Ly'=medY*13.5),slopes)$X
  # lines(directions,XfromMedY,col=colors[['blue']]$s, lty=2)
  
  legend(15,10,c('reset distance','reset time'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset coordinate:',bty='n')
  
  
  axis(side=1, at=c(10,20,30,40))
  axis(side=2, at=c(0,2,4,6,8,10,12),las=1)
  
  # ******************************************
  # SINGLE LIMIT MODEL: SPACE LIMIT
  
  # PANEL C: Y coords from space limit
  
  #plot(df$initialdirection_mean, df$boundY_mean*4, main=sprintf('space limit (%0.1f cm)', fit$par['Lx']), xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,4), col=colors[['yorkred']]$s)
  plot(90-((df$angle/pi)*180), df$RT, col=colors[['yorkred']]$s,
       main='space limit', xlab='', ylab='',
       xlim=c(5,45), ylim=c(0,4), 
       bty='n', ax=F)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='reset time [s]', line=2.4, cex=0.8)
  
  #print( (fit$par['Ly']/13.5)*4 )
  #lines(x=range(directions),y=rep((fit$par['Ly']/13.5)*4,2),col=colors[['blue']]$s)
  
  # fittedY <- resetYfromXlim(fit$par,slopes)$Y
  # lines(directions,fittedY/4,col=colors[['yorkred']]$s)
  
  
  fittedY <- XlimResets(fit$Xlim$par,data=data)
  for (spd in c(3.375,4.5)) {
    resetTime <- sqrt(fittedY$Y^2 + fittedY$X^2) / spd
    lines(directions,resetTime,col=colors[['yorkred']]$s)
  }
  
  
  
  # median models
  # YfromMedX <- resetYfromXlim(c('Lx'=medX*4),slopes)$Y
  # lines(directions,YfromMedX,col=colors[['yorkred']]$s, lty=2)
  
  axis(side=1, at=c(10,20,30,40))
  axis(side=2, at=c(0,1,2,3,4),las=1)
  
  # ***********************************************
  # COMBINED LIMIT MODEL
  
  plot(-1000,-1000,main='combined limits',
       xlab='',ylab='',
       xlim=c(5,45),ylim=c(0,1),
       bty='n',ax=F)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
  
  points(90-((df$angle/pi)*180),df$X/13.5,col=colors[['blue']]$s)
  points(90-((df$angle/pi)*180),df$RT/4,col=colors[['yorkred']]$s)
  
  
  #par <- fitResetModelSeq(slopes=df$slope, X=df$boundX_mean*13.5, Y=df$boundY_mean*13.5)
  fit <- fitTwoLimitModel(df)
  
  for (spd in c(3.375, 4.5)) {
    data$speed <- spd
    fitted <- twoLimResets(par=fit$par, data=data)
    lines(directions,fitted$X/13.5,col=colors[['blue']]$s)
    resetTime <- sqrt(fitted$Y^2 + fitted$X^2) / spd
    lines(directions,resetTime/4,col=colors[['yorkred']]$s)
  }
  
  #legend(25,0.8,c('X','Y'),col=c(colors[['blue']]$s, colors[['yorkred']]$s), lty=c(1,1), title='reset\ncoordinate:',bty='n')
  
  axis(1,at=seq(10,40,10))
  axis(2,at=seq(0,12,2)/13.5,labels=c('0','2','4','6','8','10','12'),las=1)
  axis(4,at=seq(0,4,1)/4,labels=c('0','1','2','3','4'),las=1)
  
  mtext('reset time [s]',side=4,line=2,cex=0.7)
  
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
  
}

# Statistics -----



# Code graveyard -----


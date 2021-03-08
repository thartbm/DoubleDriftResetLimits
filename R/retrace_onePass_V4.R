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
  
  df <- getTimeNormalizedData()
  
  # PANEL A: raw spatial coordinates:
  
  # scatter of reset points
  plot(df$boundX_mean, df$boundY_mean, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.45), ylim=c(-0.1, 1.35), bty='n', ax=F, xlab='', ylab='', col=colors[['purple']]$s)
  
  title(xlab='horizontal reset distance [cm]', line=2.5)
  title(ylab='reset time [s]',  line=2.5)
  #plot(-1000, -1000, main='reset point coordinates', asp=1, xlim=c(-0.15, 0.35), ylim=c(-0.1, 1.1), bty='n', ax=F, xlab='X coordinate [cm]', ylab='Y coordinate [s]', col=colors[['purple']]$s)
  
  
  
  segments(x0=df$boundX_mean-df$boundX_sd, y0=df$boundY_mean, x1=df$boundX_mean+df$boundX_sd, y1=df$boundY_mean, col=colors[['purple']]$t)
  segments(x0=df$boundX_mean, y0=df$boundY_mean-df$boundY_sd, x1=df$boundX_mean, y1=df$boundY_mean+df$boundY_sd, col=colors[['purple']]$t)
  
  # path of gabor:
  #lines(c(0,0),c(0,1),col='#999999', lw=2, lty=1)
  arrows(0,0,0,1,length=0.2,col='#999999',lwd=2,angle=20)
  
  # X coordinates:
  medX <- mean(df$boundX_mean)
  text(x=medX+0.01,y=1.05,labels=sprintf('%0.1f cm',medX*13.5),pos=4,adj=c(1,0))
  lines(rep(medX,2),c(0.025,1.05),col=colors[['blue']]$s,lty=2,lw=2)
  
  # Y coordinates:
  medY <- mean(df$boundY_mean)
  text(6/13.5,medY+0.05,sprintf('%0.1f s',medY*4))
  lines(c(-0.05,0.45),rep(medY,2),col=colors[['yorkred']]$s,lty=2,lw=2)
  
  # sensible tick marks on the axes:
  # xtick_cm <- c(-1,1,3)
  xtick_cm <- c(-2,0,2,4)
  axis(side=1, at=xtick_cm/13.5, labels=xtick_cm)
  ytick_s <- seq(0,5,length.out = 6)
  axis(side=2, at=ytick_s/4, labels=ytick_s)
  
  # ***********************************************************
  # COMPARISON WITH CAVANAGH & TSE (2019) DATA / MODEL
  
  colors <- getColors()
  solids <- list('2'=colors[['purple']]$s, '3'=colors[['yorkred']]$s, '4'=colors[['orange']]$s)
  transp <- list('2'=colors[['purple']]$t, '3'=colors[['yorkred']]$t, '4'=colors[['orange']]$t)
  
  
  df <- getTimeNormalizedData(illusionMinimum = 0)
  
  avg_df <- aggregate(cbind(arrowdirection_mean, initialdirection_mean) ~ internalspeed + externalspeed, data=df, FUN=mean)
  
  internalspeed <- df$internalspeed / 0.58 # in cm/s 
  externalspeed <- 13.5 / df$externalspeed # in cm/s
  
  xcoords <- atan(internalspeed / externalspeed)
  
  
  
  avg_xcoords <- atan((avg_df$internalspeed / 0.58) / (13.5 / avg_df$externalspeed))
  
  idxE3 <- which(df$externalspeed == 3)
  idxE4 <- which(df$externalspeed == 4)
  
  avg_idxE3 <- which(avg_df$externalspeed == 3)
  avg_idxE4 <- which(avg_df$externalspeed == 4)
  
  plot(-1000,-1000,
       main='illusion strength',xlab='',ylab='',
       xlim=c(0,5*(pi/12)),ylim=c(0,45),
       bty='n',ax=F)
  
  title(xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')), line=2.5)
  title(ylab='illusion strength [°]', line=2.5)
  
  angles <- seq(0,pi/2,.05)
  lines(angles,(angles/pi)*180,col='gray',lty=2)
  lines(angles,0.81*((angles/pi)*180),col='black',lty=1)
  
  xcoords <- atan(internalspeed / externalspeed)
  
  # get the best k for this data:
  X <- (xcoords/pi)*180
  Y <- as.numeric(unlist(df$initialdirection_mean))
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
  
  points(xcoords[idxE3], df$initialdirection_mean[idxE3], col=colors$blue$t, pch=16)
  points(xcoords[idxE4], df$initialdirection_mean[idxE4], col=colors$yorkred$t, pch=16)
  
  points(avg_xcoords[avg_idxE3], avg_df$initialdirection_mean[avg_idxE3], col=colors$blue$s, pch=1)
  points(avg_xcoords[avg_idxE4], avg_df$initialdirection_mean[avg_idxE4], col=colors$yorkred$s, pch=1)
  
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
  
  df <- getTimeNormalizedData()
  
  # ***************************************
  # PLOT SINGLE LIMIT MODELS: TIME LIMIT
  
  fit <- fitSeparateXYresetModels(directions=df$initialdirection_mean,
                                  X=df$boundX_mean * 13.5,
                                  Y=df$boundY_mean * 13.5)
  
  directions <- c(10:40)
  raddirections <- ((90 - directions) / 180) * pi
  slopes <- sin(raddirections) / cos(raddirections)
  
  # PANEL B: x coords from time limit
  
  #plot(df$initialdirection_mean, df$boundX_mean*13.5, main=sprintf('time limit (%0.1f s)', fit$par['Ly']*(4/13.5)), xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,10), col=colors[['blue']]$s)
  plot(df$initialdirection_mean, df$boundX_mean*13.5, main='time limit', xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,13.5), col=colors[['blue']]$s)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
  
  #lines(x=range(directions),y=rep(fit$par['Lx'],2),col=colors[['yorkred']]$s,lty=1)
  
  fittedX <- resetXfromYlim(fit$par,slopes)$X
  lines(directions,fittedX,col=colors[['blue']]$s)
  
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
  plot(df$initialdirection_mean, df$boundY_mean*4, main='space limit', xlab='', ylab='', bty='n', ax=F, xlim=c(5,45), ylim=c(0,4), col=colors[['yorkred']]$s)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='reset time [s]', line=2.4, cex=0.8)
  
  #print( (fit$par['Ly']/13.5)*4 )
  #lines(x=range(directions),y=rep((fit$par['Ly']/13.5)*4,2),col=colors[['blue']]$s)
  
  fittedY <- resetYfromXlim(fit$par,slopes)$Y
  lines(directions,fittedY/4,col=colors[['yorkred']]$s)
  
  # median models
  # YfromMedX <- resetYfromXlim(c('Lx'=medX*4),slopes)$Y
  # lines(directions,YfromMedX,col=colors[['yorkred']]$s, lty=2)
  
  axis(side=1, at=c(10,20,30,40))
  axis(side=2, at=c(0,1,2,3,4),las=1)
  
  # ***********************************************
  # COMBINED LIMIT MODEL
  
  
  #colors <- getColors()
  
  
  plot(-1000,-1000,main='combined limits',xlab='',ylab='',xlim=c(5,45),ylim=c(0,1),bty='n',ax=F)
  
  title(xlab='illusion strength [°]', line=2.4, cex=0.8)
  title(ylab='horizontal reset distance [cm]', line=2.4, cex=0.8)
  
  
  par <- fitResetModelSeq(slopes=df$slope, X=df$boundX_mean*13.5, Y=df$boundY_mean*13.5)
  
  model_angles <- seq(10,40)
  model_directions <- ((90-model_angles)/180)*pi
  model_slopes <- sin(model_directions) / cos(model_directions)
  model_resets <- resetModelSeq(par,slopes=model_slopes,verbose=TRUE)
  
  
  lines(model_angles,model_resets$X/13.5,col=colors[['blue']]$s)
  lines(model_angles,model_resets$Y/13.5,col=colors[['yorkred']]$s)
  
  points(df$initialdirection_mean,df$boundX_mean,col=colors[['blue']]$s)
  points(df$initialdirection_mean,df$boundY_mean,col=colors[['yorkred']]$s)
  
  
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


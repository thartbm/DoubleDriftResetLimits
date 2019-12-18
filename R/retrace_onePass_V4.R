library('svglite')
library('ez')


preProcessOnePass_V4 <- function(participants = c(2,3,4,5,6,8,9,10,11), target='pdf') {
  
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
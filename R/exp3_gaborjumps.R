
# do the psychophysics -----

getPSEs <- function(groups=c('exp','ctrl'), overwrite=FALSE) {
  
  for (group in groups) {
    
    # check if the file is already there, skip if it is:
    if (file.exists(sprintf('data/gaborjump/%s_PSEs.csv', group)) & !overwrite) {
      next
    }
    
    # extracted data:
    df <- read.csv(sprintf('data/gaborjump/gaborjump_%s_data.csv',group), stringsAsFactors = FALSE)
    
    # invert leftward trials:
    ext1 <- which(df$exmotion == 1)
    df$gapshift[ext1] <- -1 * df$gapshift[ext1]
    df$response[ext1] <- -1 * df$response[ext1]
    
    # we want to scale between 0 and 1:
    df$response[which(df$response == -1)] <- 0
    
    # get mean proportion of right-ward responses per condition:
    agg_df <- aggregate(response ~ participant + path_length + stimdur + exmotion + gapshift, data=df, FUN=mean)
    
    conditions <- expand.grid('path_length'=unique(agg_df$path_length),
                              #'exmotion'=unique(agg_df$exmotion),
                              'stimdur'=unique(agg_df$stimdur)
    )
    
    
    participant <- c()
    path_length <- c()
    stimdur     <- c()
    #exmotion    <- c()
    PSE         <- c()
    std         <- c()
    
    
    # need to know the participants, so we can analyze their data separately:
    participants <- unique(agg_df$participant)
    
    # 12 PSEs per participant:
    #layout(mat=matrix(c(1:12),ncol = 4,nrow = 3, byrow = TRUE))
    
    gapshifts_df <- data.frame('gapshift'=seq(-4,4,length.out = 161))
    
    for (ppno in participants) {
      
      p_df <- agg_df[which(agg_df$participant == ppno),]
      
      for (c.row in c(1:dim(conditions)[1])) {
        c_df <- df[which(df$participant == ppno &
                           df$path_length == conditions$path_length[c.row] &
                           #df$exmotion    == conditions$exmotion[c.row] &
                           df$stimdur     == conditions$stimdur[c.row]),]
        #print(c_df)
        #model <- glm(response ~ gapshift, data=c_df, family = binomial(link="probit"))
        
        #model <- glm(response ~ gapshift, data=c_df, family = binomial(mafc.probit(2)))
        #print(summary(model))
        
        # this is for a logit link function:
        # m   <- -coef(model)[[1]]/coef(model)[[2]]
        # std <- 1/coef(model)[[2]]
        # cat(sprintf('\n\nmean: %0.3f, sd: %0.3f\n\n',m,std))
        
        #print(model$coefficients)
        
        
        a_c_df <- aggregate(response ~ gapshift, data=c_df, FUN=mean)
        #plot(a_c_df$gapshift, a_c_df$response, col='red', main='',xlab='',ylab='',xlim=c(-4,4),ylim=c(0,1))
        
        #lines(gapshifts_df$gapshift, predict(object=model, newdata=gapshifts_df))
        #print(predict(object=model, newdata=gapshifts_df))
        
        #cndfit <- fit_cndf(x=a_c_df$gapshift,y=a_c_df$response)
        cndfit <- fit_cndf(x=c_df$gapshift,y=c_df$response)
        #print(cndfit)
        #lines(seq(-4,4,length.out = 161),pnorm(q=seq(-4,4,length.out = 161),mean=cndfit['mu'],sd=cndfit['sigma']),col='blue')
        
        participant <- c(participant, ppno)
        path_length <- c(path_length, conditions$path_length[c.row])
        stimdur     <- c(stimdur, conditions$stimdur[c.row])
        #exmotion    <- c(exmotion, conditions$exmotion[c.row])
        PSE         <- c(PSE, cndfit['mu'])
        std         <- c(std, cndfit['sigma'])
        
      }
      
    }
    
    ppno <- 'group'
    
    for (c.row in c(1:dim(conditions)[1])) {
      c_df <- df[which(  df$path_length == conditions$path_length[c.row] &
                           #df$exmotion    == conditions$exmotion[c.row] &
                           df$stimdur     == conditions$stimdur[c.row]),]
      #model <- glm(response ~ gapshift, data=c_df, family = binomial(link="probit"))
      
      a_c_df <- aggregate(response ~ gapshift, data=c_df, FUN=mean)
      cndfit <- fit_cndf(x=c_df$gapshift,y=c_df$response)
      
      
      
      participant <- c(participant, ppno)
      path_length <- c(path_length, conditions$path_length[c.row])
      stimdur     <- c(stimdur, conditions$stimdur[c.row])
      #exmotion    <- c(exmotion, conditions$exmotion[c.row])
      PSE         <- c(PSE, cndfit['mu'])
      std         <- c(std, cndfit['sigma'])
      
    }
    
    
    df <- data.frame(participant,
                     path_length,
                     stimdur,
                     #                 exmotion,
                     PSE,
                     std)
    
    write.csv(x         = df, 
              file      = sprintf('data/gaborjump/%s_PSEs.csv',group),
              row.names = FALSE)
  }
}

library('optimx')

fit_cndf <- function(x,y) {
  
  # define search grip points:
  mu=seq(min(x), max(x), length.out = 41)
  sigma=seq(.4, diff(range(x))*2, length.out = 41)
  
  # make search grid data frames:
  searchgrid <- expand.grid('mu'=mu, 'sigma'=sigma)

  # get error for each point in the grid:
  MSEs <- apply(searchgrid,MARGIN=c(1),FUN=cdf_err,x=x,y=y)
  
  # get some of the best points in the grid:
  topgrid <- searchgrid[order(MSEs)[c(1,5,9)],]
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=cdf_err,
                            method='L-BFGS-B',
                            lower=c(min(x),.2),
                            upper=c(max(x),diff(range(x))),
                            x=x,
                            y=y) )
  
  # pick the best fit:
  optfit <- allFits[order(allFits$value, decreasing = TRUE)[1],]
  
  # we're only interested in the distribution moments for now:
  return(c('mu'=optfit$mu,'sigma'=optfit$sigma))
  
}

cdf_err <- function(par, x, y) {
  
  return( mean( (y - pnorm(q=x, mean=par['mu'], sd=par['sigma']))^2 ) )
  
}

# stats on psychophsics -----

library('ez')

ANOVAonPSEs <- function() {
  
  for (group in c('exp','ctrl')) {
    
    df <- read.csv(sprintf('data/gaborjump/%s_PSEs.csv',group), stringsAsFactors = FALSE)
    
    df <- df[which(df$participant != 'group'),]
    
    df$participant <- as.factor(df$participant)
    df$path_length <- as.factor(df$path_length)
    df$stim_dur <- as.factor(df$stimdur)
    
    EA <- ez::ezANOVA( data = df,
                       dv = PSE,
                       wid = participant,
                       within = c(path_length, stim_dur),
                       detailed=TRUE)
    
    # add partial eta squared column:
    EA <- ezANOVA.pes(EA)
    print(EA)
    
  }
  
}

# figures -----

plotJumpPSEs <- function(target='inline', individualDots=FALSE, modelDots=FALSE, gammaDots=FALSE, modelLine=FALSE, gammaLine=FALSE) {
  
  colors <- getColors()
  
  if (target == 'pdf') {
    pdf( file = 'doc/fig_6_gaborjump_exp.pdf', width=6, height=4 )
  }
  
  layout(mat = matrix(c(1,2), 
                      ncol  = 2,
                      nrow  = 1,
                      byrow = TRUE),
         widths = c(1.125,1.1))
  
  methodsFig <- magick::image_read_pdf('doc/methods_fig_3.pdf')
  
  # extract width and height properties:
  width <- magick::image_info(methodsFig)[['width']]
  height <- magick::image_info(methodsFig)[['height']]
  
  par(mar=c(1,1,2.2,1))
  
  # 1 data, 2 individual dots, 3 model fits
  pos <- c(0,0,0)
  
  if (individualDots & !modelDots) {
    pos <- c(0.2, -0.2, 0) # Plot OffSet
  }
  if (!individualDots & modelDots) {
    pos <- c(-0.2, 0, 0.2)
  }
  if (individualDots & modelDots) {
    pos <- c(0, -0.3, 0.3)
  }
  
  plot(methodsFig,xlim=c(0,width),ylim=c(0,height),asp=1)
  title('A: methods', font.main=1, cex.main=1.5, adj=0, line=0.5)
  
  PSEdf <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
  groupPSEs <- PSEdf[which(PSEdf$participant == 'group'),]
  participantPSEs <- PSEdf[which(PSEdf$participant != 'group'),]
  
  modelPSEs <- read.csv('data/gaborjump/modelfitPSEs.csv', stringsAsFactors = FALSE)
  
  par(mar = c(2.8,2.8,2.2,0.1),
      mgp = c(2, 0.55, 0) )
  
  plot(-1000,-1000,main='',xlab='',ylab='',
       xlim=c(0,8),ylim=c(0,3.5),
       bty='n',ax=F)
  
  title(main='B: results', font.main=1, cex.main=1.5, adj=0, line=0.5)
  title(xlab='stimulus duration [s]', line=1.6, cex.lab=0.7)
  title(ylab='gap-evoked reset [dva]', line=1.6, cex.lab=0.7)
  
  for (pathlength in c(2,4)) {
    
    xoff <- (pathlength-2)*2
    
    lines(c(0.5,3.5)+xoff,rep(pathlength/sqrt(2),2),col='#000000',lty=2)
    
    text( x=3.5+xoff, 
          y=(pathlength/sqrt(2))+0.14, 
          sprintf('%0.1f dva', pathlength/sqrt(2)), 
          adj=c(1,1),
          cex=0.7, 
          col='#000000'  )
    
    cond_pch <- c(16,16)[pathlength/2] 
    lc <- c()
    
    for (stimdur in c(1,2,3)) {
      
      idx <- which(groupPSEs$path_length == pathlength & groupPSEs$stimdur == stimdur)
      mu <- groupPSEs$PSE[idx]
      sigma <- groupPSEs$std[idx]
      
      #color <- colors[[ c('orange', 'yorkred', 'purple')[stimdur] ]]$s
      #color <- colors[[ c('orange', 'orange', 'orange')[stimdur] ]]$s
      color <- colors$orange$s
      lc <- c(lc, color)
      
      pPSEs <- participantPSEs$PSE[which(participantPSEs$path_length == pathlength & participantPSEs$stimdur == stimdur)]
      
      CI <- getConfidenceInterval(data=pPSEs, method = 'b')
      lines(rep(stimdur+pos[1],2)+xoff,CI,col=color,lty=1,lw=1)
      points(stimdur+pos[1]+xoff,mu,col=color,pch=cond_pch,cex=1)
      if (individualDots) {
        points(rep(stimdur+pos[2],length(pPSEs))+xoff,pPSEs,col=t_col(color,percent=75),pch=cond_pch,cex=1)
      }
      
      # modelPSEs
      idx <- which(modelPSEs$path_length == pathlength & modelPSEs$duration == stimdur)
      if (modelDots) {
        fitPSE <- modelPSEs$fitPSE[idx]
        points(stimdur+pos[3]+xoff,fitPSE,col=mixCol('#FFFFFF', color, balance=c(2,1)),pch=18,cex=1.5)
      }
      if (gammaDots) {
        gammaFit <- modelPSEs$gamma[idx]
        points(stimdur+pos[3]+xoff,gammaFit,col=color,pch=18,cex=1)
      }
      
      text(x=2+xoff,
           y=0,
           sprintf('%d dva path',pathlength),
           cex=0.9,
           col='#999999')
      
    }
    
    if (modelLine) {
      idx <- which(modelPSEs$path_length == pathlength)
      lines(c(1,2,3)+pos[1]+xoff,modelPSEs$fitPSE[idx],col=colors$yorkred$s, lty=3)
    }
    if (gammaLine) {
      idx <- which(modelPSEs$path_length == pathlength)
      lines(c(1,2,3)+pos[1]+xoff,modelPSEs$gamma[idx],col=colors$purple$s, lty=3)
    }
    
    
    
    if (pathlength == 2) {
      legend <- c('without spontaneous resets','mean PSE + 95% CI')
      col    <- c('#000000',colors$orange$s)
      lty    <- c(2,1)
      pch    <- c(NA,16)
      if (modelDots) {
        legend <- c(legend, 'Poisson model')
        col    <- c(col, '#AAAAAA')
        lty    <- c(lty,0)
        pch    <- c(pch,18)
      }
      if (gammaDots) {
        legend <- c(legend, 'gamma model')
        col    <- c(col, '#666666')
        lty    <- c(lty,0)
        pch    <- c(pch,18)
      }
      if (modelLine) {
        legend <- c(legend, 'Poisson model')
        col    <- c(col, colors$yorkred$s)
        lty    <- c(lty,3)
        pch    <- c(pch,NA)
      }
      if (gammaLine) {
        legend <- c(legend, 'gamma model')
        col    <- c(col, colors$purple$s)
        lty    <- c(lty,3)
        pch    <- c(pch,NA)
      }
      legend(0,3.6,
             legend=legend,
             col=col,
             lty=lty,
             pch=pch,
             cex=0.7,
             seg.len = 1.5,
             bty='n')
    } 
    
    axis(side=1,at=c(1,2,3),cex.axis=0.7,labels=c('1','2','3'))
    axis(side=1,at=c(5,6,7),cex.axis=0.7,labels=c('1','2','3'))
    axis(side=2,at=c(0.0,0.7,2.1,3.5),cex.axis=0.7)
    
  }
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}


# Sirui Lui's simplest time model ====

fitResetProbRAD <- function() {
  
  df <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
  df <- df[which(df$participant == 'group'),]
  
  conditions <- df[c('path_length', 'stimdur')]
  names(conditions) <- c('path_length', 'duration')
  conditions$PSE <- df$PSE
  
  fit <- fitResetProbability(data=conditions)
  
  return(fit)
  
}


getBestFitPlotPoints <- function(distribution='exponential') {
  
  if (distribution == 'exponential') {
    # runs with 1000 simulated stimuli per condition, per fit
    #par <- 0.007178981
    #par <- 0.00697628
    # runs with 10000 simulated stimuli / percepts:
    par <- c(0.007071804)
  }
  
  if (distribution == 'gamma') {
    shape <- 3.735556
    rate  <-  2.805160
    par   <- c(rate/100, shape)
  }
  
  df <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
  df <- df[which(df$participant == 'group'),]
  
  df <- df[c('path_length', 'stimdur', 'PSE')]
  names(df) <- c('path_length', 'duration', 'PSE')
  df$fitPSE <- NA
  
  
  for (condno in c(1:dim(df)[1])) {
    df$fitPSE[condno] <- mean(bootstrapPSEs(par=par,
                                            duration=df$duration[condno],
                                            path_length=df$path_length[condno],
                                            ) )
  }
  
  return(df)
  
  
}


fitResetProbability <- function(data=NULL) {
  
  if (is.null(data)) {
    # use Sirui's original findings
    # these are the 6 conditions:
    data <- expand.grid(
      'duration'   = c( 1, 2, 3 ),
      'path_length' = c( 2, 4 )
    )
    data$PSE <- c(1.00, 0.83, 0.66, 1.65, 1.50, 1.20)
  }
  
  par <- c(0.01)
  
  fit <- optim(par          = par,
               fn           = timeLimitMSE, 
               data         = data,
               runs         = 1000000,
               method       = 'Brent',
               lower        = c(0),
               upper        = c(1))
  
  return(fit)
  
}

timeLimitMSE <- function(par, data, slice=0.01, runs=10000) {
  
  MSE <- c()
  
  for (cn in dim(data)[1]) {
    
    duration <- data$duration[cn]
    path_length <- data$path_length[cn]
    
    predictedPSEs <- bootstrapPSEs(par=par, duration=duration, path_length=path_length, slice=slice, runs=runs)
    
    MSE <- c(MSE, (mean(predictedPSEs) - data$PSE[cn])^2)
    
  }
  
  return(mean(MSE))
  
}

bootstrapPSEs <- function(par, duration=1, path_length=1, slice=0.01, runs=10000) {
  
  # repeat for a sufficciently large number of runs:
  allPSEs <- sapply(rep(list(par),runs),FUN=generateOnePSE, slice=slice, duration=duration, path_length=path_length)
  
  return(allPSEs)
  
}

generateOnePSE <- function(par, slice=0.01, duration=1, path_length=1, returnTrajectory=FALSE) {
  
  reset_probability <- par[1]
  
  if (length(par) == 1) {
    wait_for_events <- 1
  } else {
    avg_wait_for_events <- par[2]
    fraction <- avg_wait_for_events - floor(avg_wait_for_events)
    if (fraction != 0) {
      wait_for_events <- round( floor(avg_wait_for_events) + (fraction > runif(1)) )
    } else {
      wait_for_events <- round( avg_wait_for_events )
    }
  }
  
  events_counted <- 0
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
      events_counted <- events_counted + 1
      if (events_counted == wait_for_events) {
        reset_time       <- time
        events_counted   <- 0
      }
      
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



# gamma distribution based time model -----

# getBestAmpFitPlotPoints <- function() {
#   
#   par <- c(88.56911) # best fit
#   par <- c(0.1)
#   # 
#   shape <- 3.735556
#   rate <-  2.805160
#   
#   
#   df <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
#   df <- df[which(df$participant == 'group'),]
#   
#   data <- df[c('path_length', 'stimdur')]
#   names(data) <- c('path_length', 'duration')
#   data$PSE <- df$PSE
#   
#   data$fitPSE <- NA
#   
#   
#   for (cn in dim(data)[1]) {
#     
#     duration <- data$duration[cn]
#     path_length <- data$path_length[cn]
#     
#     predictedPSEs <- bootstrapGammaAmpPSEs(par         = par, 
#                                            duration    = data$duration[cn], 
#                                            path_length = data$path_length[cn], 
#                                            runs        = runs, 
#                                            shape       = shape, 
#                                            rate        = rate)
#     
#     data$fitPSE <- mean(predictedPSEs)
#     
#   }
#   
#   return(data)
#   
# }
# 
# fitResetAmpRAD <- function() {
#   
#   #Re-Analyzed Data
#   
#   df <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
#   df <- df[which(df$participant == 'group'),]
#   
#   conditions <- df[c('path_length', 'stimdur')]
#   names(conditions) <- c('path_length', 'duration')
#   conditions$PSE <- df$PSE
#   
#   fit <- fitResetAmplitude(data=conditions)
#   
#   return(fit)
#   
# }
# 
# 
# fitResetAmplitude <- function(data=NULL) {
#   
#   # optimal fit of gamma distribtuion on T of resets:
#   shape <- 3.735556
#   rate <-  2.805160
#   
#   if (is.null(data)) {
#     # these are the 6 conditions:
#     data <- expand.grid(
#       'duration'   = c( 1, 2, 3 ),
#       'path_length' = c( 2, 4 )
#     )
#     data$PSE <- c(1.00, 0.83, 0.66, 1.65, 1.50, 1.20)
#   }
# 
#   par <- c(5) # 45 degree reset
#   
#   fit <- optim(par          = par,
#                fn           = gammaAmpMSE, 
#                data         = data,
#                runs         = 100000,
#                method       = 'Brent',
#                lower        = c(0),
#                upper        = c(90),
#                shape        = shape,
#                rate         = rate)
#   
#   return(fit)
#   
# }
# 
# gammaAmpMSE <- function(par, data, runs=10000, shape, rate) {
# 
#   MSE <- c()
#   
#   for (cn in dim(data)[1]) {
#     
#     duration <- data$duration[cn]
#     path_length <- data$path_length[cn]
#     
#     predictedPSEs <- bootstrapGammaAmpPSEs(par=par, 
#                                            duration=duration, 
#                                            path_length=path_length, 
#                                            runs=runs, 
#                                            shape=shape, 
#                                            rate=rate)
#     
#     MSE <- c(MSE, (mean(predictedPSEs) - data$PSE[cn])^2)
#     
#   }
#   
#   return(mean(MSE))
#   
# }
# 
# bootstrapGammaAmpPSEs <- function(par, duration, path_length, runs=10000, shape, rate) {
#   
#   # repeat for a sufficciently large number of runs:
#   allPSEs <- sapply(rep(list(par),runs),FUN=generateOneGammaAmpPSE, duration=duration, path_length=path_length, shape=shape, rate=rate)
#   
#   return(allPSEs)
#   
# }
# 
# generateOneGammaAmpPSE <- function(par, duration=duration, path_length=path_length, shape=shape, rate=rate) {
#   
#   # at the start, we don't have a PSE, but we'll get one:
#   PSE <- NULL
#   
#   Ve <- path_length / duration # dva's per second
#   ar <- (par/180)*pi           # reset Angle in Radians
#   slope <- sin(ar)/cos(ar)     # as a slope (for trig)
#     
#   offset     <- c(0,0)  # this is where the percept starts from
#   total_time <- 0       # this is how much time has elapsed
#   
#   # we do stuff until there is a PSE to report:
#   while (is.null(PSE)) {
#     
#     # we sample a reset from the gamma distribution:
#     resetTime <- rgamma(1,shape=shape,rate=rate)
#     
#     if ((total_time + resetTime) > duration) {
#       # if we exceed stimulus duration, we calculate a PSE at cut-off (before the reset)
#       
#       # this should get us out of the while loop, to return the PSE:
#       PSE <- (((duration - total_time) * Ve) / sqrt(2)) + offset[1]
#       
#     } else {
#     
#       # it's always a 45 degree line
#       # so we add the same distance to both offset coordinates
#       offset <- offset + ((resetTime/sqrt(2)) * Ve)
#       
#       # now we do the reset:
#       if (par == 90) {
#         # hit-the-wall reset, such that the PSE is equal to the offset at the reset:
#         PSE <- offset
#       } else if (par == 0) {
#         # jump reset, also fairly simple:
#         offset[1]   <- 0
#         total_time  <- total_time + resetTime
#         
#       } else {
#         # something in between:
# 
#         # let's see if this gets us over the finish line:
#         if (total_time + resetTime + ((slope * offset[1]) / Ve) > duration) {
#           # the PSE gets measured before the percept is back at zero:
#           PSE <- offset[1] - ((path_length - offset[2]) / slope)
#         } else {
#           # we get the new offset and total time, for build-up towards a another reset:
#           offset[1] <- 0
#           offset[2] <- offset[2] + (offset[1] * slope)
#           total_time <- total_time + resetTime + ((offset[1] * slope) / Ve)
#         }
#         
#       }
#       
#     }
#     
#   }
#   
#   return(PSE)
#   
# }


# code graveyard -----

# plotJumpPSEs_old <- function(target='inline') {
#   
#   colors <- getColors()
#   
#   if (target == 'pdf') {
#     pdf( file = 'doc/fig_6_gaborjump_exp.pdf', width=6, height=4 )
#   }
#   
#   layout(mat = matrix(c(1,2,1,3), 
#                       ncol  = 2,
#                       nrow  = 2,
#                       byrow = TRUE),
#          widths = c(1.125,1.2))
#   
#   methodsFig <- magick::image_read_pdf('doc/methods_fig_3.pdf')
#   
#   # extract width and height properties:
#   width <- magick::image_info(methodsFig)[['width']]
#   height <- magick::image_info(methodsFig)[['height']]
#   
#   #print(width)
#   #print(height)
#   
#   par(mar=c(1,1,2,1))
#   
#   pos = 0.15 # Plot OffSet
#   
#   plot(methodsFig,xlim=c(0,width),ylim=c(0,height),asp=1)
#   title('A: methods', font.main=1, cex.main=1.5, adj=0, line=0.5)
#   
#   PSEdf <- read.csv('data/gaborjump/exp_PSEs.csv', stringsAsFactors = FALSE)
#   groupPSEs <- PSEdf[which(PSEdf$participant == 'group'),]
#   participantPSEs <- PSEdf[which(PSEdf$participant != 'group'),]
#   
#   # resp_df <- read.csv('data/gaborjump/gaborjump_exp_data.csv', stringsAsFactors = FALSE) 
#   # ext1 <- which(resp_df$exmotion == 1)
#   # resp_df$gapshift[ext1] <- -1 * resp_df$gapshift[ext1]
#   # resp_df$response[ext1] <- -1 * resp_df$response[ext1]
#   # resp_df$response[which(resp_df$response == -1)] <- 0
#   # ARdf <- aggregate(response ~ gapshift + path_length + stimdur, data=resp_df, FUN=mean)
#   
#   # X <- seq(-3.8,3.8,length.out=153)
#   
#   
#   for (pathlength in c(2,4)) {
#     
#     #par(mar=c(3.5,3.5,2.2,0.1))
#     par(mar = c(2.8,2.8,2.2,0.1),
#         mgp = c(2, 0.55, 0) )
#     
#     plot(-1000,-1000,main='',xlab='',ylab='',
#          xlim=c(0.1,3.9),ylim=c(0,3.5),
#          bty='n',ax=F)
#     
#     title(main=c('B: 2 dva results', 'C: 4 dva results')[pathlength/2], font.main=1, cex.main=1.5, adj=0, line=0.5)
#     title(xlab='stimulus duration [s]', line=1.6, cex.lab=0.7)
#     title(ylab='gap-evoked reset [dva]', line=1.6, cex.lab=0.7)
#     
#     lines(c(0.3,3.7),rep(pathlength/sqrt(2),2),col='#000000',lty=2)
#     
#     text( x=3.2, y=(pathlength/sqrt(2))+0.2, sprintf('%0.1f dva', pathlength/sqrt(2)), cex=0.7, col='#000000'  )
#     
#     lc <- c()
#     
#     for (stimdur in c(1,2,3)) {
#       
#       idx <- which(groupPSEs$path_length == pathlength & groupPSEs$stimdur == stimdur)
#       mu <- groupPSEs$PSE[idx]
#       sigma <- groupPSEs$std[idx]
#       
#       color <- colors[[ c('orange', 'yorkred', 'purple')[stimdur] ]]$s
#       lc <- c(lc, color)
#       
#       pPSEs <- participantPSEs$PSE[which(participantPSEs$path_length == pathlength & participantPSEs$stimdur == stimdur)]
#       
#       CI <- getConfidenceInterval(data=pPSEs)
#       lines(rep(stimdur+pos,2),CI,col=color,lty=1,lw=1)
#       points(stimdur+pos,mu,col=color,pch=16,cex=1)
#       points(rep(stimdur-pos,length(pPSEs)),pPSEs,col=t_col(color,percent=75),pch=16,cex=1)
#       
#       
#     }
#     
#     if (pathlength == 2) {
#       legend(1.75,3.9,
#              legend=c('no spontaneous resets','1 s', '2 s', '3 s'),
#              col=c('#000000',lc), lty=c(2,1,1,1),
#              pch=c(NA,16,16,16),
#              seg.len = 1.5,
#              bty='n', cex=0.7)
#     } 
#     
#     axis(side=1,at=c(1,2,3),cex.axis=0.7)
#     axis(side=2,at=c(0.0,0.7,2.1,3.5),labels = c('0','0.7','2.1','3.5'),cex.axis=0.7)
#     
#     
#   }
#   
#   if (target %in% c('svg','pdf')) {
#     dev.off()
#   }
#   
# }
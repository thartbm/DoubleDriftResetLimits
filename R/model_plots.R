
source('R/common.R')
source('R/models_likelihood.R')


plotIndividualModelFits <- function() {
  
  colors <- getColors()
  
  opv4 <- getDataTrials()
  cvrd <- getCVRdata()
  
  pdf(file='individualModels.pdf',
            width=8.5,
            height=11)
  
  data <- makeRPgrid(stepsize=.05)
  
  
  layout(matrix(c(1,11,12,2,3,4,5,6,7,8,9,10),byrow=TRUE,ncol=3,nrow=4))
  
  for (group in c('lab','demo')) {
    
    if (group == 'lab') {
      df <- opv4
    }
    if (group == 'demo') {
      df <- cvrd
    }
    
    participants <- unique(df$participant)
    
    for (participant in participants) {
      
      pdf <- df[which(df$participant == participant),]
      
      pdf <- pdf[which(pdf$X > 0),]
      
      plot(-1000,-1000,
           main=sprintf('%s: participant %d',group,participant),
           xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')),ylab='illusion strength [°]',
           xlim=c(0,pi/2),ylim=c(0,90),
           bty='n',ax=F)
      
      angles <- c(0,(2*pi)/3)
      lines(angles,(angles/pi)*180,col='gray',lty=2)
      lines(angles,0.81*((angles/pi)*180),col='black',lty=1)
      
      # data:
      X <- atan(pdf$Vi / pdf$Ve)
      Y <- 90 - (pdf$angle/pi)*180
      
      points(X, Y, col=colors$lightblue$s, pch=1, cex=1.0)
      
      # get the best k
      # linear model with only a slope (i.e. [0,0] is a point on the line)
      X <- (X/pi)*180
      linmod <- lm(Y ~ X - 1)
      slope <- summary(linmod)$coefficients['X','Estimate']
      
      print(slope)
      
      # plot that as a line:
      lines(angles,slope*((angles/pi)*180),col=colors$purple$s,lty=1)
      
      
      legend(x=0, y=90, 
             legend=c('k=0.81', 'percepts', sprintf('k=%0.2f',slope)), 
             col=c('black', colors$lightblue$s, colors$purple$s), 
             pch=c(NA,1,NA), lty=c(1,0,1), 
             bty='n', cex=1)
      
      #axis(side=1,at=seq(0,pi/4,pi/8),labels=c('0',expression(pi/8),expression(pi/4)))
      axis(side=1,at=seq(0,pi/2,pi/6),labels=sprintf('%d',seq(0,90,30)))
      axis(side=2,at=seq(0,90,30))
      
      
      ## # # # # # # # # # # # # # # # # # # # # 
      # ALL THE FITS:
      
      fits <- list()
      
      fits$normal      <- fitSingleLimitLikelihoodModels(pdf)
      fits$exponential <- fitSingleOffsetExponentialLikelihoodModels(pdf)
      fits$gamma       <- fitSingleOffsetGammaLikelihoodModels(pdf)
      
      maxL <- NA
      
      # which model fits best?
      
      distr <- c()
      limit <- c()
      logL  <- c()
      
      for (d in c('normal','exponential','gamma')) {
        for(l in c('Xlim','Ylim','Tlim')) {
          distr <- c(distr, d)
          limit <- c(limit, l)
          logL  <- c(logL, fits[[d]][[l]]$logL)
        }
      }
      
      fitsdf <- data.frame(logL, limit, distr)
      fitsdf <- fitsdf[order(fitsdf$logL, decreasing=TRUE),]
      #print(fitsdf)
      
      idx_lim <- which((fitsdf$limit %in% c('Xlim','Ylim')))
      idx_dis <- which((fitsdf$distr %in% c('normal','exponential')))
      idx <- intersect(idx_lim, idx_dis)
      #print(idx)
      subfitsdf <- fitsdf[idx,]
      
      #print(subfitsdf)
      
      for (mod in c('Xlim','Ylim','Tlim')) {
        
        #print(normalfits[[model]])
        pardf <- fits$normal[[mod]]$par
        
        FUN <- list('Xlim'=XlimProbabilities,
                    'Ylim'=YlimProbabilities,
                    'Tlim'=TlimProbabilities)[[mod]]
        
        # function is correct:
        #print(FUN)
        
        col = 'black'
        pal = 'LightGrays'
        if (subfitsdf$distr[1] == 'normal' && subfitsdf$limit[1] == mod) {
          col = colors$lightblue$s
          pal = 'Sunset'
        }
        
        if (fitsdf$distr[1] == 'normal' && fitsdf$limit[1] == mod) {
          col = colors$yorkred$s
          pal = 'Mint'
        }
        
        # convert from optimx data frame to named vector:
        par <- c()
        for (name in names(pardf)) {
          par[name]=pardf[1,name]
        }
        
        L <- FUN(par, data)
        
        # if (is.numeric(maxL)) {
        #   maxL <- max(maxL,max(L$L))
        # } else {
        #   maxL <- max(L)
        # }
        
        x <- c(unique(data$X))
        y <- c(unique(data$Y))
        z <- matrix(L$L,
                    ncol=length(x-1),
                    nrow=length(y-1))
        
        maintitle <- list('Xlim'='X: normal',
                          'Ylim'='Y: normal',
                          'Tlim'='T: normal')[[mod]]
        
        image(main=maintitle,
              x,
              y,
              z,
              useRaster=TRUE,
              xlim=c(0,13.5), ylim=c(0,13.5),
              #col = gray.colors(256, rev=TRUE),
              col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
              #lw=0,
              ax=F,
              asp=1)

        # col = hcl.colors(n=256, palette='Purp', alpha=0.2, rev=TRUE)
        # col = viridis::plasma(n=250, alpha=0.2)

        points(pdf$X, pdf$Y,col=col)
        
        axis(side=1,at=seq(0,13.5,4.5))
        axis(side=2,at=seq(0,13.5,4.5))
        
        #plot.new()
        
      }
      
      # # # # # # # # # # # # # # # # 
      # EXPONENTIAL DISTRIBUTIONS
      # # # # # # # # # # # # # # # # 
      
      # exponentialfits <- fitSingleOffsetExponentialLikelihoodModels(pdf)
      # gammafits <- fitSingleOffsetGammaLikelihoodModels(pdf)
      
      
      for (mod in c('Xlim','Ylim','Tlim')) {
        
        #print(mod)
        
        #print(normalfits[[model]])
        pardf <- fits$exponential[[mod]]$par
        
        FUN <- list('Xlim'=XoffsetExponentialProbabilities,
                    'Ylim'=YoffsetExponentialProbabilities,
                    'Tlim'=ToffsetExponentialProbabilities)[[mod]]

        col = 'black'
        pal = 'LightGrays'
        if (subfitsdf$distr[1] == 'exponential' && subfitsdf$limit[1] == mod) {
          col = colors$lightblue$s
          pal = 'Sunset'
        }
        if (fitsdf$distr[1] == 'exponential' && fitsdf$limit[1] == mod) {
          col = colors$yorkred$s
          pal = 'Mint'
        }
        
        
        # function is correct:
        #print(FUN)

        # convert from optimx data frame to named vector:
        par <- c()
        for (name in names(pardf)) {
          par[name]=pardf[1,name]
        }
        #print(par)
        
        L <- FUN(par, data)
        
        #print(str(data))
        #print(str(L))
        
        this.minL <- min(L[which(is.finite(L$L)),])
        #print(this.minL)
        L$L[which(!is.finite(L$L))] <- this.minL

        if (is.numeric(maxL)) {
          maxL <- max(maxL,max(L$L))
        } else {
          maxL <- max(L)
        }

        x <- c(unique(data$X))
        y <- c(unique(data$Y))
        z <- matrix(L$L,
                    ncol=length(x-1),
                    nrow=length(y-1))

        maintitle <- list('Xlim'='X: exponential',
                          'Ylim'='Y: exponential',
                          'Tlim'='T: exponential')[[mod]]

        image(main=maintitle,
              x,
              y,
              z,
              useRaster=TRUE,
              xlim=c(0,13.5), ylim=c(0,13.5),
              #col = gray.colors(256, rev=TRUE),
              col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
              #lw=0,
              ax=F,
              asp=1)

        # col = hcl.colors(n=256, palette='Purp', alpha=0.2, rev=TRUE)
        # col = viridis::plasma(n=250, alpha=0.2)

        points(pdf$X, pdf$Y,col=col)

        axis(side=1,at=seq(0,13.5,4.5))
        axis(side=2,at=seq(0,13.5,4.5))
        
      }

      # # # # # # # # # # # # # # # # 
      # GAMME DISTRIBUTIONS
      # # # # # # # # # # # # # # # # 
      
      #gammafits <- fitSingleOffsetGammaLikelihoodModels(pdf)
      
      
      for (mod in c('Xlim','Ylim','Tlim')) {
        
        #print(mod)
        
        #print(normalfits[[model]])
        pardf <- fits$gamma[[mod]]$par
        
        FUN <- list('Xlim'=XoffsetGammaProbabilities,
                    'Ylim'=YoffsetGammaProbabilities,
                    'Tlim'=ToffsetGammaProbabilities)[[mod]]
        
        col = 'black'
        pal = 'LightGrays'
        # if (subfitsdf$distr[1] == 'gamma' && subfitsdf$limit[1] == mod) {
        #   col = colors$lightblue$s
        #   pal = 'Sunset'
        # }
        
        if (fitsdf$distr[1] == 'gamma' && fitsdf$limit[1] == mod) {
          col = colors$yorkred$s
          pal = 'Mint'
        }
        
        # function is correct:
        #print(FUN)
        
        # convert from optimx data frame to named vector:
        par <- c()
        for (name in names(pardf)) {
          par[name]=pardf[1,name]
        }
        #print(par)
        
        L <- FUN(par, data)
        
        #print(str(data))
        #print(str(L))
        
        this.minL <- min(L[which(is.finite(L$L)),])
        #print(this.minL)
        L$L[which(!is.finite(L$L))] <- this.minL
        
        if (is.numeric(maxL)) {
          maxL <- max(maxL,max(L$L))
        } else {
          maxL <- max(L)
        }
        
        x <- c(unique(data$X))
        y <- c(unique(data$Y))
        z <- matrix(L$L,
                    ncol=length(x-1),
                    nrow=length(y-1))
        
        maintitle <- list('Xlim'='X: gamma',
                          'Ylim'='Y: gamma',
                          'Tlim'='T: gamma')[[mod]]
        
        image(main=maintitle,
              x,
              y,
              z,
              useRaster=TRUE,
              xlim=c(0,13.5), ylim=c(0,13.5),
              #col = gray.colors(256, rev=TRUE),
              col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
              #lw=0,
              ax=F,
              asp=1)
        
        # col = hcl.colors(n=256, palette='Purp', alpha=0.2, rev=TRUE)
        # col = viridis::plasma(n=250, alpha=0.2)
        
        points(pdf$X, pdf$Y,col=col)
        
        axis(side=1,at=seq(0,13.5,4.5))
        axis(side=2,at=seq(0,13.5,4.5))
        
      }
      
      #plot.new()
      
      plot(-1000, -1000, 
           main='', xlab='', ylab='',
           xlim=c(0,10), ylim=c(0,10),
           ax=F,bty='n')
      
      text(1,10,'logL')
      text(4,10,'limit')
      text(7,10,'distribution')
      for (m in c(1:dim(fitsdf)[1])) {
        if (m == 1) {col = colors$yorkred$s} else {col = 'black'}
        text(1,10-m,sprintf('%0.4f',fitsdf[m,'logL']), col=col)
        text(4,10-m,fitsdf[m,'limit'], col=col)
        text(7,10-m,fitsdf[m,'distr'], col=col)
      }

      plot.new()
      
    }
    
  }
  
  dev.off()
  
}
  

makeRPgrid <- function(stepsize=0.1, speed=13.5/4) {
  
  RPgrid <- expand.grid('X'=seq(stepsize,13.5,stepsize),'Y'=seq(stepsize,13.5,stepsize))
  
  RPgrid$speed <- speed
  RPgrid$RT    <- sqrt(RPgrid$X^2 + RPgrid$Y^2) / RPgrid$speed
  
  RPgrid$angle <- pi - atan2(RPgrid$Y, RPgrid$X)
  RPgrid$sin.a <- sin(RPgrid$angle)
  RPgrid$cos.a <- cos(RPgrid$angle)
  RPgrid$slope <- RPgrid$sin.a / RPgrid$cos.a
  
  return(RPgrid)
  
}
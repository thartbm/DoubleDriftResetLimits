
source('R/models.R')
source('R/retrace_onePass_V4.R')

# *****************
# plot residuals for individually fit limits

individualResidualsPlot <- function() {
  
  df <- prepData()
  
  df <- addPredictedCoordinates(df)
  
  par(mar=c(3.5,3.5,3.5,0.5))
  
  layout(matrix(c(1:9),ncol=3,nrow=3,byrow=TRUE))
  
  for (coordinate in c('X','Y','2D')) {
    
    for (model in c('Lx','Ly','Lx.Ly')) {
      
      if (coordinate %in% c('X','Y')) {
        
        # predicted over actual
        Ydots <- df[,sprintf('%s_%s',model,coordinate)]
        Xdots <- df[,coordinate]
        
        xlim <- c(0,13.5)
        ylim <- c(0,13.5)
        
        xlab <- 'actual'
        ylab <- 'predicted'
        
      } else {
        
        Xdots <- df[,sprintf('%s_X',model)] - df[,'X']
        Ydots <- df[,sprintf('%s_Y',model)] - df[,'Y']
        
        xlim <- c(-13.5,13.5)
        ylim <- c(-13.5,13.5)
        
        xlab <- 'X error'
        ylab <- 'Y error'
        
      }
      
      plot(Xdots,Ydots, 
           main=sprintf('%s (%s)',model,coordinate), xlab='', ylab='',
           xlim=xlim, ylim=ylim, 
           col='#e516362f',
           asp=1,bty='n',ax=F)
      
      title(xlab=xlab, line=2.5)
      title(ylab=ylab, line=2.5)
      
      axis(side=1, at=seq(min(xlim),max(xlim),4.5))
      axis(side=2, at=seq(min(ylim),max(ylim),4.5))
      
    }
    
  }

}


prepData <- function() {
  
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = FALSE)
  df <- df[which(is.finite(df$illusionstrength)),]
  
  df$angle_rad <- (df$illusionstrength/180)*pi
  
  df$sin.a <- sin(df$angle_rad)
  df$cos.a <- cos(df$angle_rad)
  
  df$slope <- df$cos.a / df$sin.a
  
  df$speed <- round(0.5 / df$externalspeed)
  
  df$X <- df$boundX * 13.5
  df$Y <- df$boundY * 13.5
  
  return(df)
  
}

getIndividualFits <- function(df) {
  
  ppnos <- unique(df$participant)
  
  trials <- c()
  
  Lx_Lx     <- c()
  Lx_MSE    <- c()
  Ly_Ly     <- c()
  Ly_MSE    <- c()
  Lx.Ly_Lx  <- c()
  Lx.Ly_Ly  <- c()
  Lx.Ly_MSE <- c()
  
  for (ppno in ppnos) {
    
    ppdf <- df[which(df$participant == ppno),]
    
    # we only get positive illusion strengths:
    ppdf <- ppdf[which(ppdf$illusionstrength > 5),]
    
    # and remove the strongest five:
    sis <- sort(ppdf$illusionstrength)
    cutoff <- sis[length(sis)-5]
    ppdf <- ppdf[which(ppdf$illusionstrength < cutoff),]

    # we want to know how many trials are left:
    trials <- c(trials, dim(ppdf)[1])
    
    # we fit the models:
    pp1fits <- fitSingleLimitModels(ppdf)
    print(pp1fits)
    pp2fit  <- fitTwoLimitModel(ppdf)
    
    # and store the fit parameters and MSEs:
    Lx_Lx     <- c(Lx_Lx, unname(pp1fits$Xlim$par['Lx']))
    Lx_MSE    <- c(Lx_MSE, unname(pp1fits$Xlim$MSE))
    Ly_Ly     <- c(Ly_Ly, unname(pp1fits$Ylim$par['Ly']))
    Ly_MSE    <- c(Ly_MSE, unname(pp1fits$Ylim$MSE))
    Lx.Ly_Lx  <- c(Lx.Ly_Lx, unname(pp2fit$par['Lx']))
    Lx.Ly_Ly  <- c(Lx.Ly_Ly, unname(pp2fit$par['Ly']))
    Lx.Ly_MSE <- c(Lx.Ly_MSE, unname(pp2fit$MSE))
    
  }
  
  # the fit parameters and MSEs are put in a data frame:
  individual.fits <- data.frame('participant'=ppnos,
                                trials,
                                Lx_Lx,
                                Lx_MSE,
                                Ly_Ly,
                                Ly_MSE,
                                Lx.Ly_Lx,
                                Lx.Ly_Ly,
                                Lx.Ly_MSE)
  
  # the data frame is stored
  write.csv(individual.fits, 'data/individual_fits.csv', row.names=FALSE)
  
  # and returned to the caller
  return(individual.fits)
  
}

addPredictedCoordinates <- function(df) {
  
  ppfits <- getIndividualFits(df)
  
  df$Lx_X <- NA
  df$Lx_Y <- NA
  
  df$Ly_X <- NA
  df$Ly_Y <- NA
  
  df$Lx.Ly_X <- NA
  df$Lx.Ly_Y <- NA
  
  for (ppno in unique(df$participant)) {
    
    idx <- which(df$participant == ppno)
    
    ppdat <- df[which(df$participant == ppno),]
    
    Lx_Lx <- ppfits$Lx_Lx[which(ppfits$participant == ppno)]
    Ly_Ly <- ppfits$Ly_Ly[which(ppfits$participant == ppno)]
    Lx.Ly_Lx <- ppfits$Lx.Ly_Lx[which(ppfits$participant == ppno)]
    Lx.Ly_Ly <- ppfits$Lx.Ly_Ly[which(ppfits$participant == ppno)]
    
    Lx <- XlimResets(par=c('Lx'=Lx_Lx), data=ppdat)
    
    df$Lx_X[idx] <- Lx$X
    df$Lx_Y[idx] <- Lx$Y
    
    Ly <- YlimResets(par=c('Ly'=Ly_Ly), data=ppdat)
    
    df$Ly_X[idx] <- Ly$X
    df$Ly_Y[idx] <- Ly$Y
    
    Lx.Ly <- twoLimResets(par=c('Lx'=Lx.Ly_Lx, 'Ly'=Lx.Ly_Ly), data=ppdat)
    
    df$Lx.Ly_X[idx] <- Lx.Ly$X
    df$Lx.Ly_Y[idx] <- Lx.Ly$Y
    
  }
  
  return(df)
  
}

getIndividualFitQuality <- function() {
  
  ppfits <- read.csv('data/individual_fits.csv')
  
  MSEs <- c( 'Lx'=mean(ppfits$Lx_MSE), 
             'Ly'=mean(ppfits$Ly_MSE), 
             'Lx.Ly'=mean(ppfits$Lx.Ly_MSE))
  
  print(MSEs)
  
  AICs <- AIC(MSE=MSEs, k=c(9,9,18), N=9)
  
  print(AICs)
  
  rLL <- relativeLikelihood(AICs)
  
  print(rLL)
  
}


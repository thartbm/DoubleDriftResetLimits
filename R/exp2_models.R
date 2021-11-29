
# *************************
# model functions -----
# *************************


  TlimResets <- function(par,data) {
  
  # time on diagonal limit
  
  Lt <- par['Lt']
  
  d <- Lt * data$speed
  
  Y <- d * data$sin.a
  X <- d * data$cos.a
  
  return(data.frame(X,Y))
  
}

YlimResets <- function(par,data) {
  
  # time on y-axis limit
  
  Ly <- par['Ly']
  
  X = Ly / data$slope
  
  return(data.frame('X'=X, 'Y'=rep(Ly,length(X))))
  
}


XlimResets <- function(par,data) {
  
  # space limit
  
  Lx <- par['Lx']
  
  Y <- Lx * data$slope
  
  return(data.frame('Y'=Y, 'X'=rep(Lx,length(Y))))
  
}

XTlimResets <- function(par,data) {
  
  Lx <- par['Lx']
  Lt <- par['Lt']
  
  d <- Lt * data$speed
  
  Y <- (d * data$sin.a) + (Lx * data$slope)
  X <- (d * data$cos.a) + rep(Lx, length(Y))
  
  return(data.frame(X,Y))
  
}

XYlimResets <- function(par,data) {
  
  Lx <- par['Lx']
  Ly <- par['Ly']
  
  Y <- Ly + (Lx * data$slope)
  X <- (Ly / data$slope) + Lx
  
  return(data.frame(X,Y))
  
}


YTlimResets <- function(par,data) {
  
  Ly <- par['Ly']
  Lt <- par['Lt']
  
  d <- Lt * data$speed
  
  Y <- (d * data$sin.a) + Ly
  X <- (d * data$cos.a) + (Ly / data$slope)
  
  return(data.frame(X,Y))
  
}

XYTlimResets <- function(par, data) {
  
  Ly <- par['Ly']
  Lx <- par['Lx']
  Lt <- par['Lt']
  
  d <- Lt * data$speed
  
  Y <- (d * data$sin.a) + Ly + (Lx * data$slope)
  X <- (d * data$cos.a) + (Ly / data$slope) + Lx
  
  return(data.frame(X,Y))
  
}


individualLimResets <- function(par,data) {
  
  participants <- unique(data$participant)
  
  X <- rep(NA, dim(data)[1])
  Y <- rep(NA, dim(data)[1])
  
  for (pn in participants) {
    
    pdata <- data[which(data$participant == pn),]
    
    if (sprintf('Lx%d',pn) %in% names(par)) {
      Lx <- par[sprintf('Lx%d',pn)]
    } else {
      Lx <- par['Lx']
    }
    if (sprintf('Ly%d',pn) %in% names(par)) {
      Ly <- par[sprintf('Ly%d',pn)]
    } else {
      Ly <- par['Ly']
    }
    
    d <- Ly * pdata$speed
    
    Y[which(data$participant == pn)] <- (d * pdata$sin.a) + (Lx * pdata$slope)
    X[which(data$participant == pn)] <- (d * pdata$cos.a) + rep(Lx, dim(pdata)[1])
    
  }
  
  return(data.frame(X,Y))
  
}


# *******************
# model fitting -----
# *******************


resetMSE <- function(par,data,fitFUN) {
  
  resets <- fitFUN(par,data)

  MSE <- mean( (resets$Y - data$Y)^2 + (resets$X - data$X)^2 )
  
  return(MSE)
  
}

library('optimx')

fitSingleLimitModels <- function(df) {
  
  # print(df$X)
  # print(df$Y)
  # print(df$speed)
  # print(df$sin.a)
  # print(df$cos.a)
  
  # create search "grids":
  Lx=seq(0, 8, length.out = 41)
  Ly=seq(0, 13.5, length.out = 41)
  Lt=seq(0, 4, length.out = 41)
  
  # make them into data frames:
  searchgridYlim <- expand.grid('Ly'=Ly)
  searchgridXlim <- expand.grid('Lx'=Lx)
  searchgridTlim <- expand.grid('Lt'=Lt)
  
  # get MSE for points in search grid:
  YlimMSE <- apply(searchgridYlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=YlimResets)
  XlimMSE <- apply(searchgridXlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=XlimResets)
  TlimMSE <- apply(searchgridTlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=TlimResets)
  
  # get 4 best points in the grid:
  topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[c(1,3,5)],])
  topgridYlim <- data.frame('Ly'=searchgridYlim[order(YlimMSE)[c(1,3,5)],])
  topgridTlim <- data.frame('Lt'=searchgridTlim[order(TlimMSE)[c(1,3,5)],])
  
  # do the actual fitting:
  allXlimFits <- do.call("rbind",
                         apply( topgridXlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(13.5),
                                data=df,
                                fitFUN=XlimResets) )
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(13.5),
                                data=df,
                                fitFUN=YlimResets) )
  
  allTlimFits <- do.call("rbind",
                         apply( topgridTlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(4),
                                data=df,
                                fitFUN=TlimResets) )
  
  #print(allXlimFits)
  #print(allYlimFits)
  
  # pick the best fit:
  winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
  winYlimFit <- allYlimFits[order(allYlimFits$value)[1],]
  winTlimFit <- allTlimFits[order(allTlimFits$value)[1],]
  # print(win[1:3])
  
  winXlim <- as.numeric(winXlimFit[1])
  names(winXlim) <- c('Lx')
  winYlim <- as.numeric(winYlimFit[1])
  names(winYlim) <- c('Ly')
  winTlim <- as.numeric(winTlimFit[1])
  names(winTlim) <- c('Lt')
  
  winXval <- as.numeric(winXlimFit[2])
  names(winXval) <- c('Lx')
  winYval <- as.numeric(winYlimFit[2])
  names(winYval) <- c('Ly')
  winTval <- as.numeric(winTlimFit[2])
  names(winTval) <- c('Lt')
  
  
  return(list( 'Xlim'=list('par'=winXlim,'MSE'=winXval),
               'Ylim'=list('par'=winYlim,'MSE'=winYval),
               'Tlim'=list('par'=winTlim,'MSE'=winTval) ) )
  
}


fitTwoLimitModels <- function(df) {
  
  # create search grid:
  Lx = seq(0, 8,    length.out = 21) # more than 8 centimeters is useless (the data goes up to ~5 or ~6)
  Ly = seq(0, 13.5, length.out = 21) 
  Lt = seq(0, 4,    length.out = 21)
  
  LxLysearchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly)
  LxLtsearchgrid <- expand.grid('Lx'=Lx, 'Lt'=Lt)
  LyLtsearchgrid <- expand.grid('Ly'=Ly, 'Lt'=Lt)
  
  # get MSE for points in search grid:
  LxLyMSE <- apply(LxLysearchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=XYlimResets)
  LxLtMSE <- apply(LxLtsearchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=XTlimResets)
  LyLtMSE <- apply(LyLtsearchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=YTlimResets)
  
  # cat('(evaluated search grid)\n')
  
  # get good points in the grid:
  topgridXYlim <- LxLysearchgrid[order(LxLyMSE)[c(1,10,100)],]
  topgridXTlim <- LxLtsearchgrid[order(LxLtMSE)[c(1,10,100)],]
  topgridYTlim <- LyLtsearchgrid[order(LyLtMSE)[c(1,10,100)],]
  

  # do the actual fitting:
  allXYlimfits <- do.call("rbind",
                          apply( topgridXYlim,
                                 MARGIN=c(1),
                                 FUN=optimx,
                                 fn=resetMSE,
                                 method='L-BFGS-B',
                                 lower=c(0,0),
                                 upper=c(8,13.5),
                                 data=df,
                                 fitFUN=XYlimResets) )
  allXTlimfits <- do.call("rbind",
                          apply( topgridXTlim,
                                 MARGIN=c(1),
                                 FUN=optimx,
                                 fn=resetMSE,
                                 method='L-BFGS-B',
                                 lower=c(0,0),
                                 upper=c(8,4),
                                 data=df,
                                 fitFUN=XTlimResets) )
  
  allYTlimfits <- do.call("rbind",
                          apply( topgridYTlim,
                                 MARGIN=c(1),
                                 FUN=optimx,
                                 fn=resetMSE,
                                 method='L-BFGS-B',
                                 lower=c(0,0),
                                 upper=c(13.5,4),
                                 data=df,
                                 fitFUN=YTlimResets) )
  
  # pick the best fit:
  winXY <- allXYlimfits[order(allXYlimfits$value)[1],]
  winXT <- allXTlimfits[order(allXTlimfits$value)[1],]
  winYT <- allYTlimfits[order(allYTlimfits$value)[1],]
  # print(win[1:3])
  #print(winXY)
  #print(winXT)
  #print(winYT)
  
  winparXY <- as.numeric(winXY[1:2])
  names(winparXY) <- names(winXY[1:2])
  winparXT <- as.numeric(winXT[1:2])
  names(winparXT) <- names(winXT[1:2])
  winparYT <- as.numeric(winYT[1:2])
  names(winparYT) <- names(winYT[1:2])
  
  return( list('XY' = list('par'=winparXY, 'MSE'=winXY$value),
               'XT' = list('par'=winparXT, 'MSE'=winXT$value),
               'YT' = list('par'=winparYT, 'MSE'=winYT$value) ) )
  
}

# ***********************
# individual limits -----
# ***********************

fitNineSingleLimitModels <- function(df) {
  
  # create search "grids":
  Lx=seq(0, 4, length.out = 41)
  Ly=seq(0, 4, length.out = 41)
  
  # make them into data frames:
  searchgridYlim <- expand.grid('Ly'=Ly)
  searchgridXlim <- expand.grid('Lx'=Lx)
  
  participant <- c()
  Lx <- c()
  Lx_MSE <- c()
  Ly <- c()
  Ly_MSE <- c()
  
  for (ppno in unique(df$participant)) {
    
    # get only the participants data:
    ppdf <- df[which(df$participant == ppno),]
    
    # get MSE for points in search grid:
    YlimMSE <- apply(searchgridYlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=YlimResets)
    XlimMSE <- apply(searchgridXlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=XlimResets)
    
    # get 4 best points in the grid:
    topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[1:2],])
    topgridYlim <- data.frame('Ly'=searchgridYlim[order(YlimMSE)[1:2],])
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(13.5),
                                  data=ppdf,
                                  fitFUN=XlimResets) )
    
    allYlimFits <- do.call("rbind",
                           apply( topgridYlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(5),
                                  data=ppdf,
                                  fitFUN=YlimResets) )
    
    #print(allXlimFits)
    #print(allYlimFits)
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
    winYlimFit <- allYlimFits[order(allYlimFits$value)[1],]
    # print(win[1:3])
    
    winXlim <- as.numeric(winXlimFit[1])
    names(winXlim) <- c('Lx')
    winYlim <- as.numeric(winYlimFit[1])
    names(winYlim) <- c('Ly')
    
    winXval <- as.numeric(winXlimFit[2])
    names(winXval) <- c('Lx')
    winYval <- as.numeric(winYlimFit[2])
    names(winYval) <- c('Ly')
    
    participant <- c(participant, ppno)
    Lx <- c(Lx, winXlim)
    Lx_MSE <- c(Lx_MSE, winXval)
    Ly <- c(Ly, winYlim)
    Ly_MSE <- c(Ly_MSE, winYval)
    
  }
  
  return(data.frame(participant, Lx, Lx_MSE, Ly, Ly_MSE))
  
}


fitNineXlimitsSingleYlimit <- function(df) {
  
  # create search "grids":
  Lx=seq(1, 3, length.out = 3)
  Ly=seq(0, 4, length.out = 5)
  
  gridlist <- list('Ly'=Ly)
  for (ppno in df$participant) {gridlist[[sprintf('Lx%d',ppno)]] <- Lx}
  
  # make them into data frames:
  searchgrid <- expand.grid(gridlist)
  
  # search the grid for good starting points:
  MSE <- apply(searchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=individualLimResets)
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(MSE)[1:5],]
  # print(topgrid)
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetMSE,
                            method='L-BFGS-B',
                            lower=c(0,0,0,0,0,0,0,0,0,0),
                            upper=c(5,13.5,13.5,13.5,13.5,13.5,13.5,13.5,13.5,13.5),
                            data=df,
                            fitFUN=individualLimResets) )
  # print(allfits)
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  # print(win)
  
  winpar <- as.numeric(win[1:10])
  names(winpar) <- names(win[1:10])
  
  return( list('par'=winpar, 'MSE'=win$value) )
  
  
}


fitNineYlimitsSingleXlimit <- function(df) {
  
  # create search "grids":
  Ly=seq(1, 3, length.out = 3)
  Lx=seq(0, 4, length.out = 5)
  
  gridlist <- list('Lx'=Lx)
  for (ppno in df$participant) {gridlist[[sprintf('Ly%d',ppno)]] <- Ly}
  
  # make them into data frames:
  searchgrid <- expand.grid(gridlist)
  
  # search the grid for good starting points:
  MSE <- apply(searchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=individualLimResets)
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(MSE)[1:5],]
  # print(topgrid)
  
  library(BB)
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetMSE,
                            method='L-BFGS-B',
                            lower=c(0,0,0,0,0,0,0,0,0,0),
                            upper=c(13.5,5,5,5,5,5,5,5,5,5),
                            data=df,
                            fitFUN=individualLimResets) )
  # print(allfits)
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  # print(win)
  
  winpar <- as.numeric(win[1:10])
  names(winpar) <- names(win[1:10])
  
  return( list('par'=winpar, 'MSE'=win$value) )
  
  
}

fitEighteenLimits <- function(df) {
  
  ppnos <- unique(df$participant)
  
  participant <- c()
  Lx <- c()
  Ly <- c()
  MSE <- c()
  
  for (ppno in ppnos) {
    
    pdf <- df[which(df$participant == ppno),]
    
    ppfit <- fitTwoLimitModel(pdf)
    
    participant <- c(participant, ppno)
    Lx <- c(Lx, ppfit$par['Lx'])
    Ly <- c(Ly, ppfit$par['Ly'])
    MSE <- c(MSE, ppfit$MSE)

  }
  
  return(list('fits'=data.frame(participant,Lx,Ly,MSE), 'MSE'=mean(MSE)))
  
}

# **********************
# model evaluation -----
# **********************

AIC <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}

AICc <- function(MSE, k, N) {
  return( AIC(MSE, k, N) * (((2*k^2) + 2*k) / (N - k - 1)) )
}

relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}


# # # # # # # # # # # # # #
# % explained variance -----
# # # # # # # # # # # # # #


D2var <- function(coords, ddof=0) {
  
  # subtract the mean of each column from values in column:
  mcoords <- apply(coords, 2, scale, scale=FALSE, center=TRUE)
  
  return( sum ( rowSums( mcoords^2 ) ) / (dim(coords)[1] - ddof) )
  
}

D2explvar <- function(actual, predicted, ddof) {
  
  # raw data variance:
  mdv.a <- D2var(coords=actual, 
                 ddof=ddof)
  # variance of residuals:
  mdv.r <- D2var(coords=actual-predicted, 
                 ddof=ddof)
  
  # exp.var <- ( (mdv.a-mdv.r) / mdv.a)
  
  exp.var <- 1 - ( mdv.r / mdv.a)
  
  return(exp.var)
  
}

# # # # # # # # # # # # # # # # # #
# previous model comparisons -----
# # # # # # # # # # # # # # # # # #


compareSimpleModels <- function(N=9,AICfun=AIC,verbosity=1) {
  
  df <- getData()

  single <- fitSingleLimitModels(df)
  # Lx_MSE <- single$Xlim$MSE[['Lx']]
  # Ly_MSE <- single$Ylim$MSE[['Ly']]
  
  double <- fitTwoLimitModel(df)
  # LxLy_MSE <- double$MSE
  
  # MSE_o1 <- c( 'LxLy'=LxLy_MSE, 'Lx'=Lx_MSE, 'Ly'=Ly_MSE )
  # MSE_o2 <- c( 'Lx'=Lx_MSE, 'Ly'=Ly_MSE, 'LxLy'=LxLy_MSE )
  
  if (verbosity > 0) {

    cat('\nFITTED MODEL LIMIT PARAMETERS:\n\n')
    cat(sprintf('single Lx: %0.2f cm\n',                  single$Xlim$par['Lx']))
    cat(sprintf('single Ly: %0.2f s\n',                   single$Ylim$par['Ly']))
    cat(sprintf('combined Lx: %0.2f cm and Ly: %0.2f s\n',double$par['Lx'],double$par['Ly']))
    
  }

  MSEs <- c( 'LxLy'=double$MSE, 'Lx'=single$Xlim$MSE[['Lx']], 'Ly'=single$Ylim$MSE[['Ly']] )

  if (verbosity > 0) {
    cat('\nmodel MSEs:\n')
    print(MSEs)
  }

  # the AIC parameters are the same for both models, except the MSEs
  AICs <- AICfun(MSE=MSEs, k=c(2,1,1), N=N)
  
  if (verbosity > 0) {
    cat('\nmodel AICs:\n')
    print(AICs)
  }

  # rLL <- relativeLikelihood(AICs[which(names(AICs) %in% c('Lx','Ly'))])
  # 
  # if (verbosity > 0) {
  #   cat('\nsingle limit model relative likelihoods:\n')
  #   print(rLL)
  # }

  rLL <- relativeLikelihood(AICs)
  
  if (verbosity > 0) {
    cat('\nALL model relative likelihoods:\n')
    print(rLL)
  }
  
}


compareAllModels <- function(N=9,AICfun=AIC,verbosity=1) {
  
  df <- getData()
  
  single <- fitSingleLimitModels(df)

  double <- fitTwoLimitModel(df)

  MSEs <- c( 'Lx.Ly'=double$MSE, 'Lx'=single$Xlim$MSE[['Lx']], 'Ly'=single$Ylim$MSE[['Ly']] )
  Lx <- c(double$par['Lx'], single$Xlim$par['Lx'], NA)
  Ly <- c(double$par['Ly'], NA, single$Ylim$par['Ly'])
  
  # individual limits for one parameter, but no limits for the other:
  
  if (file.exists('data/Lx9_and_Ly9.csv')) {
    fit9slm <- read.csv('data/Lx9_and_Ly9.csv')
  } else {
    fit9slm <- fitNineSingleLimitModels(df)
    write.csv(fit9slm, file='data/Lx9_and_Ly9.csv', row.names = F)
  }
  
  MSEs <- c(MSEs, 'Lx9'=mean(fit9slm$Lx_MSE), 'Ly9'=mean(fit9slm$Ly_MSE))
  Lx  <-  c(Lx, mean(fit9slm$Lx), NA)
  Ly  <-  c(Ly, NA, mean(fit9slm$Ly))
  
  # two models with individual limits on one parameter, but a group limit on the other
  
  if (file.exists('data/Lx9.Ly.Rdata')) {
    load('data/Lx9.Ly.Rdata')
  } else {
    Lx9.Ly <- fitNineXlimitsSingleYlimit(df)
    save(Lx9.Ly, file='data/Lx9.Ly.Rdata')
  }
  
  MSEs <- c(MSEs, 'Lx9.Ly'=Lx9.Ly$MSE)
  Lx <- c(Lx, mean(Lx9.Ly$par[which(names(Lx9.Ly$par) != 'Ly')]))
  Ly <- c(Ly, Lx9.Ly$par['Ly'])
  
  if (file.exists('data/Lx.Ly9.Rdata')) {
    load('data/Lx.Ly9.Rdata')
  } else {
    Lx.Ly9 <- fitNineYlimitsSingleXlimit(df)
    save(Lx.Ly9, file='data/Lx.Ly9.Rdata')
  }
  
  MSEs <- c(MSEs, 'Lx.Ly9'=Lx.Ly9$MSE)
  Ly <- c(Ly, mean(Lx.Ly9$par[which(names(Lx.Ly9$par) != 'Lx')]))
  Lx <- c(Lx, Lx.Ly9$par['Lx'])
  
  # and finally, each participant gets their own indidivual double-limit fit:
  
  if (file.exists('data/Lx9.Ly9.Rdata')) {
    load('data/Lx9.Ly9.Rdata')
  } else {
    Lx9.Ly9 <- fitEighteenLimits(df)
    save(Lx9.Ly9, file='data/Lx9.Ly9.Rdata')
  }
  
  MSEs <- c(MSEs, 'Lx9.Ly9'=Lx9.Ly9$MSE)
  Lx   <- c(Lx, mean(Lx9.Ly9$fits$Lx))
  Ly   <- c(Ly, mean(Lx9.Ly9$fits$Ly))
  
  # the AIC parameters are the same for both models, except the MSEs
  AICs <- AICfun(MSE=MSEs, k=c(2,1,1,9,9,10,10,18), N=N)
  
  # if (verbosity > 0) {
  #   cat('\nmodel AICs:\n')
  #   print(AICs)
  # }
  
  # rLL <- relativeLikelihood(AICs[which(names(AICs) %in% c('Lx','Ly'))])
  # 
  # if (verbosity > 0) {
  #   cat('\nsingle limit model relative likelihoods:\n')
  #   print(rLL)
  # }
  
  rLL <- relativeLikelihood(AICs)
  
  # if (verbosity > 0) {
  #   cat('\nALL model relative likelihoods:\n')
  #   print(rLL)
  # }
  
  return(data.frame('model'=names(MSEs),Lx,Ly,MSEs,'k'=c(2,1,1,9,9,10,10,18),AICs,'likelihood'=rLL))
  
  
}


# **********************
# getting the data -----
# **********************

#source('R/retrace_onePass_V4.R')

getData54 <- function(illusionMinimum=0) {
  
  df <- summarizeTraceBoundsV4()
  
  df$speed <- NA
  
  # convert speed to pass duration
  df$speed[which(df$externalspeed == 0.125)] <- (13.5/4)
  df$speed[which(df$externalspeed == 0.167)] <- (13.5/3)
  
  # only get substantial illusion strengths:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$initialdirection_mean > illusionMinimum & df$initialdirection_mean < 100),]
  }
  
  # add slopes conveniently:
  #df$angle <- ((90-df$initialdirection_mean)/180)*pi
  df$angle <- df$initialdirection_mean
  df$sin.a <- sin(df$angle)
  df$cos.a <- cos(df$angle)
  df$slope <- df$sin.a / df$cos.a
  
  df$X <- df$resetX_mean #* 13.5
  df$Y <- df$resetY_mean #* 13.5
  
  df$X.sd <- df$resetX_sd #* 13.5
  df$Y.sd <- df$resetY_sd #* 13.5
  
  df$RT <- sqrt(df$X^2 + df$Y^2) / df$speed
  
  df$Vi <- df$internalspeed
  df$Ve <- round(0.5 / df$externalspeed)
  
  df <- df[,c('participant','X', 'Y', 'X.sd', 'Y.sd', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve')]
  
  return(df)
  
}

getDataTrials <- function(illusionMinimum=0, illusionMaximum=90) {
  
  # load re-trace resets and illusion strengths
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)
  
  # remove trials without reset:
  df <- df[!is.na(df$resetX),]
  
  # convert speed to pass duration
  df$speed <- 13.5/round(0.5/df$externalspeed)
  
  # only get illusion strengths within a reasonable range:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$illusionstrength > illusionMinimum),]
  }
  if (is.numeric(illusionMaximum)) {
    df <- df[which(df$illusionstrength < illusionMaximum),]
  }
  
  
  # add slopes conveniently:
  #df$angle <- ((df$initialdirection)/180)*pi
  df$angle <- (pi/2) - df$initialdirection
  df$sin.a <- sin(df$angle)
  df$cos.a <- cos(df$angle)
  df$slope <- df$sin.a / df$cos.a
  
  # get coordinates in centimeters:
  df$X <- df$resetX 
  df$Y <- df$resetY 
  
  # get reset time in seconds:
  df$RT <- sqrt(df$X^2 + df$Y^2) / df$speed
  
  # get internal/external speeds in cm/s (should give the same ratios as dva/s):
  df$Vi <- df$internalspeed
  df$Ve <- round(0.5 / df$externalspeed)
  
  
  df$unMu <- df$slope * df$Y
  
  # this seems incorrect:
  #df$unRT <- pmax(0,df$Y-(df$X/df$slope)) + pmin(df$Y*df$slope,df$X/df$slope)
  
  # also wrong:
  #df$unRT <- sqrt(df$X^2 + (df$X*df$slope)^2) + pmax(0,df$Y-(df$X/df$slope))
  
  df$unRT <- ( df$Y + pmax(0, (sqrt(df$X^2 +(df$X*df$slope)^2) - (df$X*df$slope) ) ) ) / df$speed
  
  # select only useful columns:
  df <- df[,c('participant','X', 'Y', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve', 'unMu', 'unRT')]
  
  return(df)
  
}


binData <- function(pdf, bins) {
  
  orderTrials <- order(pdf$angle)
  #orderTrials <- order(atan2(pdf$Y,pdf$X))
  
  bin_edges <- round(seq(1, length(orderTrials), length.out=bins+1))
  
  pdf$bin <- NA
  
  for (bin_no in c(1:bins)) {
    lo <- bin_edges[bin_no]
    hi <- bin_edges[bin_no+1]-1
    pdf$bin[orderTrials[lo:hi]] <- bin_no
  }
  
  pdf <- aggregate(cbind(participant, X, Y, RT, speed, angle) ~ bin, data=pdf, FUN=mean)
  
  pdf$sin.a <- sin(pdf$angle)
  pdf$cos.a <- cos(pdf$angle)
  pdf$slope <- pdf$sin.a / pdf$cos.a
  
  # slope, sin.a, cos.a
  
  pdf <- pdf[,c('participant','X', 'Y', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a')]
  
  return(pdf)
  
}


# # # # # # # # # # # # # # #
# participant fit table -----
# # # # # # # # # # # # # # #

participantFitTable <- function(bin.it=FALSE) {
  
  participant <- c()
  data.points <- c()
  
  parameters <- list('Xl'=c(),
                     'Yl'=c(),
                     'Tl'=c(),
                     'XlYl'=c(),   # these will have multiple values?
                     'XlTl'=c(),
                     'YlTl'=c())
  MSE <- list('Xl'=c(),
              'Yl'=c(),
              'Tl'=c(),
              'XlYl'=c(),
              'XlTl'=c(),
              'YlTl'=c())
  
  AIC <- list('Xl'=c(),
              'Yl'=c(),
              'Tl'=c(),
              'XlYl'=c(),
              'XlTl'=c(),
              'YlTl'=c())
  
  pvar <- list('Xl'=c(),
               'Yl'=c(),
               'Tl'=c(),
               'XlYl'=c(),
               'XlTl'=c(),
               'YlTl'=c())
  
  # could add an LxLyLt model as well?
  # mjeah... no
  
  df <- getDataTrials()
  
  participants <- unique(df$participant)
  
  for (ppno in participants) {
    
    pdf <- df[which(df$participant == ppno),]
    
    if (bin.it) {
      bins <- floor(dim(pdf)[1] / 5)
      pdf <- binData(pdf, bins=bins)
    }
    
    participant <- c(participant, ppno)
    data.points <- c(data.points, dim(pdf)[1])
    
    singleFits <- fitSingleLimitModels(pdf)
    
    print(ppno)
    #print(singleFits)
    
    for (m in c('X','Y','T')) {
      ms <- singleFits[[sprintf('%slim',m)]]
      key <- sprintf('%sl',m)
      # parameters, MSE, AIC, pvar
      parameters[[key]] <- c(parameters[[key]], sprintf('%sl: %0.3f',m,ms$par))
      MSE[[key]] <- c(MSE[[key]], ms$MSE )
      AIC[[key]] <- c(AIC[[key]], AIC(MSE=ms$MSE, k=1, N=dim(pdf)[1]) )
      
      actual = matrix(0, nrow=dim(pdf)[1], ncol=2)
      actual[,1] <- pdf$X
      actual[,2] <- pdf$Y
      
      if (m == 'X') {
        predicted = as.matrix(XlimResets(par=ms$par, data=pdf))
      }
      if (m == 'Y') {
        predicted = as.matrix(YlimResets(par=ms$par, data=pdf))
      }
      if (m == 'T') {
        predicted = as.matrix(TlimResets(par=ms$par, data=pdf))
      }
      
      
      expvar <- mdexplvar(actual = actual,
                          predicted = predicted,
                          ddof = 0)
      #print(expvar)
      pvar[[key]] <- c(pvar[[key]], expvar)
      
    }
    
    doubleFits <- fitTwoLimitModels(pdf)
    
    for (m in c('XY','XT','YT')) {
      ms <- doubleFits[[m]]
      key <- paste(paste0(unlist(strsplit(m,'')), sep='l'), sep='', collapse='')
      # parameters, MSE, AIC, pvar
      parstring <- ''
      #print(ms)
      for (parn in unlist(strsplit(m,''))) {
        #print(ms$par)
        parstring <- sprintf('%s%sl: %0.3f ', parstring, parn, ms$par[[sprintf('L%s',tolower(parn))]])
      }
      #print(parstring)
      #sprintf('%sl: %0.3f',m,ms$par)
      parameters[[key]] <- c(parameters[[key]], parstring)
      MSE[[key]] <- c(MSE[[key]], ms$MSE )
      AIC[[key]] <- c(AIC[[key]], AIC(MSE=ms$MSE, k=2, N=dim(pdf)[1]) )
      
      actual = matrix(0, nrow=dim(pdf)[1], ncol=2)
      actual[,1] <- pdf$X
      actual[,2] <- pdf$Y
      
      if (m == 'XY') {
        predicted = as.matrix(XYlimResets(par=ms$par, data=pdf))
      }
      if (m == 'XT') {
        predicted = as.matrix(XTlimResets(par=ms$par, data=pdf))
      }
      if (m == 'YT') {
        predicted = as.matrix(YTlimResets(par=ms$par, data=pdf))
      }
      
      
      expvar <- D2explvar(actual = actual,
                          predicted = predicted,
                          ddof = 0)
      #print(expvar)
      pvar[[key]] <- c(pvar[[key]], expvar)
      
    }
    
  }
  
  fitTable <- data.frame('participant'=participant,
                         'datapoints' =data.points )
  #                        'parameters' =parameters,
  #                        'MSE'=MSE,
  #                        'AIC'=AIC,
  #                        'explained variance'=pvar
  
  for (mod in c('Xl','Yl','Tl','XlYl','XlTl','YlTl')) {
    fitTable[sprintf('%s.parameters',mod)] <- parameters[[mod]]
    fitTable[sprintf('%s.MSE',mod)] <- MSE[[mod]]
    fitTable[sprintf('%s.AIC',mod)] <- AIC[[mod]]
    fitTable[sprintf('%s.expl.var',mod)] <- pvar[[mod]]
  }

  return(fitTable)
  
}

plotDataPatterns <- function() {
  
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
                      n=c(8/.05, 13.5/.05),
                      from=c(0,0),
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
       main=sprintf('distribution of %d reset points', dim(df)[1]),
       pch=1, col='#AAAAAA7D',
       xlim=c(-1,8), ylim=c(-1,13.5),
       xlab='',ylab='',
       asp=1, ax=F, bty='n')
  nlevels <- 9
  top <- max(dens2d$z)
  levels <- seq(top/nlevels,top,top/nlevels)
  levels <- levels[1:(length(levels)-1)]
  contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
          add=TRUE,
          levels=levels,
          asp=1, ax=F, bty='n',
          #col=gray.colors(n=13,start=0.9, end=0),
          col=hcl.colors(n=nlevels, palette='SunsetDark', rev=TRUE),
          drawlabels=FALSE, lw=2)
  # filled.contour(x=dens2d$x, y=dens2d$y, z=dens2d$z,
  #                xlim=c(0,8), ylim=c(0,13.5),
  #                xlab='',ylab='',
  #                #add=TRUE,
  #                levels=seq(top/nlevels,top,top/nlevels), 
  #                asp=1, ax=F, bty='n',
  #                #col=gray.colors(n=13,start=0.9, end=0),
  #                col=hcl.colors(n=nlevels, palette='SunsetDark', rev=TRUE))
  
  
  axis(side=1, at=c(0,4,8))
  axis(side=2, at=seq(0,13.5,length.out = 4))
  
  # # PARTICIPANT 3 for X limit example
  # 
  # df4 <- df[which(df$participant == 3),]
  # 
  # plot(df4$X, df4$Y, pch=1, col='#AAAAAA7D',
  #      main='participant 3',
  #      xlim=c(-1,8), ylim=c(-1,13.5),
  #      xlab='',ylab='',
  #      asp=1, ax=F, bty='n')
  # 
  # bins <- floor(dim(df4)[1] / 5)
  # #bins <- 10
  # bdf4 <- binData(df4, bins=bins)
  # 
  # points(bdf4$X, bdf4$Y, pch=19, col=colors$yorkred$s)
  # 
  # axis(side=1, at=c(0,4,8))
  # axis(side=2, at=seq(0,13.5,length.out = 4))
  # 
  # # PARTICIPANT 6 for T limit example
  # 
  # df10 <- df[which(df$participant == 6),]
  # 
  # plot(df10$X, df10$Y, pch=1, col='#AAAAAA7D',
  #      main='participant 6',
  #      xlim=c(-1,8), ylim=c(-1,13.5),
  #      xlab='',ylab='',
  #      asp=1, ax=F, bty='n')
  # 
  # bins <- floor(dim(df10)[1] / 5)
  # #bins <- 10
  # bdf10 <- binData(df10, bins=bins)
  # 
  # points(bdf10$X, bdf10$Y, pch=19, col=colors$yorkred$s)
  # 
  # axis(side=1, at=c(0,4,8))
  # axis(side=2, at=seq(0,13.5,length.out = 4))
  
}

bootStrapFits <- function(bootstraps=1000, useParticipants=c(2,3,4,5,6,9,10,11)) {
  
  colors=getColors()
  
  #layout(matrix(c(1,1,2,1,1,3),nrow=2,ncol=3,byrow=TRUE))
  
  # get resets per trial
  # remove trials without resets or out of range
  df <- getDataTrials()
  df <- df[which(df$participant %in% useParticipants),]
  df <- df[which(!is.na(df$X)),]
  df <- df[which(df$X > 0),]
  df <- df[which(df$Y > 0),]
  
  # get starting points for fitting:
  Lx=seq(0, 8, length.out = 41)
  Lt=seq(0, 4, length.out = 41)
  
  # make them into data frames:
  searchgridXlim <- expand.grid('Lx'=Lx)
  searchgridTlim <- expand.grid('Lt'=Lt)
  searchgridXTlim <- expand.grid('Lx'=Lx,'Lt'=Lt)
  
  # get MSE for points in search grid:
  XlimMSE <- apply(searchgridXlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=XlimResets)
  TlimMSE <- apply(searchgridTlim,FUN=resetMSE,MARGIN=c(1),data=df,fitFUN=TlimResets)
  LxLtMSE <- apply(searchgridXTlim,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=XTlimResets)
  
  # get the best points in the overall grid to start each bootstrapped fit:
  topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[c(1)],])
  topgridTlim <- data.frame('Lt'=searchgridTlim[order(TlimMSE)[c(1)],])
  topgridXTlim <- searchgridXTlim[order(LxLtMSE)[c(1)],]
  
  # print(topgridXlim)
  # print(topgridTlim)
  # print(topgridXTlim)

  # set up bootstrapping to weight participants equally:
  idx_list <- list()
  participants <- unique(df$participant)
  bs_n <- 144
  
  for (ppno in c(1:length(participants))) {
    idx_list[[ppno]] <- which(df$participant == participants[ppno])
    bs_n <- min(bs_n, length(which(df$participant == participants[ppno])))
  }
  
  print(bs_n)
  
  # store results:
  Xlim_X    <- c()
  Xlim_MSE  <- c()
  Tlim_T    <- c()
  Tlim_MSE  <- c()
  XTlim_X   <- c()
  XTlim_T   <- c()
  XTlim_MSE <- c()
  
  for (bs in c(1:bootstraps)) {
    
    # re-sample data
    bs_idx <- c()
    for (ppidx in c(1:length(idx_list))) {
      bs_idx <- c(bs_idx, sample(x = idx_list[[ppidx]], size = bs_n, replace = TRUE))
    }
    bs_df <- df[bs_idx,]
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(13.5),
                                  data=bs_df,
                                  fitFUN=XlimResets) )
    #print(allXlimFits)
    
    Xlim_X    <- c(Xlim_X,    allXlimFits$Lx[1])
    Xlim_MSE  <- c(Xlim_MSE,  allXlimFits$value[1])
    
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(4),
                                  data=bs_df,
                                  fitFUN=TlimResets) )
    
    Tlim_T    <- c(Tlim_T,    allTlimFits$Lt[1])
    Tlim_MSE  <- c(Tlim_MSE,  allTlimFits$value[1])
    
    allXTlimfits <- do.call("rbind",
                            apply( topgridXTlim,
                                   MARGIN=c(1),
                                   FUN=optimx,
                                   fn=resetMSE,
                                   method='L-BFGS-B',
                                   lower=c(0,0),
                                   upper=c(8,4),
                                   data=bs_df,
                                   fitFUN=XTlimResets) )
    
    XTlim_X    <- c(XTlim_X,    allXTlimfits$Lx[1])
    XTlim_T    <- c(XTlim_T,    allXTlimfits$Lt[1])
    XTlim_MSE  <- c(XTlim_MSE,  allXTlimfits$value[1])
    
    cat(sprintf('bootstrap %d / %d\n',bs, bootstraps))
    
  }
  
  bs_fits <- data.frame(  Xlim_X, 
                          Xlim_MSE, 
                          Tlim_T, 
                          Tlim_MSE, 
                          XTlim_X, 
                          XTlim_T, 
                          XTlim_MSE )
  
  write.csv(bs_fits, 'data/bootstrapped_fits.csv', quote = FALSE, row.names = FALSE)
  
  return(bs_fits)
  
}

plotBootstrappedFits <- function(target='inline') {
 
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/FigY_modeled_limits.pdf',onefile=TRUE,width=5,height=4)
  }
  if (target == 'svg') {
    svglite(file='doc/FigY_modeled_limits.svg',width=5,height=4)
  }
  
  
  colors=getColors()
  layout(matrix(c(1,2),nrow=1,ncol=2,byrow=TRUE))
  
  # get resets per trial
  # remove trials without resets or out of range
  df <- getDataTrials()
  df <- df[which(!is.na(df$X)),]
  df <- df[which(df$X > 0),]
  df <- df[which(df$Y > 0),]
  
  
  bs_fits <- read.csv('data/bootstrapped_fits.csv', stringsAsFactors = FALSE)
  
  plot(x=df$X, y=df$Y,
       main=sprintf('X limit model', dim(df)[1]),
       pch=1, col='#AAAAAA7D',
       xlim=c(0,8), ylim=c(0,13.5),
       xlab='',ylab='',
       asp=1, ax=F, bty='n')
  
  X_qs <- quantile(bs_fits$Xlim_X, probs = c(0.025, 0.500, 0.975))
  #print(X_qs)
  
  polygon(x=c(X_qs[c(1,3)],rev(X_qs[c(1,3)])),
          y=c(0,0,13.5,13.5),
          col=colors$yorkred$t,
          border = NA)
  lines(x=c(X_qs[2],X_qs[2]),y=c(0,13.5),
        col=colors$yorkred$s)
  
  axis(side=1, at=c(0,4,8))
  axis(side=2, at=seq(0,13.5,length.out = 4))
  
  
  
  plot(x=df$X, y=df$Y,
       main=sprintf('T limit model', dim(df)[1]),
       pch=1, col='#AAAAAA7D',
       xlim=c(0,8), ylim=c(0,13.5),
       xlab='',ylab='',
       asp=1, ax=F, bty='n')
  
  T_qs <- quantile(bs_fits$Tlim_T, probs = c(0.025, 0.500, 0.975))
  #print(T_qs)
  
  arch_X <- cos(seq(0,pi/2,pi/180))
  arch_Y <- sin(seq(0,pi/2,pi/180))
  
  speeds <- 13.5/c(3,4)
  
  for (speed_no in c(1:length(speeds))) {
    speed <- speeds[speed_no]
    pX <- c(arch_X * T_qs[1] * speed, rev(arch_X * T_qs[3] * speed))
    pY <- c(arch_Y * T_qs[1] * speed, rev(arch_Y * T_qs[3] * speed))
    polygon(pX,pY,
            col=colors$yorkred$t,
            border = NA)
    lines(x = arch_X * T_qs[2] * speed,
          y = arch_Y * T_qs[2] * speed,
          col=colors$yorkred$s,
          lty=speed_no)
    
    
  }
  
  axis(side=1, at=c(0,4,8))
  axis(side=2, at=seq(0,13.5,length.out = 4))
  
  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}


# LIKELIHOOD MODELS -----


# ******************************************
# normal probability density functions -----
# ******************************************


ToffsetGaussianLikelihood <- function(par,data) {
  
  # time limit
  
  mT <- par['mTn']
  sT <- par['sTn']
  
  L <- (1 / (sT * sqrt(2 * pi)) ) * exp( -0.5 * ( ( data$RT - mT) / sT )^2 )
  
  return(data.frame(L))
  
}

XoffsetGaussianLikelihood <- function(par,data) {
  
  # space limit
  
  mX <- par['mXn']
  sX <- par['sXn']
  
  L <- (1 / (sX * sqrt(2 * pi)) ) * exp( -0.5 * ( (data$X-mX) / sX )^2 )
  
  return(data.frame(L))
  
}

# *****************************************
# gamma probability density functions -----
# *****************************************

# gamma distributions have a 'shape' parameter that puts them
# on a continuum that spans exponential and normal distributions  

XoffsetGammaLikelihood <- function(par, data) {
  
  sX <- par['sXg']
  rX <- par['rXg']
  oX <- par['oXg']
  
  L <- dgamma( data$X - oX, shape=sX, rate=rX )
  # log(0) will be -Inf, not a good probability to maximize
  #L[which(is.na(L) | is.infinite(L) | L==0)] <- 1e-10
  
  return(data.frame(L))
  
}

ToffsetGammaLikelihood <- function(par, data) {
  
  sT <- par['sTg']
  rT <- par['rTg']
  oT <- par['oTg']
  
  # hmmm... this is actually done in centimeters ?
  #L <- dgamma( sqrt(data$X^2 + data$Y^2) - (oT * data$speed), shape=sT, rate=rT  )
  # let's switch it to seconds!
  L <- dgamma( data$RT - oT, shape=sT, rate=rT  )
  # log(0) will be -Inf, not a good probability to maximize
  #L[which(is.na(L) | is.infinite(L) | L==0)] <- 1e-10
  
  return(data.frame(L))
  
}

XGammaLikelihood <- function(par, data) {
  
  sX <- par['sXg']
  rX <- par['rXg']

  L <- dgamma( data$X, shape=sX, rate=rX )
  # log(0) will be -Inf, not a good probability to maximize
  #L[which(is.na(L) | is.infinite(L) | L==0)] <- 1e-10
  
  return(data.frame(L))
  
}

TGammaLikelihood <- function(par, data) {
  
  sT <- par['sTg']
  rT <- par['rTg']

  L <- dgamma( data$RT, shape=sT, rate=rT  )
  # log(0) will be -Inf, not a good probability to maximize
  #L[which(is.na(L) | is.infinite(L) | L==0)] <- 1e-10
  
  return(data.frame(L))
  
}


# **********************
# UNCOUPLED MODELS -----
# *********************

# the idea here is that the path the illusion follows is limited by an X-offset
# so it will be restricted in the X coordinate, 
# for which we can fit a likelihood distribution on the X coordinate

# but then the resets occur along this trajectory at random times
# for which we can fit a likelihood distribution on the T (or Y?) coordinate

# the distributions could be Gaussian (as a default) or Gamma
# if Gamma fits better than Gaussian, it allows estimating how
# much it matters that the underlying process might be Poisson in nature:
# the distribution of wait-times between Poisson-generated events is exponential
# (we assume this distribution holds even with random double-drift offsets)

# unlike for the limit functions, two likelihood distributions (on X and T) can be combined
# (e.g. there is a normal distribution around the path, and gamma distributed reset times)

# while we do fit single coordinate likelihood distributions here,
# (a priori) the better approach seems to be to use a distribution on both X and on T


# we need to specify parameter names, where I will use 
# x (for x coordinates) or
# t (for t coordinates), followed by either
# N (for Gaussian/Normal)
# + m for mu (mean)
# + s for sigma (standard deviation), or by
# G (for Gamma)
# + s for shape
# + r for rate

# additionally there could be limits (offsets from 0) for each coordinate:
# Lx
# Lt
# as in the simpler models

# BUT, for the Normal distributions, this is already given by mu
# AND, for Gamma distributions, I'm not sure if any limit other than T=0
# makes any sense (given resets are supposedly uncoupled from perception here)
# however an offset on a model with a Gamma-shaped likelihood on X coordinates 
# ... could make sense?

# so I'm not implementing any additional parameters (for now)

# so we could have a single Gaussian likelihood on X coordinates:
# > names(par)
# c('xNm','xNs')
# or two likelihoods on both X and T:
# > names(par)
# c('xNm','xNs','tGs','tGr')

# all of these parameters can be provided
# (the naming scheme is meant to ensure they won't overwrite eachother)
# but each function returning likelihoods only uses the parameters it needs
# (note that Gamma distributions take 1 more parameter than Normal distributions)


# *******************************
# uncoupled Gaussian models -----
# *******************************


# this model should be equivalent to an X limit

XuncoupledGaussianLikelihood <- function(par, data) {
  
  m <- par['mXn'] # mu (mean), also: ~X limit
  s <- par['sXn'] # sigma (sd)
  
  #   df$unMu <- df$slope * df$Y
  mu <- pmin(m, data$unMu)
  L <- (1 / (s * sqrt(2 * pi)) ) * exp( -0.5 * ( (data$X-mu) / s )^2 )
  
  return(data.frame(L))
  
}

# this model should be equivalent to a T limit

TuncoupledGaussianLikelihood <- function(par, data) {
  
  m <- par['mTg'] # mu (mean), also: ~T limit
  s <- par['sTg'] # sigma (sd)
  
  # rt <- max(0,data$Y-(data$x/data$slope))
  # rt <- rt + min(data$Y*data$slope,data$X/data$slope)
  
  L <- (1 / (s * sqrt(2 * pi)) ) * exp( -0.5 * ( ( data$unRT - m) / s )^2 )
  
  return(data.frame(L))
  
}

# this model combines the above two models (literally)

XTuncoupledGaussianLikelihood <- function(par,data) {
  
  LX <- XuncoupledGaussianLikelihood(data, par)
  LT <- TuncoupledGaussianLikelihood(data, par)
  
  L <- LX$L * LT$L
  
  return(data.frame(L))
  
}

# ****************************
# uncoupled Gamma models -----
# ****************************


# this model uses a Gamma distribution on the X coordinate only

XuncoupledGammaLikelihood <- function(par,data) {
  
  s <- par['sXg'] # shape
  r <- par['rXg'] # rate
  
  mu <- pmin(m, data$unMu)
  
  L <- dgamma( mu, shape=s, rate=r  )
  
  return(data.frame(L))
  
}

# this model uses a Gamma distribution on the T coordinate only

TuncoupledGammaLikelihood <- function(par,data) {
  
  s <- par['sTg'] # shape
  r <- par['rTg'] # rate
  
  # rt <- pmax(0,data$Y-(data$X/data$slope))
  # rt <- rt + pmin(data$Y*data$slope,data$X/data$slope)
  
  L <- dgamma( data$unRT, shape=s, rate=r )
  
  return(data.frame(L))
  
}

# this model combines the two models above (literally)

XTuncoupledGammaLikelihood <- function(par,data) {
  
  LX <- XuncoupledGammaLikelihood(data, par)
  LT <- TuncoupledGammaLikelihood(data, par)
  
  L <- LX$L * LT$L
  
  return(data.frame(L))
  
}

# *********************************
# crossover model likelihoods -----
# *********************************

XgaussianTgammaLikelihood <- function(par,data) {
  
  LX <- XoffsetGaussianLikelihood(par, data)
  LT <- TGammaLikelihood(par, data)
  
  L <- LX$L * LT$L
  
  return(data.frame(L))
  
}

XgammaTgaussianLikelihood <- function(par,data) {
  
  LX <- XGammaLikelihood(par, data)
  LT <- ToffsetGaussianLikelihood(par, data)
  
  L <- LX$L * LT$L
  
  return(data.frame(L))
  
}


# **********************
# model likelihood -----
# **********************

resetLogLikelihood <- function(par,data,fitFUN) {
  
  L <- fitFUN(par,data)$L
  
  # only positive, numeric values should be used, so
  # replace other values with a very small value:
  L[which(is.na(L) | is.infinite(L) | L<=0)] <- 1e-16
  
  return(sum(log(L)))
  
}

# for comparison we have orthogonal MSE functions:

XorthogonalMSE <- function(par,data,fitFUN) {
  
  resets <- fitFUN(par,data)
  
  MSE <- mean( (resets$X - data$X)^2 )
  
  return(MSE)
  
}

TorthogonalMSE <- function(par,data,fitFUN) {
  
  resets <- fitFUN(par,data)
  
  d <- sqrt( data$Y^2 + data$X^2 )
  RT <- d / data$speed
  
  MSE <- mean( (RT - par['Lt'])^2 )
  
  return(MSE)
  
}

# ******************************
# fit / plot single models -----
# ******************************

# this first function fits models that fit single limits/offsets/distributions
# i.e. on one coordinate only, for now this is the X and T coordinates
# minimizing errors calculated orthogonal to the limit
# *should* be the same as maximizing a Gaussian likelihood distribution
# which is what the orthogonal and normal models compare (it is the same)
# and then there is an offset gamma distribution for comparison

allTheSingleModels <- function(df, doModels=c( 'orthogonal'  = TRUE,
                                               'normal'      = TRUE,
                                               'offsetgamma' = TRUE,
                                               'gamma'       = TRUE )) {
  
  outputlist <- list()
  
  # ********************************
  # first the orthogonal errors
  # ********************************
  
  if (doModels['orthogonal']) {
    
    Lx=seq(0, 8, length.out = 41)
    Lt=seq(0, 4, length.out = 41)
    
    # make them into data frames:
    searchgridXlim <- expand.grid('Lx'=Lx)
    searchgridTlim <- expand.grid('Lt'=Lt)
    
    # get MSE for points in search grid:
    XlimMSE <- apply(searchgridXlim,FUN=XorthogonalMSE,MARGIN=c(1),data=df,fitFUN=XlimResets)
    TlimMSE <- apply(searchgridTlim,FUN=TorthogonalMSE,MARGIN=c(1),data=df,fitFUN=TlimResets)
    
    # get 4 best points in the grid:
    topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[c(1,3,5)],])
    topgridTlim <- data.frame('Lt'=searchgridTlim[order(TlimMSE)[c(1,3,5)],])
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=XorthogonalMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(13.5),
                                  data=df,
                                  fitFUN=XlimResets) )
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=TorthogonalMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(4),
                                  data=df,
                                  fitFUN=TlimResets) )
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
    winTlimFit <- allTlimFits[order(allTlimFits$value)[1],]
    # print(win[1:3])
    
    winXlimO <- as.numeric(winXlimFit[1])
    names(winXlimO) <- c('Lx')
    winTlimO <- as.numeric(winTlimFit[1])
    names(winTlimO) <- c('Lt')
    
    winXvalO <- as.numeric(winXlimFit[2])
    names(winXvalO) <- c('Lx')
    winTvalO <- as.numeric(winTlimFit[2])
    names(winTvalO) <- c('Lt')
    
    outputlist[['XlimOrth']]=list('par'=winXlimO,'MSE'=winXvalO)
    outputlist[['TlimOrth']]=list('par'=winTlimO,'MSE'=winTvalO)
    
  }
  
  # **********************************
  # then the Gaussian offsets
  # **********************************
  
  if (doModels['normal']) {
    
    # create search "grids":
    mX = seq(0, 8, length.out = 16)
    mT = seq(0, 4, length.out = 16)
    sX = seq(0, 8, length.out = 16)
    sT = seq(0, 4, length.out = 16)
    
    # make them into data frames:
    searchgridXlim <- expand.grid('mXn'=mX, 'sXn'=sX)
    searchgridTlim <- expand.grid('mTn'=mT, 'sTn'=sT)
    
    # get Likelihoods for points in search grid:
    expTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=ToffsetGaussianLikelihood)
    expXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XoffsetGaussianLikelihood)
    
    # get 5 best points in the grid:
    topgridXlim <- searchgridXlim[order(expXlimLLs, decreasing = TRUE)[c(1,5,9)],]
    topgridTlim <- searchgridTlim[order(expTlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # print(topgridXlim)
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c( 0.0, 0.0 ),
                                  upper=c(13.5, 13.5),
                                  control=control,
                                  data=df,
                                  fitFUN=XoffsetGaussianLikelihood,) )
    
    # print(topgridTlim)
    # print(sort(expTlimLLs, decreasing = TRUE)[c(1,3,5)])
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(0, 0),
                                  upper=c(4, 4),
                                  control=control,
                                  data=df,
                                  fitFUN=ToffsetGaussianLikelihood) )
    
    # print(topgridYlim)
    # print(sort(expYlimLLs, decreasing = TRUE)[c(1,3,5)])
    
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
    winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
    
    winXparN <- unlist(winXlimFit[c('mXn','sXn')])
    winTparN <- unlist(winTlimFit[c('mTn','sTn')])
    
    winXvalN <- as.numeric(winXlimFit$value)
    names(winXvalN) <- c('logL')
    winTvalN <- as.numeric(winTlimFit$value)
    names(winTvalN) <- c('logL')
    
    
    outputlist[['XdistNormal']]=list('par'=winXparN,'logL'=winXvalN)
    outputlist[['TdistNormal']]=list('par'=winTparN,'logL'=winTvalN)
    
  }
  
  # **********************************
  # and finally Gamma distributions
  # **********************************
  
  if (doModels['offsetgamma']) {
    
    # create search "grids":
    sX = seq(0, 20, length.out = 16)
    rX = 1/seq(0, 4, length.out = 16)[2:16]
    oX = seq(0,  8, length.out = 16)
    
    sT = seq(0, 20, length.out = 16)
    rT = 1/seq(0, 4, length.out = 16)[2:16]
    oT = seq(0,  8, length.out = 16)
    
    # make them into data frames:
    searchgridXlim <- expand.grid('sXg'=sX, 'rXg'=rX, 'oXg'=oX)
    searchgridTlim <- expand.grid('sTg'=sT, 'rTg'=rT, 'oTg'=oT)
    
    # get MSE for points in search grid:
    gamTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=ToffsetGammaLikelihood)
    gamXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XoffsetGammaLikelihood)
    
    # get 5 best points in the grid:
    topgridXlim <- searchgridXlim[order(gamXlimLLs, decreasing = TRUE)[c(1,5,9)],]
    topgridTlim <- searchgridTlim[order(gamTlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10, 0.0),
                                  upper=c( 20.0,  10.0, 8.0),
                                  control=control,
                                  data=df,
                                  fitFUN=XoffsetGammaLikelihood,) )
    
    # print(topgridTlim)
    # print(sort(expTlimLLs, decreasing = TRUE)[c(1,3,5)])
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10, 0.0),
                                  upper=c( 20.0,  10.0, 4.0),
                                  control=control,
                                  data=df,
                                  fitFUN=ToffsetGammaLikelihood) )
    
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
    winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
    
    winXparG <- unlist(winXlimFit[c('sXg','rXg','oXg')])
    winTparG <- unlist(winTlimFit[c('sTg','rTg','oTg')])
    
    winXvalG <- as.numeric(winXlimFit$value)
    names(winXvalG) <- c('logL')
    winTvalG <- as.numeric(winTlimFit$value)
    names(winTvalG) <- c('logL')
    
    outputlist[['XdistOffGamma']]=list('par'=winXparG,'logL'=winXvalG) 
    outputlist[['TdistOffGamma']]=list('par'=winTparG,'logL'=winTvalG)
    
  }
  
  if (doModels['gamma']) {
    
    # create search "grids":
    sX = seq(0, 20, length.out = 16)
    rX = 1/seq(0, 4, length.out = 16)[2:16]

    sT = seq(0, 20, length.out = 16)
    rT = 1/seq(0, 4, length.out = 16)[2:16]

    # make them into data frames:
    searchgridXlim <- expand.grid('sXg'=sX, 'rXg'=rX)
    searchgridTlim <- expand.grid('sTg'=sT, 'rTg'=rT)
    
    # get MSE for points in search grid:
    gamTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=TGammaLikelihood)
    gamXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XGammaLikelihood)
    
    # get 5 best points in the grid:
    topgridXlim <- searchgridXlim[order(gamXlimLLs, decreasing = TRUE)[c(1,5,9)],]
    topgridTlim <- searchgridTlim[order(gamTlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10),
                                  upper=c( 20.0,  10.0),
                                  control=control,
                                  data=df,
                                  fitFUN=XGammaLikelihood,) )
    
    # print(topgridTlim)
    # print(sort(expTlimLLs, decreasing = TRUE)[c(1,3,5)])
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10),
                                  upper=c( 20.0,  10.0),
                                  control=control,
                                  data=df,
                                  fitFUN=TGammaLikelihood) )
    
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
    winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
    
    winXparG <- unlist(winXlimFit[c('sXg','rXg')])
    winTparG <- unlist(winTlimFit[c('sTg','rTg')])
    
    winXvalG <- as.numeric(winXlimFit$value)
    names(winXvalG) <- c('logL')
    winTvalG <- as.numeric(winTlimFit$value)
    names(winTvalG) <- c('logL')
    
    outputlist[['XdistGamma']]=list('par'=winXparG,'logL'=winXvalG) 
    outputlist[['TdistGamma']]=list('par'=winTparG,'logL'=winTvalG)
    
  }
  
  return( outputlist )
  
}

plotTheSingleModels <- function(df, target='inline') {
  
  modelfunctions <- list('XlimOrth'=XlimResets,
                         'TlimOrth'=TlimResets,
                         'XdistNormal'=XoffsetGaussianLikelihood,
                         'TdistNormal'=ToffsetGaussianLikelihood,
                         'XdistOffGamma'=XoffsetGammaLikelihood,
                         'TdistOffGamma'=ToffsetGammaLikelihood,
                         'XdistGamma'=XGammaLikelihood,
                         'TdistGamma'=TGammaLikelihood)
  
  
  fits <- allTheSingleModels(df)
  
  if (target == 'pdf') {
    pdf(file = 'doc/singleModelFits.pdf', width=12, height=9, bg='white')
  }
  
  layout(mat=matrix(c(1:8),byrow = FALSE,nrow=2,ncol=4))
  
  par(mar=c(4.1,3.1,2.6,0.1))
  
  coords <- c('X','T')
  models <- c('limOrth','distNormal','distOffGamma','distGamma')

  # add 1D distributions as insets
  inset.figs <- list()
  inset.fig.idx <- 1
  p <- c(.25, .80, .65, .95)
  
  for (model in models) {
    for (coord in coords) {
      
      modelname <- sprintf('%s%s',coord,model)
      
      plot(df$X,df$Y,
           main=modelname,xlab='cm',ylab='cm',
           xlim=c(0,9),ylim=c(0,13.5),
           pch=16,col='#00000011',
           bty='n',ax=F,asp=1)
      
      fit <- fits[[modelname]]
      
      par <- fit$par
      
      modelfunction <- modelfunctions[[modelname]] 
      
      if (grepl("lim", modelname, fixed=TRUE)) {
        # line fit(s)
        angle = (seq(5,85)/180)*pi
        speed = rep(4,length(angle)) # really: c(3.375, 4.500)
        data = data.frame(angle, speed)
        data$cos.a <- cos(data$angle)
        data$sin.a <- sin(data$angle)
        data$slope <- data$sin.a / data$cos.a
        
        resets <- modelfunction(par,data)
        
        lines(resets$X, resets$Y, col='red')
        
        
      } else {
        # distribution fits
        stepsize <- 0.2
        X <- seq(0.1,7.9,stepsize)
        Y <- seq(0.15,13.5,stepsize)
        data = expand.grid('X'=X,'Y'=Y)
        data$speed <- 4 # really: c(3.375, 4.500)
        data$RT <- sqrt(data$X^2 + data$Y^2) / data$speed
        
        likelihoods <- modelfunction(par,data)
        
        Z <- matrix(likelihoods$L,
                    ncol=length(Y),
                    nrow=length(X))
        
        pal='Purple-Blue'
        
        image(add=TRUE,
              x=seq(0,8,stepsize),
              y=seq(0,13.5,stepsize),
              z=Z,
              useRaster=TRUE,
              #col = gray.colors(256, rev=TRUE),
              col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
              #lw=0
              )
        
        # make room for the inset figure:
        inset.figs[[inset.fig.idx]] <- c(grconvertX(p[1:2], from="npc", to="ndc"),
                                         grconvertY(p[3:4], from="npc", to="ndc"))
        inset.fig.idx <- inset.fig.idx + 1
        
      }
      
      axis(side=1,at=c(0,4,8))
      axis(side=2,at=c(0,4.5,9,13.5))
      
    }
  }
  
  # now do the inset figures for the distribution models:
  inset.fig.idx <- 1
  
  for (model in models[c(2:4)]) {
    for (coord in coords) {
      if (coord == 'X') {
        vals <- df$X
        xlim <- c(0,8)
        data <- data.frame('X'=seq(0,8,0.25))
      }
      if (coord == 'T') {
        vals <- df$RT
        xlim <- c(0,5)
        data <- data.frame('RT'=seq(0,5,0.25))
      }
      
      

      # create inset figure
      op <- par( fig=inset.figs[[inset.fig.idx]], 
                 new=TRUE, 
                 mar=rep(1, 4), 
                 cex=0.5)
      

      plot(-1000,-1000,
           xlim=xlim,
           ylim=c(0,1),
           bty='n', ax=F)
      polygon(x=c(xlim,rev(xlim)),y=c(0,0,1,1),col='#FFFFFF55',border=NA)
      
      # put a histogram of the data:
      histogram <- hist(vals, breaks=seq(0,20,xlim[2]/12), plot=FALSE)
      idx <- which(histogram$breaks < xlim[2])
      barheights=histogram$density[idx]/max(histogram$density[idx])
      for (id in idx) {
        x <- c(histogram$breaks[c(id,id,id+1,id+1)])+c(0.1,0.1,-0.1,-0.1)
        y <- c(0,barheights[c(id,id)],0)
        polygon(x=x, y=y,
                col='#00000022',border=NA)
      }
      
      # draw a line with the density of the model function
      modelname <- sprintf('%s%s',coord,model)
      fit <- fits[[modelname]]
      par <- fit$par
      dens <- modelfunctions[[modelname]](par,data)$L 
      dens <- dens / max(dens)
      if (coord == 'X') {
        xvals <- data$X
      } else {
        xvals <- data$RT
      }
      
      lines(x=xvals,y=dens,col='purple')

      
      # finalize inset figure
      inset.fig.idx <- inset.fig.idx + 1
      par(op)
    }
  }
  
  if (target %in% c('pdf')) {
    dev.off()
  }
  
}

# *********************************
# fit / plot uncoupled models -----
# *********************************

fitUncoupledModels <- function(df) {
  
  outputlist <- list()
  
  # uncoupled Gaussian X limit/offset:
  
  # build search grid:
  xNm <- seq(0,8,length.out = 19)
  xNs <- seq(0,4,length.out = 19)
  unXgrid <- expand.grid('xNm'=xNm, 'xNs'=xNs)
  
  # get Likelihoods for points in search grid:
  unXlLs <- apply(unXgrid,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XuncoupledGaussianLikelihood)
  
  # get some of the best points in the grid:
  topgridUnX <- unXgrid[order(unXlLs, decreasing = TRUE)[c(1,5,9)],]

  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  allUnXfits <- do.call("rbind",
                         apply( topgridUnX,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c( 0.0, 0.0 ),
                                upper=c( 8.0, 4.0),
                                control=control,
                                data=df,
                                fitFUN=XuncoupledGaussianLikelihood,) )
  
  # pick the best fit:
  winUnXfit <- allUnXfits[order(allUnXfits$value)[1],]
  
  winUnXpar <- unlist(winUnXfit[c('xNm','xNs')])
  
  winUnXval <- as.numeric(winUnXfit$value)
  names(winUnXval) <- c('logL')
  
  outputlist[['UnXnormal']]=list('par'=winUnXpar,'L'=winUnXval)
  
  
  # uncoupled Gamma T distribution:
  
  # build search grid:
  tGs <- seq(0,10,length.out = 31)
  tGr <- seq(0, 5,length.out = 31)
  unTgrid <- expand.grid('tGs'=tGs, 'tGr'=tGr)
  
  # get Likelihoods for points in search grid:
  unTlLs <- apply(unTgrid,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=TuncoupledGammaLikelihood)
  
  # get some of the best points in the grid:
  topgridUnT <- unTgrid[order(unTlLs, decreasing = TRUE)[c(1,5,9)],]
  
  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  allUnTfits <- do.call("rbind",
                        apply( topgridUnT,
                               MARGIN=c(1),
                               FUN=optimx,
                               fn=resetLogLikelihood,
                               method=c('nlminb'),
                               lower=c(   0.0,   0.0 ),
                               upper=c(  10.0,   5.0 ),
                               control=control,
                               data=df,
                               fitFUN=TuncoupledGammaLikelihood,) )
  
  # pick the best fit:
  winUnTfit <- allUnTfits[order(allUnTfits$value)[1],]
  
  winUnTpar <- unlist(winUnTfit[c('tGs','tGr')])
  
  winUnTval <- as.numeric(winUnTfit$value)
  names(winUnTval) <- c('logL')
  
  outputlist[['UnTgamma']]=list('par'=winUnTpar,'L'=winUnTval)
  
  
  # **************************************
  # the combined model:
  # X has an uncoupled normal distribution
  # T has an uncoupled gamma distribution
  # **************************************
  
  # build a search grid:
  xNm <- seq(0,8,length.out = 10)
  xNs <- seq(0,4,length.out = 10)
  tGs <- seq(0,10,length.out = 10)[2:11]
  tGr <- seq(0, 5,length.out = 10)[2:11]
  unXTgrid <- expand.grid('xNm'=xNm, 'xNs'=xNs, 'tGs'=tGs, 'tGr'= tGr)
  
  # get Likelihoods for points in search grid:
  unXTlLs <- apply(unXTgrid,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XgaussianTgammaLikelihood)
  
  # get some of the best points in the grid:
  topgridUnXT <- unXTgrid[order(unXTlLs, decreasing = TRUE)[c(1:10)],]
  
  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  allUnXTfits <- do.call("rbind",
                         apply( topgridUnXT,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c( 0.0, 0.0,  0.0, 0.0 ),
                                upper=c( 8.0, 4.0, 10.0, 5.0 ),
                                control=control,
                                data=df,
                                fitFUN=XgaussianTgammaLikelihood) )
  
  # pick the best fit:
  winUnXTfit <- allUnXTfits[order(allUnXTfits$value)[1],]
   
  winUnXTpar <- unlist(winUnXTfit[c('xNm','xNs','tGs','tGr')])
  
  winUnXTval <- as.numeric(winUnXTfit$value)
  names(winUnXTval) <- c('logL')
   
  outputlist[['UnXnormalTgamma']]=list('par'=winUnXTpar,'L'=winUnXTval)
  
  return(outputlist)
  
}

plotUncoupledModels <- function(df, target='inline') {
  
  modelfits <- fitUncoupledModels(df)
  
  if (target == 'pdf') {
    pdf(file = 'doc/ucoupledModelFits.pdf', width=8, height=4, bg='white')
  }
  
  layout(mat=matrix(c(1,2,3,4),byrow=TRUE,ncol=4,nrow=1))
  
  plot(df$X,df$Y,
       main='resets',xlab='cm',ylab='cm',
       xlim=c(0,9),ylim=c(0,13.5),
       pch=16,col='#00000011',
       bty='n',ax=F,asp=1)
  
  lines(x=c(0,3,3),y=c(0,6,13.5),col='purple',lty=1,lw=3)
  
  axis(side=1,at=c(0,4,8))
  axis(side=2,at=c(0,4.5,9,13.5))
  
  
  # distribution fits
  stepsize <- 0.2
  X <- seq(0.1,7.9,stepsize)
  Y <- seq(0.15,13.5,stepsize)
  data = expand.grid('X'=X,'Y'=Y)
  data$speed <- 4 # really: c(3.375, 4.500)
  data$RT <- sqrt(data$X^2 + data$Y^2) / data$speed
  #data$slope <- data$X/data$Y # this assumes that initial trajectory goes straight to reset point, which is false under the uncoupled conditions...
  data$slope <- mean(df$slope) # we use one constant slope, so that the likelihood distribution can actually be shown
  
  data$unMu <- data$slope * data$Y
  #data$unRT <- sqrt(data$X^2 + (data$X*data$slope)^2) + pmax(0,data$Y-(data$X/data$slope))
  data$unRT <- ( data$Y + pmax(0, (sqrt(data$X^2 +(data$X*data$slope)^2) - (data$X*data$slope) ) ) ) / data$speed
  
  for (model in names(modelfits)) {
    
    print('*****************************')
    print(model)
    print(modelfits[[model]])
    
    plot(df$X,df$Y,
         main=model,xlab='cm',ylab='cm',
         xlim=c(0,9),ylim=c(0,13.5),
         pch=16,col='#00000011',
         bty='n',ax=F,asp=1)
    
    fit <- modelfits[[model]]
    par <- fit$par
    
    modelfunction <- list('UnXnormal'=XuncoupledGaussianLikelihood,
                          'UnTgamma'=TuncoupledGammaLikelihood,
                          'UnXnormalTgamma'=XgaussianTgammaLikelihood)[[model]]
    
    likelihoods <- modelfunction(par,data)
    
    Z <- matrix(likelihoods$L,
                ncol=length(Y),
                nrow=length(X))
    
    pal='Purple-Blue'
    
    image(add=TRUE,
          x=seq(0,8,stepsize),
          y=seq(0,13.5,stepsize),
          z=Z,
          useRaster=TRUE,
          #col = gray.colors(256, rev=TRUE),
          col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
          #lw=0
    )
    
    axis(side=1,at=c(0,4,8))
    axis(side=2,at=c(0,4.5,9,13.5))
    
  }
  
  if (target %in% c('pdf')) {
    dev.off()
  }
  
}

# ****************************
# fit / plot joint model -----
# ****************************

# this first function fits models that fit single limits/offsets/distributions
# i.e. on one coordinate only, for now this is the X and T coordinates
# minimizing errors calculated orthogonal to the limit
# *should* be the same as maximizing a Gaussian likelihood distribution
# which is what the orthogonal and normal models compare (it is the same)
# and then there is an offset gamma distribution for comparison

fitSomeModels <- function(df, jointModel=FALSE, Xnormal=TRUE, Tnormal=TRUE, Xgamma=TRUE, Tgamma=TRUE, Orthogonal=FALSE) {
  
  outputlist <- list()
  
  # ********************************
  # ortogonal models, just because
  # ********************************
  
  if (Orthogonal) {
    
    Lx=seq(0, 8, length.out = 41)
    Lt=seq(0, 4, length.out = 41)
    
    # make them into data frames:
    searchgridXlim <- expand.grid('Lx'=Lx)
    searchgridTlim <- expand.grid('Lt'=Lt)
    
    # get MSE for points in search grid:
    XlimMSE <- apply(searchgridXlim,FUN=XorthogonalMSE,MARGIN=c(1),data=df,fitFUN=XlimResets)
    TlimMSE <- apply(searchgridTlim,FUN=TorthogonalMSE,MARGIN=c(1),data=df,fitFUN=TlimResets)
    
    # get 4 best points in the grid:
    topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[c(1,3,5)],])
    topgridTlim <- data.frame('Lt'=searchgridTlim[order(TlimMSE)[c(1,3,5)],])
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=XorthogonalMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(13.5),
                                  data=df,
                                  fitFUN=XlimResets) )
    
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=TorthogonalMSE,
                                  method='L-BFGS-B',
                                  lower=c(0),
                                  upper=c(4),
                                  data=df,
                                  fitFUN=TlimResets) )
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
    winTlimFit <- allTlimFits[order(allTlimFits$value)[1],]
    # print(win[1:3])
    
    winXlimO <- as.numeric(winXlimFit[1])
    names(winXlimO) <- c('Lx')
    winTlimO <- as.numeric(winTlimFit[1])
    names(winTlimO) <- c('Lt')
    
    winXvalO <- as.numeric(winXlimFit[2])
    names(winXvalO) <- c('Lx')
    winTvalO <- as.numeric(winTlimFit[2])
    names(winTvalO) <- c('Lt')
    
    outputlist[['XlimOrth']]=list('par'=winXlimO,'MSE'=winXvalO)
    outputlist[['TlimOrth']]=list('par'=winTlimO,'MSE'=winTvalO)
    
  }
  
  
  # **********************************
  # the X-Normal distribution
  # **********************************
  
  if (Xnormal) {
    
    # create search "grids":
    mX = seq(0, 8, length.out = 16)
    sX = seq(0, 8, length.out = 16)
    
    # make them into data frames:
    searchgridXlim <- expand.grid('mXn'=mX, 'sXn'=sX)
    
    # get Likelihoods for points in search grid:
    expXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XoffsetGaussianLikelihood)
    
    # get 5 best points in the grid:
    topgridXlim <- searchgridXlim[order(expXlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # print(topgridXlim)
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c( 0.0, 0.0 ),
                                  upper=c(13.5, 13.5),
                                  control=control,
                                  data=df,
                                  fitFUN=XoffsetGaussianLikelihood,) )
    
    
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
    
    winXparN <- unlist(winXlimFit[c('mXn','sXn')])
    
    winXvalN <- as.numeric(winXlimFit$value)
    names(winXvalN) <- c('logL')
    
    
    outputlist[['XdistNormal']]=list('par'=winXparN,'logL'=winXvalN)
    
  }
  
  # **********************************
  # the T-Normal distribution
  # **********************************
  
  if (Tnormal) {
    
    # create search "grids":
    mT = seq(0, 4, length.out = 16)
    sT = seq(0, 4, length.out = 16)
    
    # make them into data frames:
    searchgridTlim <- expand.grid('mTn'=mT, 'sTn'=sT)
    
    # get Likelihoods for points in search grid:
    expTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=ToffsetGaussianLikelihood)
    
    # get best points in the grid:
    topgridTlim <- searchgridTlim[order(expTlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # print(topgridXlim)
    
    # do the actual fitting:
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c( 0.0, 0.0 ),
                                  upper=c( 4.0, 4.0),
                                  control=control,
                                  data=df,
                                  fitFUN=ToffsetGaussianLikelihood,) )
    
    
    
    # pick the best fit:
    winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
    
    winTparN <- unlist(winTlimFit[c('mTn','sTn')])
    
    winTvalN <- as.numeric(winTlimFit$value)
    names(winTvalN) <- c('logL')
    
    
    outputlist[['TdistNormal']]=list('par'=winTparN,'logL'=winTvalN)
    
  }
  
  # **********************************
  # the T-Gamma distributions
  # **********************************
  
  if (Tgamma) {
    
    # create search "grids":
    sT = seq(0, 20, length.out = 16)
    rT = 1/seq(0, 4, length.out = 16)[2:16]
    
    # make them into data frames:
    searchgridTlim <- expand.grid('sTg'=sT, 'rTg'=rT)
    
    # get MSE for points in search grid:
    gamTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=TGammaLikelihood)
    
    # get 5 best points in the grid:
    topgridTlim <- searchgridTlim[order(gamTlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # do the actual fitting:
    allTlimFits <- do.call("rbind",
                           apply( topgridTlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10),
                                  upper=c( 20.0,  10.0),
                                  control=control,
                                  data=df,
                                  fitFUN=TGammaLikelihood) )
    
    
    # pick the best fit:
    winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
    
    winTparG <- unlist(winTlimFit[c('sTg','rTg')])
    
    winTvalG <- as.numeric(winTlimFit$value)
    names(winTvalG) <- c('logL')
    
    outputlist[['TdistGamma']]=list('par'=winTparG,'logL'=winTvalG)
    
  }
  
  # **********************************
  # the X-Gamma distributions
  # **********************************
  
  if (Xgamma) {
    
    # create search "grids":
    sXg = seq(0, 20, length.out = 16)
    rXg = 1/seq(0, 4, length.out = 16)[2:16]
    
    # make them into data frames:
    searchgridXlim <- expand.grid('sXg'=sXg, 'rXg'=rXg)
    
    # get MSE for points in search grid:
    gamXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XGammaLikelihood)
    
    # get 5 best points in the grid:
    topgridXlim <- searchgridXlim[order(gamXlimLLs, decreasing = TRUE)[c(1,5,9)],]
    
    control <- list( 'maximize' = TRUE )
    
    # do the actual fitting:
    allXlimFits <- do.call("rbind",
                           apply( topgridXlim,
                                  MARGIN=c(1),
                                  FUN=optimx,
                                  fn=resetLogLikelihood,
                                  method=c('nlminb'),
                                  lower=c(1e-10, 1e-10),
                                  upper=c( 20.0,  10.0),
                                  control=control,
                                  data=df,
                                  fitFUN=XGammaLikelihood) )
    
    
    # pick the best fit:
    winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
    
    winXparG <- unlist(winXlimFit[c('sXg','rXg')])
    
    winXvalG <- as.numeric(winXlimFit$value)
    names(winXvalG) <- c('logL')
    
    outputlist[['XdistGamma']]=list('par'=winXparG,'logL'=winXvalG)
    
  }
  
  # **************************************
  # the joint model:
  # X has a normal distribution
  # T has a gamma distribution
  # **************************************
  
  if (jointModel) {
    
    # build a search grid:
    xNm <- seq(0,8,length.out = 10)
    xNs <- seq(0,4,length.out = 10)
    tGs <- seq(0,10,length.out = 10)[2:11]
    tGr <- seq(0, 5,length.out = 10)[2:11]
    jointXTgrid <- expand.grid('mXn'=xNm, 'sXn'=xNs, 'sTg'=tGs, 'rTg'= tGr)
    
    # get Likelihoods for points in search grid:
    jointXTlLs <- apply(jointXTgrid,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XgaussianTgammaLikelihood)
    
    # get some of the best points in the grid:
    topgridJointXT <- jointXTgrid[order(jointXTlLs, decreasing = TRUE)[c(1:10)],]
    
    control <- list( 'maximize' = TRUE )
    
    # do the actual fitting:
    allJointXTfits <- do.call("rbind",
                              apply( topgridJointXT,
                                     MARGIN=c(1),
                                     FUN=optimx,
                                     fn=resetLogLikelihood,
                                     method=c('nlminb'),
                                     lower=c( 0.0, 0.0,  0.0, 0.0 ),
                                     upper=c( 8.0, 4.0, 10.0, 5.0 ),
                                     control=control,
                                     data=df,
                                     fitFUN=XgaussianTgammaLikelihood) )
    
    # pick the best fit:
    winJointXTfit <- allJointXTfits[order(allJointXTfits$value)[1],]
    
    winJointXTpar <- unlist(winJointXTfit[c('mXn','sXn','sTg','rTg')])
    
    winJointXTval <- as.numeric(winJointXTfit$value)
    names(winJointXTval) <- c('logL')
    
    outputlist[['JointXnormalTgamma']]=list('par'=winJointXTpar,'L'=winJointXTval)
    
  }
  
  
  return( outputlist )
  
}

plotModels <- function(target='inline') {
  
  df <- getDataTrials()
  
  modelfits <- fitSomeModels(df, Xnormal=FALSE)
  
  colors <- getColors()
  
  if (target == 'pdf') {
    pdf(file = 'doc/Fig5_marginals_distributions.pdf', width=6, height=4/0.75, bg='white')
  }
  
  # layout(mat=matrix(c(2,1,4,2,1,5,7,3,6),byrow=TRUE,ncol=3,nrow=3),
  #        widths = c(0.75,2,1.5), heights = c(1,1,1))
  # layout(mat=matrix(c(2,1,4,5,2,1,6,8,7,3,6,8),byrow=TRUE,ncol=4,nrow=3),
  #        widths = c(0.75,2,1.25,1.25), heights = c(1,0.35,0.65))
  layout(mat=matrix(c(2,1,4, 2,1,5, 6,3,5),byrow=TRUE,ncol=3,nrow=3),
         widths = c(1,2,1.5), heights = c(2,1,1))
  
  par(mar=c(3.5,3.5,2,2))
  
  plot(df$X,df$Y,
       main='',xlab='',ylab='',
       xlim=c(0,8),ylim=c(0,13.5),
       pch=16,col=t_col(colors$purple$s, percent = 95),cex=2.5,
       bty='n',ax=F,asp=1)
  
  title(main='A: reset time and offset',
        font.main=1, cex.main=1.5, adj=0, line=0.25)
  title(xlab='reset X [cm]', line=2.4)
  title(ylab='reset Y [cm]', line=2.4)
  
  Lx <- modelfits$XlimOrth$par['Lx']
  lines(x   = rep(Lx,2),
        y   = c(0,13.5), 
        col = colors$blue$s,
        lw=2)
  polygon(x  = c(Lx,Lx-.2,Lx+.2),
          y  = c(-.2, 0, 0),
          col = colors$blue$s,
          border=NA)
  
  Lt <- as.numeric(modelfits$TlimOrth$par['Lt'])
  for (speed in c(3,4)) {
    lines(x   = sin(seq(0,pi/2,length.out = 50)) * Lt * speed,
          y   = cos(seq(0,pi/2,length.out = 50)) * Lt * speed,
          col = colors$yorkred$s,
          lw  = 2,
          lty = speed-2)
    polygon(x   = c(-.2,0,0),
            y   = (Lt * speed) + c(0, .2, -.2),
            col = colors$yorkred$s,
            border=NA)
  }
  # lines(x   = c(0,8),
  #       y   = rep(Lt,2),
  #       col = colors$yorkred$s)
  
  axis(side=1,at=c(0,4,8))
  axis(side=2,at=c(0,4.5,9,13.5))
  
  
  
  # # # # # # # # # # # # # # #
  #
  # second plot: T distribution
  #
  # # # # # # # # # # # # # # #
  
  
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(1-0.75,1),ylim=c(0,4),
       bty='n', ax=F)
  
  title(ylab='reset time [s]', line=2.4)
  
  # put a histogram of the data:
  histogram <- hist(df$RT[which(df$RT <=4)], breaks=seq(0, 4, length.out = 40), plot=FALSE)
  idx <- which(histogram$breaks < 4)
  barheights=histogram$density[idx] #/max(histogram$density[idx])
  #print(barheights)
  for (id in idx) {
    y <- c(histogram$breaks[c(id,id,id+1,id+1)]) #+ c(0.005,0.005,-0.005,-0.005)
    x <- c(1,1-barheights[c(id,id)],1)
    polygon(x=x, y=y,
            col='#CCCCCC',border=NA)
  }
  
  
  x <- c(0)
  y <- c(0)
  for (id in idx) {
    y <- c(y, histogram$breaks[c(id,id+1)]) #+ c(0.005,0.005,-0.005,-0.005)
    x <- c(x, barheights[c(id,id)])
  }
  x <- 1-c(x,0)
  y <- c(y,histogram$breaks[id])
  polygon(x=x, y=y,
          col='#CCCCCC',border=NA)
  
  
  # draw a line with the density of the model function
  par  <- modelfits$TdistGamma$par
  #print(par)
  data <- data.frame('RT'=seq(0,4,0.02))
  dens <- TGammaLikelihood(par,data)$L 
  #dens <- dens / max(dens)
  #print(max(dens))
  xvals <- data$RT
  lines(x=1-dens,y=xvals,col=colors$yorkred$s)
  
  axis(side=2,at=c(0,1,2,3,4))
  
  
  # # # # # # # # # # # # # # #
  #
  # third plot: X distribution
  #
  # # # # # # # # # # # # # # #
  
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,8),ylim=c(0,0.36), # max density = 0.351679104
       bty='n', ax=F)
  
  # put a histogram of the data:
  histogram <- hist(df$X[which(df$X <=8)], breaks=seq(0, 8, length.out = 40), plot=FALSE)
  idx <- which(histogram$breaks < 8)
  barheights = histogram$density[idx]

  x <- c(0)
  y <- c(0)
  for (id in idx) {
    x <- c(x, histogram$breaks[c(id,id+1)]) #+ c(0.005,0.005,-0.005,-0.005)
    y <- c(y, barheights[c(id,id)])
  }
  x <- c(x,histogram$breaks[id])
  y <- 0.36 - c(y,0)
  polygon(x=x, y=y,
          col='#CCCCCC',border=NA)
  
  
  # draw a line with the density of the model function
  par  <- modelfits$XdistGamma$par
  #print(par)
  data <- data.frame('X'=seq(0,8,0.02))
  dens <- XGammaLikelihood(par,data)$L 
  #dens <- dens / max(dens)
  #print(max(dens))
  #print(dens)
  xvals <- data$X
  lines(x=xvals,y=0.36-dens,col=colors$blue$s)
  
  
  
  # # # # # # # # # # # # # # # # # # # # #
  #
  # no plot: distribution data for all fits
  #
  # # # # # # # # # # # # # # # # # # # # #
  
  
  # data for all distribution fits
  stepsize <- 0.1
  img_x <- seq(0,8,stepsize)
  img_y <- seq(0,13.5,stepsize)
  data_x <- img_x[1:(length(img_x)-1)] + diff(img_x)
  data_y <- img_y[1:(length(img_y)-1)] + diff(img_y)
  data = expand.grid('X'=data_x,'Y'=data_y)
  data$speed <- 4 # really: c(3.375, 4.500)
  data$RT <- sqrt(data$X^2 + data$Y^2) / data$speed
  
  
  # # # # # # # # # # # # # # # # #
  #
  # fifth plot: T distribution fit
  #
  # # # # # # # # # # # # # # # # #
  
  canvas_idx <- which(df$X <= 8 & df$Y <= 13.5)
  
  plot(df$X[canvas_idx],df$Y[canvas_idx],
       main='',xlab='',ylab='',
       xlim=c(0,8),ylim=c(0,13.5),
       pch=16,col=t_col('#000000', percent = 0),cex=0.2,
       bty='n',ax=F,asp=1)
  
  title(main='B: time model',
        font.main=1, cex.main=1.5, adj=0, line=0.25)
  title(xlab='reset X [cm]', line=2.4)
  title(ylab='reset Y [cm]', line=2.4)
  
  
  par <- modelfits$TdistGamma$par
  likelihoods <- TGammaLikelihood(par,data)
  
  Z <- matrix(likelihoods$L,
              ncol=length(data_y),
              nrow=length(data_x))
  
  #pal='Purple-Blue'
  
  image(add=TRUE,
        x=img_x,
        y=img_y,
        z=Z,
        useRaster=TRUE,
        #col = gray.colors(256, rev=TRUE),
        #col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
        col = linPal(from='#FFFFFF',to=colors$yorkred$s,alpha=0.5),
        #lw=0
  )
  
  axis(side=1,at=c(0,8))
  axis(side=2,at=c(0,13.5))
  
  # # # # # # # # # # # # # # # # #
  #
  # sixth plot: X distribution fit
  #
  # # # # # # # # # # # # # # # # #
  
  plot(df$X[canvas_idx],df$Y[canvas_idx],
       main='',xlab='',ylab='',
       xlim=c(0,8),ylim=c(0,13.5),
       pch=16,col=t_col('#000000', percent = 0),cex=0.2,
       bty='n',ax=F,asp=1)
  
  title(main='C: offset model',
        font.main=1, cex.main=1.5, adj=0, line=0.25)
  title(xlab='reset X [cm]', line=2.4)
  title(ylab='reset Y [cm]', line=2.4)
  
  
  par <- modelfits$XdistGamma$par
  likelihoods <- XGammaLikelihood(par,data)
  
  Z <- matrix(likelihoods$L,
              ncol=length(data_y),
              nrow=length(data_x))
  
  pal='Purple-Blue'
  
  image(add=TRUE,
        x=img_x,
        y=img_y,
        z=Z,
        useRaster=TRUE,
        #col = gray.colors(256, rev=TRUE),
        #col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
        col = linPal(from='#FFFFFF',to=colors$blue$s,alpha=0.5),
        #lw=0
  )
  
  axis(side=1,at=c(0,8))
  axis(side=2,at=c(0,13.5))
  

  if (target %in% c('pdf','svg')) {
    dev.off()
  }
  
}

AIClL <- function(lL, k) {
  return((2*k) - (2*lL))
}

fitParticipantModels <- function() {
  
  df <- getDataTrials()
  
  participant <- c()
  
  XlimLik <- c()
  XlimAIC <- c()
  XlimPn <- c()
  TlimLik <- c()
  TlimAIC <- c()
  TlimPn <- c()
  XgamLik <- c()
  XgamAIC <- c()
  TgamLik <- c()
  TgamAIC <- c()
  XlimPng <- c()
  TlimPng <- c()
  XgamPng <- c()
  TgamPng <- c()
  
  for (ppno in c(unique(df$participant),-1)) {
    
    if (ppno > 0) {
      participant <- c(participant, sprintf('%d',ppno))
      pdf <- df[which(df$participant==ppno),]
    } else {
      participant <- c(participant, 'all')
      pdf <- df
    }
    
    p_fits <- fitSomeModels(df=pdf)
    
    # X_MSE <- p_fits$XlimOrth$MSE
    # T_MSE <- p_fits$TlimOrth$MSE
    # 
    # AICs <- AICc(MSE=c(X_MSE, T_MSE), k=c(2,2), N=6)
    # limLL <- relativeLikelihood(AICs)
    # 
    # # print(AICs)
    # # print(limLL)
    # 
    # XlimMSE <- c(XlimMSE, X_MSE)
    # XlimAIC <- c(XlimAIC, AICs['Lx'])
    # TlimMSE <- c(TlimMSE, T_MSE)
    # TlimAIC <- c(TlimAIC, AICs['Lt'])
    # XlimP <- c(XlimP, limLL['Lx'])
    # TlimP <- c(TlimP, limLL['Lt'])
    
    Xn_lL <- as.numeric(p_fits$XdistNormal$logL)
    Tn_lL <- as.numeric(p_fits$TdistNormal$logL)
    
    NlogL <- c('Xn_ll'=Xn_lL, 'Tn_ll'=Tn_lL)
    NAICs <- AIClL(NlogL, k=c(2,2))
    normLL <- relativeLikelihood(NAICs)
    
    #print(p_fits$XdistNormal)
    # print(Xn_lL)
    # print(AICs)
    # print(limLL)
    
    XlimLik <- c(XlimLik, Xn_lL)
    XlimAIC <- c(XlimAIC, NAICs['Xn_ll'])
    TlimLik <- c(TlimLik, Tn_lL)
    TlimAIC <- c(TlimAIC, NAICs['Tn_ll'])
    XlimPn <- c(XlimPn, normLL['Xn_ll'])
    TlimPn <- c(TlimPn, normLL['Tn_ll'])
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    Xg_lL <- as.numeric(p_fits$XdistGamma$logL)
    Tg_lL <- as.numeric(p_fits$TdistGamma$logL)
    
    GlogL <- c('Xg_ll'=Xg_lL, 'Tg_ll'=Tg_lL)
    GAICs <- AIClL(GlogL, k=c(2,2))
    LL <- relativeLikelihood(c(NAICs,GAICs))
    
    #print(AICs)
    #print(limLL)
    
    XgamLik <- c(XgamLik, Xg_lL)
    XgamAIC <- c(XgamAIC, GAICs['Xg_ll'])
    TgamLik <- c(TgamLik, Tg_lL)
    TgamAIC <- c(TgamAIC, GAICs['Tg_ll'])
    XgamPng <- c(XgamPng, LL['Xg_ll'])
    TgamPng <- c(TgamPng, LL['Tg_ll'])
    XlimPng <- c(XlimPng, LL['Xn_ll'])
    TlimPng <- c(TlimPng, LL['Tn_ll'])
    
  }
  
  # print(length(participant))
  # print(length(XlimLik))
  # print(length(XlimAIC))
  
  return(data.frame(participant,XlimLik,XlimAIC,TlimLik,TlimAIC,XlimPn,TlimPn,XgamLik,XgamAIC,TgamLik,TgamAIC,XlimPng,TlimPng,XgamPng,TgamPng))
  
}

# Code Graveyard: -----

# plotJointModel <- function(df, target='inline') {
#   
#   modelfits <- fitSomeModels(df, jointModel=TRUE, Xgamma=FALSE)
#   
#   colors <- getColors()
#   
#   if (target == 'pdf') {
#     pdf(file = 'doc/jointModel.pdf', width=6, height=4.5, bg='white')
#   }
#   
#   # layout(mat=matrix(c(2,1,4,2,1,5,7,3,6),byrow=TRUE,ncol=3,nrow=3),
#   #        widths = c(0.75,2,1.5), heights = c(1,1,1))
#   layout(mat=matrix(c(2,1,4,5,2,1,6,8,7,3,6,8),byrow=TRUE,ncol=4,nrow=3),
#          widths = c(0.75,2,1.25,1.25), heights = c(1,0.35,0.65))
#   
#   par(mar=c(3.75,3.5,2,0.5))
#   
#   plot(df$X,df$RT,
#        main='',xlab='',ylab='',
#        xlim=c(0,8),ylim=c(0,4),
#        pch=16,col=t_col(colors$purple$s, percent = 95),cex=2.5,
#        bty='n',ax=F,asp=3)
#   
#   title(main='A: reset time and offset',
#         font.main=1, cex.main=1.5, adj=0, line=0.25)
#   title(xlab='reset offset [cm]', line=2.4)
#   title(ylab='reset time [s]', line=2.4)
#   
#   Lx <- modelfits$XlimOrth$par['Lx']
#   lines(x   = rep(Lx,2),
#         y   = c(0,4), 
#         col = colors$blue$s)
#   
#   Lt <- modelfits$TlimOrth$par['Lt']
#   lines(x   = c(0,8),
#         y   = rep(Lt,2),
#         col = colors$yorkred$s)
#   
#   axis(side=1,at=c(0,4,8))
#   axis(side=2,at=c(0,2,4))
#   
#   
#   
#   # # # # # # # # # # # # # # #
#   #
#   # second plot: T distribution
#   #
#   # # # # # # # # # # # # # # #
#   
#   
#   
#   plot(-1000,-1000,
#        main='',xlab='',ylab='',
#        xlim=c(1-0.66,1),ylim=c(0,4),
#        bty='n', ax=F)
#   
#   # put a histogram of the data:
#   histogram <- hist(df$RT[which(df$RT <=4)], breaks=seq(0, 4, length.out = 40), plot=FALSE)
#   idx <- which(histogram$breaks < 4)
#   barheights=histogram$density[idx] #/max(histogram$density[idx])
#   #print(barheights)
#   for (id in idx) {
#     y <- c(histogram$breaks[c(id,id,id+1,id+1)]) #+ c(0.005,0.005,-0.005,-0.005)
#     x <- c(1,1-barheights[c(id,id)],1)
#     polygon(x=x, y=y,
#             col='#CCCCCC',border=NA)
#   }
#   
#   # draw a line with the density of the model function
#   par  <- modelfits$TdistGamma$par
#   #print(par)
#   data <- data.frame('RT'=seq(0,4,0.02))
#   dens <- TGammaLikelihood(par,data)$L 
#   #dens <- dens / max(dens)
#   #print(max(dens))
#   xvals <- data$RT
#   lines(x=1-dens,y=xvals,col=colors$yorkred$s)
#   
#   
#   # # # # # # # # # # # # # # #
#   #
#   # third plot: X distribution
#   #
#   # # # # # # # # # # # # # # #
#   
#   
#   plot(-1000,-1000,
#        main='',xlab='',ylab='',
#        xlim=c(0,8),ylim=c(0,0.29), # max density = 0.284705
#        bty='n', ax=F)
#   
#   # put a histogram of the data:
#   histogram <- hist(df$X[which(df$X <=8)], breaks=seq(0, 8, length.out = 40), plot=FALSE)
#   idx <- which(histogram$breaks < 8)
#   #barheights=histogram$density[idx]/max(histogram$density[idx])
#   barheights = histogram$density[idx]
#   #print(barheights)
#   for (id in idx) {
#     x <- c(histogram$breaks[c(id,id,id+1,id+1)]) #+ c(0.005,0.005,-0.005,-0.005)
#     y <- c(0.29,0.29-barheights[c(id,id)],0.29)
#     polygon(x=x, y=y,
#             col='#CCCCCC',border=NA)
#   }
#   
#   # draw a line with the density of the model function
#   par  <- modelfits$XdistNormal$par
#   #print(par)
#   data <- data.frame('X'=seq(0,8,0.02))
#   dens <- XoffsetGaussianLikelihood(par,data)$L 
#   #dens <- dens / max(dens)
#   #print(max(dens))
#   #print(dens)
#   xvals <- data$X
#   lines(x=xvals,y=0.29-dens,col=colors$blue$s)
#   
#   
# 
#   # # # # # # # # # # # # # # # # # # # # #
#   #
#   # no plot: distribution data for all fits
#   #
#   # # # # # # # # # # # # # # # # # # # # #
#   
#   
#   # data for all distribution fits
#   stepsize <- 0.1
#   img_x <- seq(0,8,stepsize)
#   img_y <- seq(0,13.5,stepsize)
#   data_x <- img_x[1:(length(img_x)-1)] + diff(img_x)
#   data_y <- img_y[1:(length(img_y)-1)] + diff(img_y)
#   data = expand.grid('X'=data_x,'Y'=data_y)
#   data$speed <- 4 # really: c(3.375, 4.500)
#   data$RT <- sqrt(data$X^2 + data$Y^2) / data$speed
#   
#   
#   # # # # # # # # # # # # # # # # #
#   #
#   # fifth plot: T distribution fit
#   #
#   # # # # # # # # # # # # # # # # #
#   
#   plot(df$X,df$Y,
#        main='',xlab='',ylab='',
#        xlim=c(0,8),ylim=c(0,13.5),
#        pch=16,col=t_col('#000000', percent = 0),cex=0.2,
#        bty='n',ax=F,asp=1)
#   
#   title(main='B: gamma T',
#         font.main=1, cex.main=1.5, adj=0, line=0.25)
#   title(xlab='reset X [cm]', line=2.4)
#   title(ylab='reset Y [cm]', line=2.4)
#   
#   
#   par <- modelfits$TdistGamma$par
#   likelihoods <- TGammaLikelihood(par,data)
#   
#   Z <- matrix(likelihoods$L,
#               ncol=length(data_y),
#               nrow=length(data_x))
#   
#   pal='Purple-Blue'
#   
#   image(add=TRUE,
#         x=img_x,
#         y=img_y,
#         z=Z,
#         useRaster=TRUE,
#         #col = gray.colors(256, rev=TRUE),
#         col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
#         #lw=0
#   )
#   
#   axis(side=1,at=c(0,8))
#   axis(side=2,at=c(0,13.5))
#   
#   # # # # # # # # # # # # # # # # #
#   #
#   # sixth plot: X distribution fit
#   #
#   # # # # # # # # # # # # # # # # #
#   
#   plot(df$X,df$Y,
#        main='',xlab='',ylab='',
#        xlim=c(0,8),ylim=c(0,13.5),
#        pch=16,col=t_col('#000000', percent = 0),cex=0.2,
#        bty='n',ax=F,asp=1)
#   
#   title(main='C: normal X',
#         font.main=1, cex.main=1.5, adj=0, line=0.25)
#   title(xlab='reset X [cm]', line=2.4)
#   title(ylab='reset Y [cm]', line=2.4)
#   
#   
#   par <- modelfits$XdistNormal$par
#   likelihoods <- XoffsetGaussianLikelihood(par,data)
#   
#   Z <- matrix(likelihoods$L,
#               ncol=length(data_y),
#               nrow=length(data_x))
#   
#   pal='Purple-Blue'
#   
#   image(add=TRUE,
#         x=img_x,
#         y=img_y,
#         z=Z,
#         useRaster=TRUE,
#         #col = gray.colors(256, rev=TRUE),
#         col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
#         #lw=0
#   )
# 
#   axis(side=1,at=c(0,8))
#   axis(side=2,at=c(0,13.5))
#   
#   # # # # # # # # # # # # # # # # # # # #
#   #
#   # seventh plot: joint distribution fit
#   #
#   # # # # # # # # # # # # # # # # # # # #
#   
#   plot(df$X,df$Y,
#        main='',xlab='',ylab='',
#        xlim=c(0,8),ylim=c(0,13.5),
#        pch=16,col=t_col('#000000', percent = 0),cex=0.2,
#        bty='n',ax=F,asp=1)
#   
#   title(main=expression('D: joint X,T'), 
#         font.main=1, cex.main=1.5, adj=0, line=0.25)
#   # title(main='D: joint X$\times$T',
#   #       font.main=1, cex.main=1.5, adj=0, line=0.25)
#   title(xlab='reset X [cm]', line=2.4)
#   title(ylab='reset Y [cm]', line=2.4)
#   
#   
#   par <- modelfits$JointXnormalTgamma$par
#   likelihoods <- XgaussianTgammaLikelihood(par,data)
#   
#   Z <- matrix(likelihoods$L,
#               ncol=length(data_y),
#               nrow=length(data_x))
#   
#   pal='Purple-Blue'
#   
#   image(add=TRUE,
#         x=img_x,
#         y=img_y,
#         z=Z,
#         useRaster=TRUE,
#         #col = gray.colors(256, rev=TRUE),
#         col = hcl.colors(n=256, palette=pal, alpha=0.5, rev=TRUE),
#         #lw=0
#   )
#   
#   axis(side=1,at=c(0,8))
#   axis(side=2,at=c(0,13.5))
#   
#   if (target %in% c('pdf','svg')) {
#     dev.off()
#   }
#   
# }
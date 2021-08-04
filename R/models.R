
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


mdvar <- function(coords, ddof=0) {
  
  # subtract the mean of each column from values in column:
  mcoords <- apply(coords, 2, scale, scale=FALSE, center=TRUE)
  
  return( sum ( rowSums( mcoords^2 ) ) / (dim(coords)[1] - ddof) )
  
}

mdexplvar <- function(actual, predicted, ddof) {
  
  # raw data variance:
  mdv.a <- mdvar(actual)
  # variance of residuals:
  mdv.r <- mdvar(actual-predicted)
  
  fraction <- ( (mdv.a-mdv.r) / mdv.a)
  #fraction <- ( mdv.r / mdv.a)
  
  # if (fraction < 0) {
  #   exp.var <- -1 - fraction
  # } else {
  #   exp.var <- 1 - fraction
  # }
  
  exp.var <- 1 - fraction
  
  #print(fraction)
  #print(exp.var)
  
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

getData54 <- function(illusionMinimum=5) {
  
  df <- summarizeTraceBoundsV4()
  
  df$speed <- NA
  
  # convert speed to pass duration
  df$speed[which(df$externalspeed == 0.125)] <- (13.5/4)
  df$speed[which(df$externalspeed == 0.167)] <- (13.5/3)
  
  # only get substantial illusion strengths:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$initialdirection_mean > illusionMinimum),]
  }
  
  # add slopes conveniently:
  df$angle <- ((90-df$initialdirection_mean)/180)*pi
  df$sin.a <- sin(df$angle)
  df$cos.a <- cos(df$angle)
  df$slope <- df$sin.a / df$cos.a
  
  df$X <- df$boundX_mean * 13.5
  df$Y <- df$boundY_mean * 13.5
  
  df$X.sd <- df$boundX_sd * 13.5
  df$Y.sd <- df$boundY_sd * 13.5
  
  df$RT <- sqrt(df$X^2 + df$Y^2) / df$speed
  
  df$Vi <- df$internalspeed
  df$Ve <- round(0.5 / df$externalspeed)
  
  df <- df[,c('participant','X', 'Y', 'X.sd', 'Y.sd', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve')]
  
  return(df)
  
}

getDataTrials <- function(illusionMinimum=5, illusionMaximum=85) {
  
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)
  
  # remove trials without reset:
  df <- df[!is.na(df$boundX),]
  
  # convert speed to pass duration
  df$speed <- 13.5/round(0.5/df$externalspeed)
  
  # only get illusion strengths within a reasonable range:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$initialdirection > illusionMinimum),]
  }
  if (is.numeric(illusionMaximum)) {
    df <- df[which(df$initialdirection < illusionMaximum),]
  }
  
  # add slopes conveniently:
  df$angle <- ((df$initialdirection)/180)*pi
  df$sin.a <- sin(df$angle)
  df$cos.a <- cos(df$angle)
  df$slope <- df$sin.a / df$cos.a
  
  # get coordinates in centimeters:
  df$X <- df$boundX * 13.5
  df$Y <- df$boundY * 13.5
  
  # get reset time in seconds:
  df$RT <- sqrt(df$X^2 + df$Y^2) / df$speed
  
  # get internal/external speeds in cm/s (should give the same ratios as dva/s):
  df$Vi <- df$internalspeed
  df$Ve <- round(0.5 / df$externalspeed)
  
  # select only useful columns:
  df <- df[,c('participant','X', 'Y', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve')]
  
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
      
      
      expvar <- mdexplvar(actual = actual,
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

bootStrapFits <- function(bootstraps=1000) {
  
  colors=getColors()
  
  #layout(matrix(c(1,1,2,1,1,3),nrow=2,ncol=3,byrow=TRUE))
  
  # get resets per trial
  # remove trials without resets or out of range
  df <- getDataTrials()
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
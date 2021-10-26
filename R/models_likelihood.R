
# ******************************************
# normal probability density functions -----
# ******************************************


TlimProbabilities <- function(par,data) {
  
  # time limit
  
  mT <- par['mT']
  sT <- par['sT']
  
  L <- (1 / (sT * sqrt(2 * pi)) ) * exp( -0.5 * ( ( data$RT - mT) / sT )^2 )
  
  return(data.frame(L))
  
}

YlimProbabilities <- function(par,data) {
  
  # time limit
  mY <- par['mY']
  sY <- par['sY']
  
  L <- (1 / (sY * sqrt(2 * pi)) ) * exp( -0.5 * ( ( data$Y - mY) / sY )^2 )
  
  return(data.frame(L))
  
}

XlimProbabilities <- function(par,data) {
  
  # space limit
  
  mX <- par['mX']
  sX <- par['sX']
  
  L <- (1 / (sX * sqrt(2 * pi)) ) * exp( -0.5 * ( (data$X-mX) / sX )^2 )
  
  return(data.frame(L))
  
}

# XTlimProbabilities <- function(par,data) {
#   
#   XlimP <- XlimProbabilities(par,data)
#   TlimP <- TlimProbabilities(par,data)
#   
#   return(data.frame('L'=XlimP+TlimP)) # THIS NEEDS WORK: DO NOT ADD PROBABILITIES?
#   # this would give the same solution as a joint probability, except the likelihood is incorrectly calculated
#   
# }


# ******************************************************
# offset exponential probability density functions -----
# ******************************************************

XoffsetExponentialProbabilities <- function(par, data) {
  
  r <- par['r']
  mX <- par['mX']
  
  L <- dexp( data$X - mX, r=r )
  L[which(L == 0)] <- 0.0000000001 # log(0) will be -Inf, not a good probability to maximize
  
  return(data.frame(L))
  
}

ToffsetExponentialProbabilities <- function(par, data) {
  
  r <- par['r']
  mT <- par['mT']
  
  L <- dexp( (sqrt(data$X^2 + data$Y^2)/data$speed) - mT, r=r )
  L[which(L == 0)] <- 0.0000000001 # log(0) will be -Inf, not a good probability to maximize
  
  return(data.frame(L))
  
}

YoffsetExponentialProbabilities <- function(par, data) {
  
  r <- par['r']
  mY <- par['mY']
  
  L <- dexp( data$Y - mY, r=r )
  L[which(L == 0)] <- 0.0000000001 # log(0) will be -Inf, not a good probability to maximize
  
  return(data.frame(L))
  
}


# ******************************************************
# offset gamma probability density functions -----
# ******************************************************

# gamma distributions have a 'shape' parameter that puts them
# on a continuum that spans exponential and normal distributions  

XoffsetGammaProbabilities <- function(par, data) {
  
  s <- par['s']
  r <- par['r']
  mX <- par['mX']
  
  L <- dgamma( data$X - mX, shape=s, rate=r )
  L[which(is.na(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(is.infinite(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(L == 0)] <- 1e-10
  #print(L)
  return(data.frame(L))
  
}

ToffsetGammaProbabilities <- function(par, data) {
  
  s <- par['s']
  r <- par['r']
  mT <- par['mT']
  
  L <- dgamma( sqrt(data$X^2 + data$Y^2) - (mT * data$speed), shape=s, rate=r  )
  L[which(is.na(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(is.infinite(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(L == 0)] <- 1e-10
  #print(L)
  return(data.frame(L))
  
}

YoffsetGammaProbabilities <- function(par, data) {
  
  s <- par['s']
  r <- par['r']
  mY <- par['mY']
  
  L <- dgamma( data$Y - mY, shape=s, rate=r )
  L[which(is.na(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(is.infinite(L))] <- 1e-10 # log(0) will be -Inf, not a good probability to maximize
  L[which(L == 0)] <- 1e-10
  
  return(data.frame(L))
  
}


# twoLimResets <- function(par,data) {
#   
#   Lx <- par['Lx']
#   Ly <- par['Ly']
#   
#   d <- Ly * data$speed
#   
#   Y <- (d * data$sin.a) + (Lx * data$slope)
#   X <- (d * data$cos.a) + rep(Lx, length(Y))
#   
#   return(data.frame(X,Y))
#   
# }

# individualLimResets <- function(par,data) {
#   
#   participants <- unique(data$participant)
#   
#   X <- rep(NA, length(participants))
#   Y <- rep(NA, length(participants))
#   
#   for (pn in participants) {
#     
#     pdata <- data[which(data$participant == pn),]
#     
#     if (sprintf('Lx%d',pn) %in% names(par)) {
#       Lx <- par[sprintf('Lx%d',pn)]
#     } else {
#       Lx <- par['Lx']
#     }
#     if (sprintf('Ly%d',pn) %in% names(par)) {
#       Ly <- par[sprintf('Ly%d',pn)]
#     } else {
#       Ly <- par['Ly']
#     }
#     
#     d <- Ly * pdata$speed
#     
#     Y[which(data$participant == pn)] <- (d * pdata$sin.a) + (Lx * pdata$slope)
#     X[which(data$participant == pn)] <- (d * pdata$cos.a) + rep(Lx, dim(pdata)[1])
#     
#   }
#   
#   return(data.frame(X,Y))
#   
# }


# **********************
# model likelihood -----
# **********************


resetLogLikelihood <- function(par,data,fitFUN) {
  
  likelihoods <- fitFUN(par,data)
  
  return(sum(log(likelihoods)))
  
}


# *****************************
# fit singel limit models -----
# *****************************



library('optimx')

fitSingleLimitLikelihoodModels <- function(df) {
  
  # sX, mX
  # sT, mT
  
  # create search "grids":
  mX=seq(0, 8, length.out = 41)
  sX=seq(0.4, 6, length.out = 15)
  mT=seq(0, 4, length.out = 41)
  sT=seq(0.4, 6, length.out = 15)
  mY=seq(0, 13.5, length.out = 41)
  sY=seq(0.4, 6, length.out = 15)
  
  # make them into data frames:
  searchgridTlim <- expand.grid('mT'=mT, 'sT'=sT)
  searchgridXlim <- expand.grid('mX'=mX, 'sX'=sX)
  searchgridYlim <- expand.grid('mY'=mY, 'sY'=sY)
  
  # get MSE for points in search grid:
  TlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=TlimProbabilities)
  XlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XlimProbabilities)
  YlimLLs <- apply(searchgridYlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=YlimProbabilities)
  
  # get 4 best points in the grid:
  topgridXlim <- searchgridXlim[order(XlimLLs, decreasing = TRUE)[c(1,3,5)],]
  topgridTlim <- searchgridTlim[order(TlimLLs, decreasing = TRUE)[c(1,3,5)],]
  topgridYlim <- searchgridYlim[order(YlimLLs, decreasing = TRUE)[c(1,3,5)],]
  
  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  allXlimFits <- do.call("rbind",
                         apply( topgridXlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0.01),
                                upper=c(13.5,6),
                                control=control,
                                data=df,
                                fitFUN=XlimProbabilities,) )
  
  allTlimFits <- do.call("rbind",
                         apply( topgridTlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0.01),
                                upper=c(5,6),
                                control=control,
                                data=df,
                                fitFUN=TlimProbabilities) )
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0.01),
                                upper=c(13.5,6),
                                control=control,
                                data=df,
                                fitFUN=YlimProbabilities,) )
  #print(allXlimFits)
  #print(allYlimFits)
  
  # pick the best fit:
  winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
  winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
  winYlimFit <- allYlimFits[order(allYlimFits$value, decreasing = TRUE)[1],]
  
  winXpar <- winXlimFit[c('mX','sX')]
  winTpar <- winTlimFit[c('mT','sT')]
  winYpar <- winYlimFit[c('mY','sY')]
  
  winXval <- as.numeric(winXlimFit$value)
  names(winXval) <- c('logL')
  winTval <- as.numeric(winTlimFit$value)
  names(winTval) <- c('logL')
  winYval <- as.numeric(winYlimFit$value)
  names(winYval) <- c('logL')
  
  
  return(list( 'Xlim'=list('par'=winXpar,'logL'=winXval),
               'Tlim'=list('par'=winTpar,'logL'=winTval),
               'Ylim'=list('par'=winYpar,'logL'=winYval) ) )
  
}


fitTwoLimitLikelihoodModel <- function(df) {
  
  # # create search grid:
  # Lx=seq(0, 8, length.out = 21) # more than 8 centimeters is useless (the data goes up to ~5 or ~6)
  # Ly=seq(0, 4, length.out = 21) # data between 0 and 4 seconds
  # searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly)
  
  cat('THIS IS A JOINT PROBABILITY MODEL, WHICH IS INCORRECT!\n')
  return()
  
  # create search "grids":
  mX=seq(0, 4, length.out = 11)
  sX=seq(0.4, 6, length.out = 15)
  mT=seq(0, 3, length.out = 16)
  sT=seq(0.4, 6, length.out = 15)
  
  searchgrid <- expand.grid('mX'=mX,
                            'sX'=sX,
                            'mT'=mT,
                            'sT'=sT)
  
  
  # get MSE for points in search grid:
  LLs <- apply(searchgrid,FUN=resetLogLikelihood,MARGIN=c(1),data=df, fitFUN=XTlimProbabilities)
  
  # cat('(evaluated search grid)\n')
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(LLs)[1:8],]
  # print(topgrid)
  
  control <- list( 'maximize' = TRUE )
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetLogLikelihood,
                            method=c('nlminb'),
                            lower=c(0,    0.01, 0, 0.01),
                            upper=c(13.5, 6,    5, 6),
                            control=control,
                            data=df,
                            fitFUN=XTlimProbabilities) )
  # print(allfits)
  # pick the best fit:
  winfit <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  # print(win)
  
  winpar <- winfit[c('mX','sX','mT','sT')]
  
  winval <- as.numeric(winfit$value)
  names(winval) <- c('logL')
  
  return( list('par'=winpar, 'logL'=winval) )
  
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
    
    #ppfit <- fitTwoLimitModel(pdf)
    ppfit <- fitSingleLimitLikelihoodModels(pdf)
    
    print(ppfit)
    
    participant <- c(participant, ppno)
    Lx <- c(Lx, ppfit$par['Lx'])
    Ly <- c(Ly, ppfit$par['Ly'])
    MSE <- c(MSE, ppfit$MSE)

  }
  
  return(list('fits'=data.frame(participant,Lx,Ly,MSE), 'MSE'=mean(MSE)))
  
}

# ***************************************
# other distributions model fitting -----
# ***************************************


fitSingleOffsetExponentialLikelihoodModels <- function(df) {
  
  # sX, mX
  # sT, mT
  
  # create search "grids":
  mX=seq(0, 8, length.out = 41)
  mY=seq(0, 13.5, length.out = 41)
  mT=seq(0, 4, length.out = 41)
  r =seq(0, 2, length.out = 41)
  
  # make them into data frames:
  searchgridTlim <- expand.grid('mT'=mT, 'r'=r)
  searchgridXlim <- expand.grid('mX'=mX, 'r'=r)
  searchgridYlim <- expand.grid('mY'=mY, 'r'=r)
  
  # get MSE for points in search grid:
  expTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=ToffsetExponentialProbabilities)
  expXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XoffsetExponentialProbabilities)
  expYlimLLs <- apply(searchgridYlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=YoffsetExponentialProbabilities)
  
  # get 4 best points in the grid:
  topgridXlim <- searchgridXlim[order(expXlimLLs, decreasing = TRUE)[1:5],]
  topgridTlim <- searchgridTlim[order(expTlimLLs, decreasing = TRUE)[1:5],]
  topgridYlim <- searchgridYlim[order(expYlimLLs, decreasing = TRUE)[1:5],]
  
  control <- list( 'maximize' = TRUE )
  
  # do the actual fitting:
  allXlimFits <- do.call("rbind",
                         apply( topgridXlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0),
                                upper=c(13.5,2),
                                control=control,
                                data=df,
                                fitFUN=XoffsetExponentialProbabilities,) )
  
  allTlimFits <- do.call("rbind",
                         apply( topgridTlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0),
                                upper=c(5,2),
                                control=control,
                                data=df,
                                fitFUN=ToffsetExponentialProbabilities) )
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,0),
                                upper=c(13.5,2),
                                control=control,
                                data=df,
                                fitFUN=YoffsetExponentialProbabilities,) )
  #print(allXlimFits)
  #print(allYlimFits)
  
  # pick the best fit:
  winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
  winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
  winYlimFit <- allYlimFits[order(allYlimFits$value, decreasing = TRUE)[1],]
  
  winXpar <- winXlimFit[c('mX','r')]
  winTpar <- winTlimFit[c('mT','r')]
  winYpar <- winYlimFit[c('mY','r')]
  
  winXval <- as.numeric(winXlimFit$value)
  names(winXval) <- c('logL')
  winTval <- as.numeric(winTlimFit$value)
  names(winTval) <- c('logL')
  winYval <- as.numeric(winYlimFit$value)
  names(winYval) <- c('logL')
  
  
  return(list( 'Xlim'=list('par'=winXpar,'logL'=winXval),
               'Ylim'=list('par'=winYpar,'logL'=winYval),
               'Tlim'=list('par'=winTpar,'logL'=winTval) ) )
  
}

fitSingleOffsetGammaLikelihoodModels <- function(df) {
  
  # create search "grids":
  mX=seq(0, 8, length.out = 25)
  mY=seq(0, 13.5, length.out = 25)
  mT=seq(0, 4, length.out = 25)
  s =seq(0, 20, length.out = 12)
  r =1/seq(0, 4, length.out = 12)[2:41]
  
  # make them into data frames:
  searchgridTlim <- expand.grid('mT'=mT, 's'=s, 'r'=r)
  searchgridXlim <- expand.grid('mX'=mX, 's'=s, 'r'=r)
  searchgridYlim <- expand.grid('mY'=mY, 's'=s, 'r'=r)
  
  # get MSE for points in search grid:
  expTlimLLs <- apply(searchgridTlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=ToffsetGammaProbabilities)
  expXlimLLs <- apply(searchgridXlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=XoffsetGammaProbabilities)
  expYlimLLs <- apply(searchgridYlim,FUN=resetLogLikelihood,MARGIN=c(1),data=df,fitFUN=YoffsetGammaProbabilities)
  
  # get 4 best points in the grid:
  topgridXlim <- searchgridXlim[order(expXlimLLs, decreasing = TRUE)[c(1,3,5)],]
  topgridTlim <- searchgridTlim[order(expTlimLLs, decreasing = TRUE)[c(1,3,5)],]
  topgridYlim <- searchgridYlim[order(expYlimLLs, decreasing = TRUE)[c(1,3,5)],]
  
  control <- list( 'maximize' = TRUE )
  
  # print(topgridXlim)
  # print(sort(expXlimLLs, decreasing = TRUE)[c(1,3,5)])
  
  # do the actual fitting:
  allXlimFits <- do.call("rbind",
                         apply( topgridXlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,   1e-10, 1e-10),
                                upper=c(13.5,   20,    10),
                                control=control,
                                data=df,
                                fitFUN=XoffsetGammaProbabilities,) )
  
  # print(topgridTlim)
  # print(sort(expTlimLLs, decreasing = TRUE)[c(1,3,5)])
  
  allTlimFits <- do.call("rbind",
                         apply( topgridTlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0, 1e-10, 1e-10),
                                upper=c(5,    20,    10),
                                control=control,
                                data=df,
                                fitFUN=ToffsetGammaProbabilities) )
  
  # print(topgridYlim)
  # print(sort(expYlimLLs, decreasing = TRUE)[c(1,3,5)])
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetLogLikelihood,
                                method=c('nlminb'),
                                lower=c(0,   1e-10, 1e-10),
                                upper=c(13.5,   20,    10),
                                control=control,
                                data=df,
                                fitFUN=YoffsetGammaProbabilities,) )
  #print(allXlimFits)
  #print(allYlimFits)
  
  # pick the best fit:
  winXlimFit <- allXlimFits[order(allXlimFits$value, decreasing = TRUE)[1],]
  winTlimFit <- allTlimFits[order(allTlimFits$value, decreasing = TRUE)[1],]
  winYlimFit <- allYlimFits[order(allYlimFits$value, decreasing = TRUE)[1],]
  
  winXpar <- winXlimFit[c('mX','s','r')]
  winTpar <- winTlimFit[c('mT','s','r')]
  winYpar <- winYlimFit[c('mY','s','r')]
  
  winXval <- as.numeric(winXlimFit$value)
  names(winXval) <- c('logL')
  winTval <- as.numeric(winTlimFit$value)
  names(winTval) <- c('logL')
  winYval <- as.numeric(winYlimFit$value)
  names(winYval) <- c('logL')
  
  
  return(list( 'Xlim'=list('par'=winXpar,'logL'=winXval),
               'Ylim'=list('par'=winYpar,'logL'=winYval),
               'Tlim'=list('par'=winTpar,'logL'=winTval) ) )
  
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

getDataPavg <- function(illusionMinimum=5) {
  
  df <- summarizeTraceBoundsV4()
  
  # get speed as cm/second
  df$speed <- 13.5 / round(0.5 / df$externalspeed)
  
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




# this function should probably live somewhere else:

getDataTrials <- function(illusionMinimum=5) {
  
  df <- read.csv('data/onePass_V4/onePass_V4_re-trace.csv', stringsAsFactors = F)

  # remove trials without reset:
  df <- df[!is.na(df$boundX),]
  
  # convert speed to pass duration
  df$speed <- 13.5/round(0.5/df$externalspeed)
  
  # only get substantial illusion strengths:
  if (is.numeric(illusionMinimum)) {
    df <- df[which(df$initialdirection > illusionMinimum),]
  }
  
  # add slopes conveniently:
  df$angle <- ((90-df$initialdirection)/180)*pi
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


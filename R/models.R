
# *************************
# model functions -----
# *************************


YlimResets <- function(par,data) {
  
  # time limit
  
  Ly <- par['Ly']
  
  d <- Ly * data$speed
  
  Y <- d * data$sin.a
  X <- d * data$cos.a
  
  return(data.frame(X,Y))
  
}


XlimResets <- function(par,data) {
  
  # space limit
  
  Lx <- par['Lx']
  
  Y <- Lx * data$slope
  
  return(data.frame('Y'=Y, 'X'=rep(Lx,length(Y))))
  
}

twoLimResets <- function(par,data) {
  
  Lx <- par['Lx']
  Ly <- par['Ly']
  
  d <- Ly * data$speed
  
  Y <- (d * data$sin.a) + (Lx * data$slope)
  X <- (d * data$cos.a) + rep(Lx, length(Y))
  
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

# resetLikelihood <- function(par,data,fitFUN) {
#   
#   resets <- fitFUN(par,data)
#   
#   Yerrors <- (resets$Y - data$Y)
#   Xerrors <- (resets$X - data$X)
#   

# it's a joint distribution, so we could take the product of the pdf values for 0 on each axis...
# but I think it's more common to use the sum of the natural logs of the pdf values for 0 on each axis?

#   return(???)
#   
# }

library('optimx')

fitSingleLimitModels <- function(df) {
  
  # create search "grids":
  Lx=seq(0, 8, length.out = 41)
  Ly=seq(0, 4, length.out = 41)
  
  # make them into data frames:
  searchgridYlim <- expand.grid('Ly'=Ly)
  searchgridXlim <- expand.grid('Lx'=Lx)
  
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
                                data=df,
                                fitFUN=XlimResets) )
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(4),
                                data=df,
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
  
  
  return(list( 'Xlim'=list('par'=winXlim,'MSE'=winXval),
               'Ylim'=list('par'=winYlim,'MSE'=winYval) ) )
  
}

fitTwoLimitModel <- function(df) {
  
  # create search grid:
  Lx=seq(0, 8, length.out = 41) # more than 8 centimeters is useless (the data goes up to ~5 or ~6)
  Ly=seq(0, 4, length.out = 41) # data between 0 and 4 seconds
  searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly)
  
  # get MSE for points in search grid:
  MSE <- apply(searchgrid,FUN=resetMSE,MARGIN=c(1),data=df, fitFUN=twoLimResets)
  
  # cat('(evaluated search grid)\n')
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(MSE)[1:15],]
  # print(topgrid)
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetMSE,
                            method='L-BFGS-B',
                            lower=c(0,0),
                            upper=c(13.5,4),
                            data=df,
                            fitFUN=twoLimResets) )
  # print(allfits)
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  # print(win)
  
  winpar <- as.numeric(win[1:2])
  names(winpar) <- names(win[1:2])
  
  return( list('par'=winpar, 'MSE'=win$value) )
  
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



compareModels <- function(N=9,AICfun=AIC,verbosity=1) {
  
  df <- getData()

  single <- fitSingleLimitModels(df)
  double <- fitTwoLimitModel(df)
  
  if (verbosity > 0) {

    cat('\nFITTED MODEL LIMIT PARAMETERS:\n\n')
    cat(sprintf('single Lx: %0.2f cm\n',                  single$Xlim$par['Lx']))
    cat(sprintf('single Ly: %0.2f s\n',                   single$Ylim$par['Ly']))
    cat(sprintf('combined Lx: %0.2f cm and Ly: %0.2f s\n',double$par['Lx'],double$par['Ly']))

    MSEs <- c('Lx'=single$Xlim$MSE[['Lx']], 'Ly'=single$Ylim$MSE[['Ly']], 'LxLy'=double$MSE)

    cat('\nmodel MSEs:\n')
    print(MSEs)

    # the AIC parameters are the same for both models, except the MSEs
    AICs <- AICfun(MSE=MSEs, k=c(1,1,2), N=N)

    cat('\nmodel AICs:\n')
    print(AICs)

    rLL <- relativeLikelihood(AICs[1:2])
    cat('\nsingle limit model relative likelihoods:\n')
    print(rLL)

    rLL <- relativeLikelihood(AICs)
    cat('\nALL model relative likelihoods:\n')
    print(rLL)
  }

}


# **********************
# getting the data -----
# **********************

source('R/retrace_onePass_V4.R')

getData <- function(illusionMinimum=5) {
  
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
  df$Ve <- df$externalspeed
  
  df <- df[,c('participant','X', 'Y', 'X.sd', 'Y.sd', 'RT', 'speed', 'slope', 'angle', 'sin.a', 'cos.a', 'Vi', 'Ve')]
  
  return(df)
  
}

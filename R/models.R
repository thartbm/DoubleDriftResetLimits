library('optimx')


# combined geometric model -----

resetModel <- function(par,directions,verbose=FALSE) {
  
  Lx <- par['Lx']
  Ly <- par['Ly']
  a  <- par['a']
  
  slopes <- sin(directions) / cos(directions)
  
  Y <- (a * Lx * slopes) + ((1-a) * Ly)
  X <- (a * Lx)          + ((1-a) * Ly / slopes)
  
  resets <- data.frame(X,Y)
  
  if (verbose) {
    cat(sprintf('Lx: %0.3f, Ly: %0.3f, a: %0.3f\n',Lx,Ly,a))
  }
  
  return(resets)
  
}

resetModelMSE <- function(par,directions,coords) {
  
  # the Lx, Ly and a parameters should be between 0 and 1, but optimx checks that
  
  resets <- resetModel(par,directions)
  
  MSE <- mean( (resets$X - coords$X)^2 + (resets$Y - coords$Y)^2 )
  
  return(MSE)
  
}

fitResetModel <- function(directions,X,Y) {
  
  # convert input to more useful variables:
  coords <- data.frame(X,Y)
  directions <- ((90 - directions) / 180) * pi
  
  # create search grid:
  
  # # bound to "reasonable" values:
  # Lx=seq(0,3/13.5,.05)
  # Ly=seq(.5,1,.05)
  # a=seq(0,1,.05)
  
  # bound to data:
  #Lx=seq(0,median(X),diff(range(X))/10)
  #Ly=seq(median(Y),1,diff(range(Y))/10)
  
  # bound to data:
  Lx=seq(0,median(X),diff(range(X))/10)
  Ly=seq(median(Y),1,diff(range(Y))/10)
  a=seq(0,1,.1)
  
  searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly, 'a'=a) # 11^3 combinations
  
  # get MSE for points in search grid:
  MSE <- apply(searchgrid,FUN=resetModelMSE,MARGIN=c(1),directions=directions,coords=coords)
  
  # get 5 best points in the grid:
  topgrid <- searchgrid[order(MSE)[1:5],]
  
  allfits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=resetModelMSE,
                            method='L-BFGS-B',
                            lower=c(min(Lx),min(Ly),min(a)),
                            upper=c(max(Lx),max(Ly),max(a)),
                            directions=directions,
                            coords=coords ) )
  # print(allfits)
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  # print(win[1:3])
  
  winpar <- as.numeric(win[1:3])
  names(winpar) <- names(win[1:3])
  
  return(winpar)
  
}


# separated geometric models -----

resetXfromYlim <- function(par,slopes) {
  
  Ly <- par['Ly']

  X <- Ly / slopes
  
  return(data.frame(X))
  
}


resetYfromXlim <- function(par,slopes) {
  
  Lx <- par['Lx']
  
  Y <- Lx * slopes
  
  return(data.frame(Y))
  
}

resetXlimMSE <- function(par,slopes,coords) {
  
  resetY <- resetYfromXlim(par,slopes)
  
  MSE <- mean( (resetY$Y - coords$Y)^2 + (par['Lx'] - coords$X)^2 )
  
  return(MSE)
  
}

resetYlimMSE <- function(par,slopes,coords) {
  
  resetX <- resetXfromYlim(par,slopes)
  
  MSE <- mean( (resetX$X - coords$X)^2 + (par['Ly'] - coords$Y)^2 )
  
  return(MSE)
  
}

fitSeparateXYresetModels <- function(directions,X,Y) {
  
  # convert input to more useful variables:
  coords <- data.frame(X,Y)
  directions <- ((90 - directions) / 180) * pi
  slopes <- sin(directions) / cos(directions)
  
  # create search "grids":
  
  # # bound to "reasonable" values:
  # Lx=seq(0,3/13.5,.05)
  # Ly=seq(.5,1,.05)
  # a=seq(0,1,.05)
  
  Lx=seq(0,13.5,.5)
  Ly=seq(0,13.5,.5)
  
  
  searchgridYlim <- expand.grid('Ly'=Ly)
  searchgridXlim <- expand.grid('Lx'=Lx)
  
  # get MSE for points in search grid:
  YlimMSE <- apply(searchgridYlim,FUN=resetYlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
  XlimMSE <- apply(searchgridXlim,FUN=resetXlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
  
  
  # get 4 best points in the grid:
  topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[1:3],])
  topgridYlim <- data.frame('Ly'=searchgridYlim[order(YlimMSE)[1:3],])
  
  allXlimFits <- do.call("rbind",
                         apply( topgridXlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetXlimMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(13.5),
                                slopes=slopes,
                                coords=coords ) )
  
  allYlimFits <- do.call("rbind",
                         apply( topgridYlim,
                                MARGIN=c(1),
                                FUN=optimx,
                                fn=resetYlimMSE,
                                method='L-BFGS-B',
                                lower=c(0),
                                upper=c(13.5),
                                slopes=slopes,
                                coords=coords ) )
  
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
  
  
  return(list('par'=c(winXlim,winYlim), 'MSE'=c(winXval,winYval)))
  
}

fitSeparateModelsOnData <- function() {
  
  df <- summarizeTraceBoundsV4()
  df$externalspeed[which(df$externalspeed == 0.125)] <- 4
  df$externalspeed[which(df$externalspeed == 0.167)] <- 3
  
  df$boundY_mean[which(df$externalspeed == 3)] <- 0.75 * df$boundY_mean[which(df$externalspeed == 3)]
  
  df <- df[which(df$initialdirection_mean > 5),]
  
  fit <- fitSeparateXYresetModels(directions=df$initialdirection_mean,
                                  X=df$boundX_mean * 13.5,
                                  Y=df$boundY_mean * 13.5)
  cat('limit parameters:\n')
  print(fit$par * c(1, 4/13.5))
  cat('limit MSEs:\n')
  print(fit$MSE)
  
  # the AIC parameters are the same for both models, except the MSEs
  N <- 9
  # this is then used for C:
  C <- N*(log(2*pi)+1)
  
  AICs <- 2 + N*log(fit$MSE) + C
  cat('limit AICs:\n')
  print(AICs)
  
  relativeLikelihoods <- exp((min(AICs)-AICs)/2)
  cat('limit relative likelihoods:\n')
  print(relativeLikelihoods)
  
}
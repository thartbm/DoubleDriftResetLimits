# library('optimx')
# 
# 
# # *******************************
# # separate geometric models -----
# # *******************************
# 
# resetXfromYlim <- function(par,slopes) {
#   
#   # time limit
#   
#   Ly <- par['Ly']
# 
#   X <- Ly / slopes
#   
#   return(data.frame(X))
#   
# }
# 
# 
# resetYfromXlim <- function(par,slopes) {
#   
#   # space limit
#   
#   Lx <- par['Lx']
#   
#   Y <- Lx * slopes
#   
#   return(data.frame(Y))
#   
# }
# 
# resetXlimMSE <- function(par,slopes,coords) {
#   
#   resetY <- resetYfromXlim(par,slopes)
#   
#   MSE <- mean( (resetY$Y - coords$Y)^2 + (par['Lx'] - coords$X)^2 )
#   
#   return(MSE)
#   
# }
# 
# resetYlimMSE <- function(par,slopes,coords) {
#   
#   resetX <- resetXfromYlim(par,slopes)
#   
#   MSE <- mean( (resetX$X - coords$X)^2 + (par['Ly'] - coords$Y)^2 )
#   
#   return(MSE)
#   
# }
# 
# fitSeparateXYresetModels <- function(directions,X,Y) {
#   
#   # convert input to more useful variables:
#   coords <- data.frame(X,Y)
#   directions <- ((90 - directions) / 180) * pi
#   slopes <- sin(directions) / cos(directions)
#   
#   # create search "grids":
#   
#   # # bound to "reasonable" values:
#   # Lx=seq(0,3/13.5,.05)
#   # Ly=seq(.5,1,.05)
#   # a=seq(0,1,.05)
#   
#   Lx=seq(0,13.5,.5)
#   Ly=seq(0,13.5,.5)
#   
#   
#   searchgridYlim <- expand.grid('Ly'=Ly)
#   searchgridXlim <- expand.grid('Lx'=Lx)
#   
#   # get MSE for points in search grid:
#   YlimMSE <- apply(searchgridYlim,FUN=resetYlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   XlimMSE <- apply(searchgridXlim,FUN=resetXlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   
#   
#   # get 4 best points in the grid:
#   topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[1:3],])
#   topgridYlim <- data.frame('Ly'=searchgridYlim[order(YlimMSE)[1:3],])
#   
#   allXlimFits <- do.call("rbind",
#                          apply( topgridXlim,
#                                 MARGIN=c(1),
#                                 FUN=optimx,
#                                 fn=resetXlimMSE,
#                                 method='L-BFGS-B',
#                                 lower=c(0),
#                                 upper=c(13.5),
#                                 slopes=slopes,
#                                 coords=coords ) )
#   
#   allYlimFits <- do.call("rbind",
#                          apply( topgridYlim,
#                                 MARGIN=c(1),
#                                 FUN=optimx,
#                                 fn=resetYlimMSE,
#                                 method='L-BFGS-B',
#                                 lower=c(0),
#                                 upper=c(13.5),
#                                 slopes=slopes,
#                                 coords=coords ) )
#   
#   #print(allXlimFits)
#   #print(allYlimFits)
#   
#   # pick the best fit:
#   winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
#   winYlimFit <- allYlimFits[order(allYlimFits$value)[1],]
#   # print(win[1:3])
#   
#   winXlim <- as.numeric(winXlimFit[1])
#   names(winXlim) <- c('Lx')
#   winYlim <- as.numeric(winYlimFit[1])
#   names(winYlim) <- c('Ly')
#   
#   winXval <- as.numeric(winXlimFit[2])
#   names(winXval) <- c('Lx')
#   winYval <- as.numeric(winYlimFit[2])
#   names(winYval) <- c('Ly')
#   
#   
#   return(list('par'=c(winXlim,winYlim), 'MSE'=c(winXval,winYval)))
#   
# }
# 
# fitSeparateModelsOnData <- function(verbosity=0) {
#   
#   df <- getTimeNormalizedData()
#   
#   # gets the two separate fits:
#   fit <- fitSeparateXYresetModels(directions=df$initialdirection_mean,
#                                   X=df$boundX_mean * 13.5,
#                                   Y=df$boundY_mean * 13.5)
#   
#   if (verbosity > 0) {
#     cat('limit parameters [cm and s]:\n')
#     #print(fit$par)
#     print(fit$par * c(1, 4/13.5))
#     cat('limit MSEs:\n')
#     print(fit$MSE)
#     
#     # the AIC parameters are the same for both models, except the MSEs
#     # N <- (9*6)-1
#     N <- length(df$boundX_mean)
#     # this is then used for C:
#     C <- N*(log(2*pi)+1)
#     
#     AICs <- 2 + N*log(fit$MSE) + C
#     cat('limit AICs:\n')
#     print(AICs)
#     
#     relativeLikelihoods <- exp((min(AICs)-AICs)/2)
#     cat('limit relative likelihoods:\n')
#     print(relativeLikelihoods)
#   }
#   
# }
# 
# 
# # weighted geometric model -----
# 
# # resetModel <- function(par,slopes,verbose=FALSE) {
# #   
# #   Lx <- par['Lx']
# #   Ly <- par['Ly']
# #   a  <- par['a']
# #   
# #   Y <- (a * Lx * slopes) + ((1-a) * Ly)
# #   X <- (a * Lx)          + ((1-a) * Ly / slopes)
# #   
# #   resets <- data.frame(X,Y)
# #   
# #   if (verbose) {
# #     cat(sprintf('Lx: %0.3f, Ly: %0.3f, a: %0.3f\n',Lx,Ly,a))
# #   }
# #   
# #   return(resets)
# #   
# # }
# # 
# # resetModelMSE <- function(par,slopes,coords) {
# #   
# #   # the Lx, Ly and a parameters should be between 0 and 1, but optimx checks that
# #   
# #   resets <- resetModel(par,slopes)
# #   
# #   MSE <- mean( (resets$X - coords$X)^2 + (resets$Y - coords$Y)^2 )
# #   
# #   return(MSE)
# #   
# # }
# # 
# # fitResetModel <- function(slopes,X,Y) {
# #   
# #   # convert input to more useful variables:
# #   coords <- data.frame(X,Y)
# #   
# #   # create search grid:
# #   
# #   # # bound to "reasonable" values:
# #   # Lx=seq(0,3/13.5,.05)
# #   # Ly=seq(.5,1,.05)
# #   # a=seq(0,1,.05)
# #   
# #   # bound to data:
# #   #Lx=seq(0,median(X),diff(range(X))/10)
# #   #Ly=seq(median(Y),1,diff(range(Y))/10)
# #   
# #   # bound to data:
# #   Lx=seq(0,median(X),diff(range(X))/10)
# #   Ly=seq(median(Y),13.5,diff(range(Y))/10)
# #   a=seq(0,1,.1)
# #   
# #   searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly, 'a'=a) # 11^3 combinations (1331)
# #   
# #   # get MSE for points in search grid:
# #   MSE <- apply(searchgrid,FUN=resetModelMSE,MARGIN=c(1),slopes=slopes,coords=coords)
# #   
# #   # get 5 best points in the grid:
# #   topgrid <- searchgrid[order(MSE)[1:5],]
# #   
# #   allfits <- do.call("rbind",
# #                      apply( topgrid,
# #                             MARGIN=c(1),
# #                             FUN=optimx,
# #                             fn=resetModelMSE,
# #                             method='L-BFGS-B',
# #                             lower=c(min(Lx),min(Ly),min(a)),
# #                             upper=c(max(Lx),max(Ly),max(a)),
# #                             slopes=slopes,
# #                             coords=coords ) )
# #   # print(allfits)
# #   # pick the best fit:
# #   win <- allfits[order(allfits$value)[1],]
# #   # print(win[1:3])
# #   # print(win)
# #   
# #   winpar <- as.numeric(win[1:3])
# #   names(winpar) <- names(win[1:3])
# #   
# #   return(winpar)
# #   
# # }
# 
# 
# # ******************************
# # combined geometric model -----
# # ******************************
# 
# resetModelSeq <- function(par,slopes,verbose=FALSE) {
#   
#   Lx <- par['Lx']
#   Ly <- par['Ly']
#   
#   Y <- (Lx * slopes) + Ly
#   X <- (Ly / slopes) + Lx
#   
#   return(data.frame(X,Y))
#   
# }
# 
# resetModelSeqMSE <- function(par, slopes, coords) {
#   
#   # cat('parameters:\n')
#   # print(par)
#   
#   resets <- resetModelSeq(par,slopes)
#   # cat('Reset points:\n')
#   # print(resets)
#   
#   MSE <- mean( (resets$X - coords$X)^2 + (resets$Y - coords$Y)^2 )
#   # cat('MSE:\n')
#   # print(MSE)
#   
#   return(MSE)
#   
# }
# 
# fitResetModelSeq <- function(slopes,X,Y) {
#   
#   # convert input to more useful variables:
#   coords <- data.frame(X,Y)
# 
#   # create search grid:
#   
#   # # bound to "reasonable" values:
#   # Lx=seq(0,3/13.5,.05)
#   # Ly=seq(.5,1,.05)
#   # a=seq(0,1,.05)
#   
#   # bound to data:
#   #Lx=seq(0,median(X),diff(range(X))/10)
#   #Ly=seq(median(Y),1,diff(range(Y))/10)
#   
#   # bound to data:
#   Lx=seq(0,median(X),diff(range(X))/10)
#   Ly=seq(0,median(Y),diff(range(Y))/10)
#   
#   searchgrid <- expand.grid('Lx'=Lx, 'Ly'=Ly) #
#   # print(searchgrid)
#   
#   # get MSE for points in search grid:
#   MSE <- apply(searchgrid,FUN=resetModelSeqMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   
#   # cat('(evaluated search grid)\n')
#   
#   # get 5 best points in the grid:
#   topgrid <- searchgrid[order(MSE)[1:5],]
#   # print(topgrid)
#   
#   allfits <- do.call("rbind",
#                      apply( topgrid,
#                             MARGIN=c(1),
#                             FUN=optimx,
#                             fn=resetModelSeqMSE,
#                             method='L-BFGS-B',
#                             lower=c(0,0),
#                             upper=c(max(Lx),max(Ly)),
#                             slopes=slopes,
#                             coords=coords ) )
#   # print(allfits)
#   # pick the best fit:
#   win <- allfits[order(allfits$value)[1],]
#   # print(win[1:3])
#   # print(win)
#   
#   winpar <- as.numeric(win[1:2])
#   names(winpar) <- names(win[1:2])
#   
#   return(winpar)
#   
# }
# 
# # **********************
# # model evaluation -----
# # **********************
# 
# compareAllModels <- function(verbosity=1) {
#   
#   # get the data:
#   df <- getTimeNormalizedData()
#   
#   # convert data for simpler models:
#   coords <- data.frame('X'=df$boundX_mean,'Y'=df$boundY_mean)
#   directions <- ((90 - df$initialdirection_mean) / 180) * pi
#   slopes <- sin(directions) / cos(directions)
#   
#   
#   # get fits for all models:
#   Lx <- seq(0,1,length.out = 21)
#   Ly <- seq(0,1,length.out = 21)
#   
#   searchgridXlim <- expand.grid('Lx'=Lx)
#   searchgridYlim <- expand.grid('Ly'=Ly)
#   
#   searchgridXYlim <- expand.grid('Lx'=Lx,
#                                  'Ly'=Ly)
#   
#   
#   # get MSE for all points in search grid:
#   XlimMSE <- apply(searchgridXlim,FUN=resetXlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   YlimMSE <- apply(searchgridYlim,FUN=resetYlimMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   
#   XYlimMSE <- apply(searchgridXYlim,FUN=resetModelSeqMSE,MARGIN=c(1),slopes=slopes,coords=coords)
#   
#   # get 5 best points in the single limit "grids":
#   topgridXlim <- data.frame('Lx'=searchgridXlim[order(XlimMSE)[1:5],])
#   topgridYlim <- data.frame('Ly'=searchgridYlim[order(YlimMSE)[1:5],])
#   # get 10 best points in the combined limit grid:
#   topgridXYlim <- searchgridXYlim[order(XYlimMSE)[1:10],]
#   
#   allXlimFits <- do.call("rbind",
#                          apply( topgridXlim,
#                                 MARGIN=c(1),
#                                 FUN=optimx,
#                                 fn=resetXlimMSE,
#                                 method='L-BFGS-B',
#                                 lower=c(0),
#                                 upper=c(1),
#                                 slopes=slopes,
#                                 coords=coords ) )
#   
#   allYlimFits <- do.call("rbind",
#                          apply( topgridYlim,
#                                 MARGIN=c(1),
#                                 FUN=optimx,
#                                 fn=resetYlimMSE,
#                                 method='L-BFGS-B',
#                                 lower=c(0),
#                                 upper=c(1),
#                                 slopes=slopes,
#                                 coords=coords ) )
#   
#   
#   allXYlimFits <- do.call("rbind",
#                          apply( topgridXYlim,
#                                 MARGIN=c(1),
#                                 FUN=optimx,
#                                 fn=resetModelSeqMSE,
#                                 method='L-BFGS-B',
#                                 lower=c(0,0),
#                                 upper=c(1,1),
#                                 slopes=slopes,
#                                 coords=coords ) )
#   
#   
#   # pick the best fit:
#   winXlimFit <- allXlimFits[order(allXlimFits$value)[1],]
#   winYlimFit <- allYlimFits[order(allYlimFits$value)[1],]
#   
#   winXYlimFit <- allXYlimFits[order(allXYlimFits$value)[1],]
#   # # 
#   # print(winXlimFit)
#   # print(winYlimFit)
#   # # 
#   # print(winXYlimFit)
#   
#   
#   if (verbosity > 0) {
#     
#     cat('\nFITTED MODEL LIMIT PARAMETERS:\n\n')
#     cat(sprintf('single Lx: %0.2f cm\n',                  winXlimFit$Lx*13.5))
#     cat(sprintf('single Ly: %0.2f s\n',                                       winYlimFit$Ly*4))
#     cat(sprintf('combined Lx: %0.2f cm and Ly: %0.2f s\n',winXYlimFit$Lx*13.5,winXYlimFit$Ly*4))
#     
#     MSEs <- c('Lx'=winXlimFit$value, 'Ly'=winYlimFit$value, 'LxLy'=winXYlimFit$value)
#     
#     cat('\nmodel MSEs:\n')
#     print(MSEs)
#     
#     # the AIC parameters are the same for both models, except the MSEs
#     # Gunnar says number of independent observations can be 
#     # N <- (9*6)-1
#     N <- length(df$boundX_mean)
#     # this is then used for C:
#     
#     # Patrick says N should be number of participants:
#     N <- 9
#     
#     cat(sprintf('\n(number of independent observations: %d)\n',N))
#     
#     AICs <- AIC(MSE=MSEs, k=c(1,1,2), N=N)
#     
#     AICs <- AICc(MSE=MSEs, k=c(1,1,2), N=N)
#     
#     cat('\nmodel AICs:\n')
#     print(AICs)
#     
#     rLL <- relativeLikelihood(AICs[1:2])
#     cat('\nsingle limit model relative likelihoods:\n')
#     print(rLL)
#     
#     rLL <- relativeLikelihood(AICs)
#     cat('\nALL model relative likelihoods:\n')
#     print(rLL)
#   }
#   
# }
# 
# 
# AIC <- function(MSE, k, N) {
#   return( (N * log(MSE)) + (2 * k) )
# }
# 
# AICc <- function(MSE, k, N) {
#   return( AIC(MSE, k, N) * (((2*k^2) + 2*k) / (N - k - 1)) ) 
# }
# 
# relativeLikelihood <- function(crit) {
#   return( exp( ( min( crit  ) - crit  ) / 2 ) )
# }

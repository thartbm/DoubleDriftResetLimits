

randomProcess <- function(repeats=10000, 
                          timepoints=5000, 
                          lambda=0.005,
                          eventno=2) {
  
  # vector of times until `eventno` events have occurred
  times <- c()
  
  # we repeat the random simulation `repeat` times
  for (r in c(1:repeats)) {
    
    # we get a bunch of random numbers:
    rp <- runif(timepoints) # timepoint random uniform numbers between 0 and 1
    
    # we check if the random numbers are lower than `lamda` at least `eventno` times
    if (length(which(rp <= lambda)) >= eventno) {
      # in this case we store that time that this happens:
      times <- c(times, which(rp <= lambda)[eventno])
    }
    
  }
  
  # plot histogram of the times:
  hist(times, breaks=40)
  
}



XlikelihoodTry <- function() {
  
  source('R/models.R')
  source('R/retrace_onePass_V4.R')
  
  df <- getData()
  
  singleFits <- fitSingleLimitModels(df)
  
  Xpred <- XlimResets(par=singleFits$Xlim$par, data=df)
  
  
  # x <- seq(pi/4, 5 * pi, length.out = 100)
  # y <- seq(pi/4, 5 * pi, length.out = 100)
  # r <- as.vector(sqrt(outer(x^2, y^2, "+")))
  # grid <- expand.grid(x=x, y=y)
  # grid$z <- cos(r^2) * exp(-r/(pi^3))

  plot(-1000,-1000,main='X-limit', xlab='', ylab='',
       xlim=c(-2,6),ylim=c(-.5,14),
       bty='n',ax=FALSE)
  
  title(xlab='x coordinate', line=2.5)
  title(ylab='y coordinate', line=2.5)
  
  x = seq(-2,6,0.1)
  y = seq(0,13.5,0.1)
  grid <- expand.grid(x=x, y=y)
  X.sigma <- sd(df$X)
  X.mu <- mean(df$X)
  grid$z <- (1 / (X.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( (grid$x-X.mu) / X.sigma )^2 )
  
  image(x=x, y=y, z=matrix(grid$z,ncol=length(y),nrow=length(x),byrow=FALSE),
        col = viridis::plasma(n=250, alpha=0.2),
        add = TRUE,
        oldstyle = FALSE, useRaster=FALSE)
  
  
  segments(df$X, df$Y, Xpred$X, Xpred$Y)
  
  axis(side=1, at=seq(-2,6,2))
  axis(side=2, at=seq(0,13.5,4.5))
  
  X.likelihoods <- (1 / (X.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( (df$X-X.mu) / X.sigma )^2 )
  
  Y.sigma <- sd(df$Y)
  Y.mu <- mean(df$Y)
  
  Y.likelihoods <- (1 / (Y.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( (df$Y-Y.mu) / Y.sigma )^2 )
  
  llX <- sum(log(X.likelihoods))
  
  llY <- sum(log(Y.likelihoods))
  
}

TlikelihoodTry <- function() {
  
  source('R/models.R')
  source('R/retrace_onePass_V4.R')
  
  df <- getData()
  
  singleFits <- fitSingleLimitModels(df)
  
  Tpred <- YlimResets(par=singleFits$Ylim$par, data=df)
  
  
  # x <- seq(pi/4, 5 * pi, length.out = 100)
  # y <- seq(pi/4, 5 * pi, length.out = 100)
  # r <- as.vector(sqrt(outer(x^2, y^2, "+")))
  # grid <- expand.grid(x=x, y=y)
  # grid$z <- cos(r^2) * exp(-r/(pi^3))
  
  plot(-1000,-1000,main='T-limit', xlab='', ylab='',
       xlim=c(-2,6),ylim=c(-.5,14),
       bty='n',ax=FALSE)
  
  title(xlab='x coordinate', line=2.5)
  title(ylab='y coordinate', line=2.5)
  
  x = seq(-2,6,0.1)
  y = seq(0,13.5,0.1)
  grid <- expand.grid(x=x, y=y)
  T.sigma <- sd(df$RT)
  T.mu <- mean(df$RT)
  grid$z <- (1 / (T.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( ( (sqrt(grid$x^2 + grid$y^2) / 3.5) - T.mu) / T.sigma )^2 )
  
  image(x=x, y=y, z=matrix(grid$z,ncol=length(y),nrow=length(x),byrow=FALSE),
        col = viridis::plasma(n=250, alpha=0.2),
        add = TRUE,
        oldstyle = FALSE, useRaster=FALSE)
  
  
  segments(df$X, df$Y, Tpred$X, Tpred$Y)
  
  axis(side=1, at=seq(-2,6,2))
  axis(side=2, at=seq(0,13.5,4.5))
  
  # X.likelihoods <- (1 / (X.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( (df$X-X.mu) / X.sigma )^2 )
  # 
  # Y.sigma <- sd(df$Y)
  # Y.mu <- mean(df$Y)
  # 
  # Y.likelihoods <- (1 / (Y.sigma * sqrt(2 * pi)) ) * exp( -0.5 * ( (df$Y-Y.mu) / Y.sigma )^2 )
  # 
  # llX <- sum(log(X.likelihoods))
  # 
  # llY <- sum(log(Y.likelihoods))
  
}


XTlikelihoodTry <- function() {
  
  
  source('R/models_likelihood.R')
  source('R/retrace_onePass_V4.R')
  
  df <- getData()
  
  
  avgpar <- c('mX' = 2.27786,
              'sX' = 0.7194591,
              'mT' = 1.679981,
              'sT' = 0.770661
              )
  
  prodpar <- c('mX' = 2.277853,
               'sX' = 0.719459,
               'mT' = 1.679974,
               'sT' = 0.770661
               )
  
  X = seq(-2,6,0.1)
  Y = seq(0,13.5,0.1)
  grid <- expand.grid(X=X, Y=Y)
  
  grid$speed <- 3.5
  grid$RT <- sqrt(grid$X^2 + grid$Y^2) / grid$speed
  
  probs <- XTlimProbabilities(par=prodpar,data=grid)^2
  
  plot(-1000,-1000,main='X&T limits', xlab='', ylab='',
       xlim=c(-2,6),ylim=c(-.5,14),
       bty='n',ax=FALSE)
  
  title(xlab='x coordinate', line=2.5)
  title(ylab='y coordinate', line=2.5)
  
  
  image(x=X, y=Y, z=matrix(as.numeric(probs$L),ncol=length(Y),nrow=length(X),byrow=FALSE),
        col = viridis::plasma(n=250, alpha=0.5),
        add = TRUE,
        oldstyle = FALSE, useRaster=FALSE)
  
  
  axis(side=1, at=seq(-2,6,2))
  axis(side=2, at=seq(0,13.5,4.5))
  
  
  
}
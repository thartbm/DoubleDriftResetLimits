


getColors <- function() {
  
  colors <- list()
  
  colors[['blue']]      <- list('s'='#005de4ff', 't'='#005de42f')
  
  colors[['lightblue']] <- list('s'='#0fd2e2ff', 't'='#0fd2e22f')
  
  colors[['yorkred']]   <- list('s'='#e51636ff', 't'='#e516362f')
  
  colors[['orange']]    <- list('s'='#ff8200ff', 't'='#ff82002f')
  
  colors[['purple']]    <- list('s'='#b400e4ff', 't'='#b400e42f')
  
  # colorset[['onlPasS']] <- '#8266f4ff' # violet
  # colorset[['onlPasT']] <- '#8266ff2f'
  
  # colorset[['onlPasS']] <- '#ff6ec7ff' # pink
  # colorset[['onlPasT']] <- '#ff6ec72f'
  
  return(colors)
  
}


getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - (z * sdx), xbar + (z * sdx)))
    
  }
  
  # add sample z-distribution?
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    return(quantile(BS, probs = c(lo,hi)))
    
  }
  
}
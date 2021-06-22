

`%notin%` <- Negate(`%in%`)



# 
# # pixels per cm
# 
# # resolution is 1680 x 1050 pixels
# # which spans ~43.5 x ~27 cm
# # for width that is roughly 38.799 pixels per cm
# # for depth that is roughly 38.888 pixels per cm
# # on average that would be: 38.755 pixels per cm
# 
# PPC <- 38.754789272
# 


getPPCdva <- function(lo=3) {
  
  props <- list()
  props['PPC'] <- 38.754789272 # pixels per cm
  
  # tan(A) = a/b
  
  dva <- expand.grid('eye.dist'  = seq(15, 25, length.out = lo), 
                     'gabor.pos' = seq(-6.75, 6.75, length.out = lo))
  
  b <- 27 # distance between mirror and monitor
  a <- dva$eye.dist + dva$gabor.pos # horizontal distance (along monitor) 
  
  # https://en.wikipedia.org/wiki/Trigonometry
  A <- atan(a/b) # angle at centre of gabor
  
  a1 <- tan(A+((0.5/180)*pi))*b
  a2 <- tan(A-((0.5/180)*pi))*b
  
  dva$cmpd <- a1 - a2
  
  ghw <- (100/3) / props[['PPC']]
  
  props[['gaborcm']] <- ghw
  
  dva$gabor.dva <- ghw / dva$cmpd
  
  props[['dva']] <- dva
  
  # visual size of the "workspace" (13.5 cm track)
  dva.track <- expand.grid('eye.dist'  = seq(15, 25, length.out = lo))
  
  b <- 27
  a <- dva.track$eye.dist
  
  A1 <- atan(a/(b-6.75)) # the angle of the close end at each head position
  A2 <- atan(a/(b+6.75)) # the angle of the far end at each head position
  
  dva.track$dva <- ((A1-A2)/pi)*180
  
  props[['track']] <- dva.track
  
  return(props)
  
}


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


rotateCoordinates <- function(df,angle,origin=c(0,0)) {
  
  df.names <- names(df)
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix, and subtract origin
  coordinates <- sweep(as.matrix(df), 2, origin)
  
  # rotate the coordinates, add the origin back in
  df <- as.data.frame(sweep(coordinates %*% R, 2, origin*-1))
  
  # restore column names
  names(df) <- df.names
  #print(df)
  # return the rotated coordinates
  return(df)
  
}

segmentMultiPassTrial <- function(df, velocityCutoff = NA, type='gaborzerocrossing') {
  
  # type: gaborreversal OR gaborzerocrossing 
  
  # first get the part where the gabor is actually moving (or present on the screen):
  gabormoves <- which(df$gabory_pix != 0 & !is.na(df$gabory_pix)) # second condition not technically necessary
  gaboronset <- min(gabormoves) - 1
  gabormoves <- c(min(gabormoves):max(gabormoves)) # in case a sample accidentally puts the gabor at exactly 0
  
  X <- df$handx_pix[gabormoves]
  Y <- df$handy_pix[gabormoves]
  
  if (type == 'gaborzerocrossing') {
    
    # now get the zero-crossings of the gabors (the middle of each segment will lag only briefly behind that):
    crossings <- zerocrossings(df$gabory_pix[gabormoves], bounces = FALSE)
    #print(crossings)
    segments <- list()
    
    for (cross.idx in c(1:length(crossings))) {
      
      #cat(sprintf('\ncrossing: %d\n\n', cross.idx))
      
      crossing <- crossings[cross.idx]
      
      # beginning of segment:
      seg.in <- crossing - which(abs(diff(sign(diff(rev(Y[1:crossing]))))) > 0)[1] + 1
      
      # end of segment:
      seg.out <- crossing + which(abs(diff(sign(diff(Y[crossing:length(Y)])))) > 0)[1] - 1
      
      #print(c(seg.in,seg.out))
      
      # velocity threshold?
      if (is.numeric(velocityCutoff)) {
        
        t <- df$time_ms[gabormoves]
        V <- getSplinedVelocity(X[c(seg.in:seg.out)], Y[c(seg.in:seg.out)], t[c(seg.in:seg.out)])$velocity
        
        #print(V)
        
        velCrit <- max(V) * velocityCutoff
        #print(velCrit)
        #print(which(V > velCrit))
        
        seg.in  <- seg.in  + (which(V      > velCrit)[1] - 1)
        seg.out <- seg.out - (which(rev(V) > velCrit)[1] - 1) 
        
      }
      
      seg.in  <- seg.in  + gaboronset
      seg.out <- seg.out + gaboronset
      
      # store segment indices:
      segments[[cross.idx]] <- c(seg.in, seg.out)
      
    }
    
    # return some list of segments: first and last sample/row indices of each segment?
    return(segments)
    
  }
  
  if (type == 'gaborreversal') {
    
    reversals <- directionchanges(df$gabory_pix[gabormoves])
    #print(reversals)
    segments <- list()
    
    for (rev.idx in c(1:(length(reversals)-1))) {
      
      # this ignores the lag of people's drawing after the gabor changes direction...
      seg.in  <- reversals[rev.idx] + 15
      seg.out <- reversals[rev.idx+1] - 14
      
      # velocity threshold?
      if (is.numeric(velocityCutoff)) {
        
        t <- df$time_ms[gabormoves]
        V <- getSplinedVelocity(X[c(seg.in:seg.out)], Y[c(seg.in:seg.out)], t[c(seg.in:seg.out)])$velocity
        
        velCrit <- max(V) * velocityCutoff
        
        seg.in  <- seg.in  + (which(V      > velCrit)[1] - 1)
        seg.out <- seg.out - (which(rev(V) > velCrit)[1] - 1) 
        
      }
      
      seg.in  <- seg.in  + gaboronset
      seg.out <- seg.out + gaboronset
      
      # store segment indices:
      segments[[rev.idx]] <- c(seg.in, seg.out)
      
    }
    
    # return some list of segments: first and last sample/row indices of each segment?
    return(segments)
    
  }
  
}

directionchanges <- function(v) {
  
  # d <- ((diff(v) > 0) - 0.5) * 2 # too simple?
  
  # this preserves zeroes and otherwise results in integers:
  # d <- diff(v)
  # d[which(d > 0)] <-  1
  # d[which(d < 0)] <- -1
  
  # same, but simpler:
  # d <- sign(diff(v))
  
  # everything converted to a one-liner:
  idx <- which(diff(sign(diff(v))) != 0) + 1 # not 0 OR larger than 1?
  
  return(idx)
  
}

zerocrossings <- function(v, bounces=FALSE) {
  
  output <- c()
  
  if (bounces) {
    
    zidx <- which(v==0) # bounces are only about samples that are exactly 0
    zidx <- zidx[which(zidx > 1 & zidx < length(v))] # first and last sample is not bounces
    
    bounce.idx <- zidx[which(diff(c(0,zidx)) > 1)] # we only want the first sample's index of every bounce
    
    output <- c(output, bounce.idx)
    
  }
  
  didx <- which(v!=0) # actual crossings detected using all non-zero samples
  signs <- sign(v[didx]) # get a vector of the signs of the non-zero samples
  cross.idx <- didx[which(abs(diff(signs)) == 2)] # whenever the abs diff between two of the sign entries equals 2
                                                  # zero has been crossed
  
  output <- sort(c(output, cross.idx))
  
  return(output)
  
}


getSplinedTrajectory <- function(x, y, t, length.out=length(t), spar=0.01) {
  
  spl.Xt <- smooth.spline(t, x, spar=spar, keep.data=F) 
  spl.Yt <- smooth.spline(t, y, spar=spar, keep.data=F) 
  
  tt <- seq( min(t), max(t), length.out = length.out )
  
  xx <- predict(spl.Xt, tt)$y
  yy <- predict(spl.Yt, tt)$y
  
  return(data.frame('x'=xx, 'y'=yy, 't'=tt))

}

getSplinedVelocity <- function(x, y, t, spar=0.01) {
  
  # spline interpolate the X and Y coordinates over time:
  # (separately... no multi-dimensional splining in base R)
  ST <- getSplinedTrajectory(x, y, t, length.out=length(t), spar=spar)
  
  # velocity on spline interpolated data
  V <- sqrt(diff(ST$x)^2 + diff(ST$y)^2) / diff(ST$t)
  
  return(data.frame('velocity'=V, 'time'=ST$t[2:dim(ST)[1]]))
  
}

confidenceEllipse <- function(x, y=NA, interval=.95, vectors=100) {
  
  # get the square root of the chi-squared value for the specified confidence interval:
  chisq.val <- sqrt(qchisq(p=interval, df=2))
  
  # get the covariance matrix of the data:
  if (is.matrix(x)) {
    covmat <- cov( x )
  } else {
    x <- matrix(c(x,y), ncol = 2, byrow = FALSE)
    covmat <- cov( x )
  }
  
  # get the centre of the ellipse:
  centre <- colMeans(x)
  
  # get the eigen decomposition of the covariance matrix
  ev <- eigen(covmat)
  
  # get eigenvalues and -vectors separately:
  eigenvalues <- ev$values
  eigenvectors <- ev$vectors
  
  # determine which is the maximum eigenvalue and -vector:
  max.EigVal.ind <- which.max(eigenvalues)
  
  max.EigVal <- eigenvalues[max.EigVal.ind]
  max.EigVec <- eigenvectors[,max.EigVal.ind]
  
  # and which are the minimum eigenvalue and -vector:
  min.EigVal.ind <- which.min(eigenvalues)
  min.EigVal <- eigenvalues[min.EigVal.ind]
  min.EigVec <- eigenvectors[,min.EigVal.ind]
  
  # calculate the angle of the largest eigen vector:
  phi = ( ( atan2(max.EigVec[2], max.EigVec[1]) %% (2*pi) ) / pi ) * 180;
  
  # ellipse angles:
  thetas <- seq(0,2*pi,length.out=vectors)
  
  # the semi-major and -minor axes:
  a <- chisq.val*sqrt(max.EigVal);
  b <- chisq.val*sqrt(min.EigVal);
  
  # get X and Y coordinates for the flat ellipse:
  X <- a*cos( thetas );
  Y <- b*sin( thetas );
  
  # rotate the ellipse:
  ellipse <- rotateCoordinates(df=data.frame(x=X, y=Y),angle=phi,origin=c(0,0))
  
  # re-centre:
  circumference$x <- ellipse$x + centre[1]
  circumference$y <- ellipse$y + centre[2]
  
  ellipse <- list()
  ellipse[['poly']] <- circumference
  ellipse[['major']] <- a
  ellipse[['minor']] <- b
  ellipse[['angle']] <- phi
  ellipse[['centre']] <- centre
  
  return(ellipse)
  
}



hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  #str(df)
  
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
    #cat('set edges from nbins\n')
    #print(x.edges)
    #print(y.edges)
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
    #cat('set edges from edges\n')
  }
  
  # cat('length x.edges:\n')
  # print(length(x.edges))
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = T, left.open = F, all.inside = F)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = T, left.open = F, all.inside = F)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  #freq <-  as.data.frame(table(xbincount,ybincount))
  #freq[,1] <- as.numeric(freq[,1])
  #freq[,2] <- as.numeric(freq[,2])
  
  #freq2D <- matrix(data=0, ncol=length(y.edges)-1, nrow=length(x.edges)-1)
  #freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  #print(freq2D)
  dimnames( freq2D ) <- c()
  rownames( freq2D ) <- c()
  colnames( freq2D ) <- c()
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}

polarHeatMap <- function(x,y,z,mincol=c(0.94,0.98,0.99),maxcol=c(0.06,0.82,0.88),xlim=NA,ylim=NA,xunit='degrees',border=NA,bordercol='white',resolution=1,alpha=1,overlay=FALSE,origin=c(0,0),scale=1,main='') {
  
  # x: area edges in some form of angles (degrees [default] or radians)
  # y: area edges in distances from the origin
  # z: matrix with values in regions defined by x & y edges (NA to not plot the area)
  
  # maybe x/y/z can be a data frame?
  
  # mincol: color for the minimum values in z
  # maxcol: color for the maximum values in z
  
  if (any(is.na(xlim))) {
    # set xlim to fit all possible data:
    #xlim <- rep(max(abs(y)),2) * c(-1,1)
    xlim <- c(-1,1)
  }
  if (any(is.na(ylim))) {
    # set xlim to fit all possible data:
    #ylim <- rep(max(abs(y)),2) * c(-1,1)
    ylim <- c(-1,1)
  }
  
  resolution <- (resolution/180)*pi
  
  #par(mar=c(4, 4, 2, 2), pin=c(width,height))
  
  if (overlay == FALSE) {
    
    plot.new() # is this allowed... yes, but then everything else has to be manually added?
    
    plot.window(xlim = xlim, ylim = ylim, asp = 1)
    title(main = main)
    
  }
  # use `rgb()`
  
  # scale z to fit into 0-1 range:
  z <- z - min(z)
  z <- z / max(z)
  
  # convert to RGB values within the min/max colors:
  R <- (z * (maxcol[1]-mincol[1])) + mincol[1]
  G <- (z * (maxcol[2]-mincol[2])) + mincol[2]
  B <- (z * (maxcol[3]-mincol[3])) + mincol[3]
  
  # scale y to fit into the figure circle:
  y <- y / max(y)
  
  # get x to be in radians:
  if (xunit == 'degrees') {
    x <- (x / 180) * pi
  }
  
  # print(dim(z))
  # print(length(x))
  # print(length(y))
  
  allPolygons <- list()
  # create all the polygons
  for (xi in seq(dim(z)[1])) {
    for (yi in seq(dim(z)[2])) {
      if (!is.na(z[xi,yi])) {
        x1 <- x[xi]
        x2 <- x[xi+1]
        y1 <- y[yi]
        y2 <- y[yi+1]
        #print(c(x1,x2))
        xs <- seq(x1,x2,length.out=ceiling(abs(diff(c(x1,x2)))/resolution))
        X <- (c(cos(xs)*y1, rev(cos(xs)*y2)) * scale) + origin[1]
        Y <- (c(sin(xs)*y1, rev(sin(xs)*y2)) * scale) + origin[2]
        allPolygons[[length(allPolygons)+1]] <- list('x'=X, 'y'=Y, 'col'=rgb(R[xi,yi],G[xi,yi],B[xi,yi],alpha=alpha))
      }
    }
  }
  
  for (p in allPolygons) {
    polygon(x=p$x, y=p$y, col=p$col, border=NA)
  }
  
  if (!is.na(border)) {
    for (p in allPolygons) {
      polygon(x=p$x, y=p$y, col=NA, border=bordercol, lwd=border)
    }
  }
  
  
}

density2D <- function(x, y, bw=1, weights=NULL, n=100, from=NULL, to=NULL, cut=3, na.rm=FALSE) {
  
  # have weights:
  if (!is.null(weights)) {
    if (length(x) != length(weights)) {
      cat('denisty2d error: weights need to be the same length as x and y\n')
      return()
    }
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1/length(x), length(x))
  }

  # remove NA values, if specified:
  if (na.rm) {
    idx <- intersect( which(!is.na(x)), which(!is.na(y)) )
    x <- x[idx]
    y <- y[idx]
    weights <- weights[idx]
  }
  
  # make sure we have bandwidth in the correct shape:
  if (length(bw) == 1) {
    bw <- c(bw,bw)
  }
  
  # set up the coordinates for the grid:
  if (is.null(from) | is.null(to)) {
    from <- c(min(x)-(cut*bw[1]),min(y)-(cut*bw[2]))
    to   <- c(max(x)+(cut*bw[1]),max(y)+(cut*bw[2]))
  }
  if (length(n) == 1) {
    n = c(n,n)
  }
  
  # make the actual grid:
  X <- seq(from[1], to[1], length.out=n[1])
  Y <- seq(from[2], to[2], length.out=n[2])
  grid <- expand.grid('x'=X,'y'=Y,'z'=NA)
  
  
  # get density at the grid points:
  for (idx in c(1:dim(grid)[1])) {
    grid$z[idx] <- sum( weights * ( 1/(2*pi*bw[1]+bw[2]) * exp( -1 * ((x-grid$x[idx])^2/(2*(bw[1]^2)) + (y-grid$y[idx])^2/(2*(bw[2]^2)))) ) )
  }
  
  return( list('x'=X,
               'y'=Y,
               'z'=matrix(grid$z, nrow=length(X)) ) )
  
}



## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END
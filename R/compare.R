

functionCavanaghTse2019 <- function(internal=c(), external=c(), k=0.81) {
  
  # speeds need to be in dva/s
  
  # perceived angle = arctan of ( k times internal motion over external motion )
  illusionstrength <- atan ( k * (internal %o% (1/external)) )
  
  # convert to degrees deviation from veridical:
  illusionstrength <- (illusionstrength / pi) * 180
  
  # add row and column names containing the speeds
  rownames(illusionstrength) <- as.character(internal)
  colnames(illusionstrength) <- as.character(external)
  
  # return the info
  return(illusionstrength)
  
}

plotStrengthComparison <- function() {
  

  #plot(c(5:35),IS['7.800',], type='l')
  
  plot(-1000,-1000,xlim=c(0,40), ylim=c(0,60), xlab='external motion [dva/s]', ylab='illusion strength [deg]', bty='n', ax=F)
  
  # use these external models for prediction lines:
  ems <- seq(3,35,0.2)
  
  
  # Cavanagh & Tse 2019 model:
  external <- c(3:35)
  internal <- c(7.8)
  IS_CT19 <- functionCavanaghTse2019(internal, external=ems)
  lines(ems,IS_CT19['7.8',],lty=1,col='#000000')
  
  text(x=36, y=IS_CT19['7.8', length(ems)], '7.8 dva/s', adj=c(0,0.5))
  text(x=36, y=20, 'internal\nmotion:', adj=c(0,0.5))
  
  ex_spd <- c(31.25, 15.63,  7.81, 15.63,  7.81,  3.91)
  illstr <- c(11.31, 21.57, 37.03, 19.41, 38.41, 60.12)
  
  points(ex_spd, illstr, pch=1, col='#000000')
  
  # predictions for experiment 1a:
  external <- c(7)
  internal <- c(1,3) * 1.5
  # our conditions:
  IS_Ex1a <- functionCavanaghTse2019(internal, external)
  lines(c(7,7),c(0,IS_Ex1a[2,'7']),col='#999999',lty=3)
  #text(x=7,y=-2.5,'1a',adj=c(0.5,0.5))
  

  IS_Ex1a <- functionCavanaghTse2019(internal, external=ems)
  lines(ems,IS_Ex1a['1.5',],lty=1,col='#999999')
  lines(ems,IS_Ex1a['4.5',],lty=1,col='#999999')
  
  text(x=36, y=IS_Ex1a['1.5',length(ems)], '1.5 dva/s', adj=c(0,.5))
  text(x=36, y=IS_Ex1a['4.5',length(ems)], '4.5 dva/s', adj=c(0,.5))
  
  # data for exp 1a (average heading):
  points(x=c(7,7,7), y=c(0, 9.7, 27.5), col='red', pch=19)
  
  # # predictions for experiment 1b:
  # external <- c(3.5)
  # internal <- c(3*1.5)
  # 
  # IS_Ex1b <- functionCavanaghTse2019(internal, external)
  # lines(c(3.5,3.5),c(0,IS_Ex1b[,'3.5']),lty=3,col='#999999')
  # text(x=3.5,y=-2.5,'1b',adj=c(0.5,0.5))
  # 
  # # data for exp 1b:
  # points(x=c(3.5), y=c(28.9), col='blue', pch=19)
  
  
  legend(x=15, y=60, legend=c('Cavanagh & Tse (2019) data+model', 'experiment 1'), col=c('#000000', '#FF0000'), pch=c(1,19), lty=c(1,0), bty='n')
  
  axis(side=1, at=seq(0,35,5))
  axis(side=2, at=seq(0,60,10))
  
  
  
}
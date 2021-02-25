

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

plotStrengthComparison <- function(target='svg') {
  
  if (target == 'pdf') {
    cairo_pdf(filename='doc/tracking_CTmodel.pdf',onefile=TRUE,width=5,height=5)
  }
  if (target == 'svg') {
    svglite(file='doc/tracking_CTmodel.svg',width=5,height=5)
  }
  
  colors <- getColors()
  
  #par(mfrow=c(1,2))
  
  
  stdsegments <- read.csv('data/bounded_tracking/segment_directions.csv', stringsAsFactors = F)  
  
  stdsegments$direction[which(stdsegments$direction < 0)] <- NA
  stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
  
  internalSpeeds <- sort(unique(stdsegments$internalSpeed))
  participants <- sort(unique(stdsegments$participant))
  
  internalspeed <- c()
  participant <- c()
  avgheading <- c()
  
  for (internalSpeed.idx in c(1:length(internalSpeeds))) {
    
    internalSpeed <- internalSpeeds[internalSpeed.idx]
    
    for (pp in participants) {
      
      # this is the part of the data we're dealing with now:
      idx <- which(stdsegments$internalSpeed == internalSpeed & stdsegments$participant == pp)
      
      # # we put it into a normalized 2D histogram:
      # pfreq <- hist2d(x=stdsegments$direction[idx], y=stdsegments$sample_no[idx], nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
      # 
      # if (!is.matrix(freq2D)) {
      #   freq2D <- pfreq$freq2D
      # } else {
      #   freq2D <- freq2D + pfreq$freq2D
      # }
      
      # also get the average, to plot on top of the polar heat map:
      # or justs to plot a point for comparison with the Cavanagh & Tse 2019 model
      PPavgDir <- aggregate(direction ~ sample_no, data = stdsegments[idx,], FUN=mean, na.rm=T)
      
      internalspeed <- c(internalspeed, internalSpeed)
      participant <- c(participant, pp)
      avgheading <- c(avgheading, 90-mean(PPavgDir$direction))
      
    }
    
  }
  
  headingdir <- data.frame(internalspeed, participant, avgheading)
  idx <- which(headingdir$internalspeed < 0)
  headingdir$avgheading[idx] <- headingdir$avgheading[idx] * -1 
  headingdir$internalspeed[idx] <- headingdir$internalspeed[idx] * -1 
  
  #str(headingdir)
  
  headingdir <- aggregate(cbind(avgheading, participant) ~ internalspeed + participant, data=headingdir, FUN=mean)
  #str(headingdir)
  #plot(c(5:35),IS['7.800',], type='l')
  
  # plot(-1000,-1000,xlim=c(0,25), ylim=c(0,60), xlab='external motion [dva/s]', ylab='illusion strength [deg]', bty='n', ax=F)
  # 
  # # use these external models for prediction lines:
  # ems <- seq(3,18,0.2)
  # 
  # 
  # # # Cavanagh & Tse 2019 model:
  # # external <- c(3:35)
  # # internal <- c(7.8)
  # # IS_CT19 <- functionCavanaghTse2019(internal, external=ems)
  # # lines(ems,IS_CT19['7.8',],lty=1,col='#000000')
  # # 
  # # text(x=36, y=IS_CT19['7.8', length(ems)], '7.8 dva/s', adj=c(0,0.5))
  # # text(x=36, y=20, 'internal\nmotion:', adj=c(0,0.5))
  # # 
  # # ex_spd <- c(31.25, 15.63,  7.81, 15.63,  7.81,  3.91)
  # # illstr <- c(11.31, 21.57, 37.03, 19.41, 38.41, 60.12)
  # # 
  # # points(ex_spd, illstr, pch=1, col='#000000')
  # 
  # # predictions for experiment 1a:
  # external <- c(7)
  # internal <- c(1,3) * 1.5
  # # our conditions:
  # IS_Ex1a <- functionCavanaghTse2019(internal, external)
  # lines(c(7,7),c(0,IS_Ex1a[2,'7']),col='#999999',lty=3)
  # #text(x=7,y=-2.5,'1a',adj=c(0.5,0.5))
  # 
  # 
  # IS_Ex1a <- functionCavanaghTse2019(internal, external=ems)
  # lines(ems,IS_Ex1a['1.5',],lty=1,col='#999999')
  # lines(ems,IS_Ex1a['4.5',],lty=1,col='#999999')
  # 
  # text(x=19, y=IS_Ex1a['1.5',length(ems)], '1.5 dva/s', adj=c(0,.5))
  # text(x=19, y=IS_Ex1a['4.5',length(ems)], '4.5 dva/s', adj=c(0,.5))
  # 
  # # data for exp 1a (average heading):
  # points(x=c(7,7,7), y=c(0, 9.7, 27.5), col='red', pch=19)
  # 
  # # # predictions for experiment 1b:
  # # external <- c(3.5)
  # # internal <- c(3*1.5)
  # # 
  # # IS_Ex1b <- functionCavanaghTse2019(internal, external)
  # # lines(c(3.5,3.5),c(0,IS_Ex1b[,'3.5']),lty=3,col='#999999')
  # # text(x=3.5,y=-2.5,'1b',adj=c(0.5,0.5))
  # # 
  # # # data for exp 1b:
  # # points(x=c(4.5), y=c(28.9), col='blue', pch=19)
  # 
  # 
  # legend(x=4, y=62, legend=c('Cavanagh & Tse (2019)', 'experiment 1'), col=c('#999999', '#FF0000'), pch=c(NA,19), lty=c(1,0), bty='n')
  # 
  # axis(side=1, at=seq(0,20,5))
  # axis(side=2, at=seq(0,60,20))
  
  plot(-1000,-1000,xlim=c(0,7), ylim=c(0,45), xlab='internal motion [dva/s]', ylab='illusion strength [deg]', bty='n', ax=F)
  
  # use these internal motions for prediction:
  ims <- seq(0,7,0.1)
  prediction <- functionCavanaghTse2019(internal=ims, external=c(7))
  lines(ims,prediction[,'7'],lty=1,col='#999999')
  
  points(headingdir$internalspeed*1.5, headingdir$avgheading, col=colors$lightblue$s, pch=1, cex=1.0)
  
  agg.headdir <- aggregate(avgheading ~ internalspeed, data=headingdir, FUN=mean)
  
  points(agg.headdir$internalspeed*1.5, agg.headdir$avgheading, col=colors$yorkred$s, pch=1, cex=1.5)
  

  # text(x=19, y=IS_Ex1a['1.5',length(ems)], '1.5 dva/s', adj=c(0,.5))
  # text(x=19, y=IS_Ex1a['4.5',length(ems)], '4.5 dva/s', adj=c(0,.5))
  # 
  # # data for exp 1a (average heading):
  # points(x=c(7,7,7), y=c(0, 9.7, 27.5), col='red', pch=19)
  # 
  legend(x=2.5, y=10, 
         legend=c('participants', 'average', 'Cavanagh & Tse (2019)'), 
         col=c(colors$lightblue$s, colors$yorkred$s, '#999999'), 
         pch=c(1,1,NA), lty=c(0,0,1), 
         bty='n', cex=1)
  
  axis(side=1, at=seq(0,7,1))
  axis(side=2, at=seq(0,40,10))
  
  if (target %in% c('svg', 'pdf')) {
    dev.off()
  }
  
}

plotStrengthComparison2 <- function() {
  
  colors <- getColors()
  
  external <- c(7)
  internal <- c(0,1,3) * 1.5
  # our conditions:
  IS_Ex1a <- functionCavanaghTse2019(internal, external)
  

  plot(-1000,-1000,xlim=c(0,40),ylim=c(0,40),xlab='predicted illusion strength [deg]',ylab='illusion strength [deg]',asp=1,bty='n',ax=F)
  
  lines(c(0,40),c(0,40),col='#999999',lty=1)
  
  for (ISidx in c(1,2,3)) {
    
    IS <- c(0,1,3)[ISidx]
    prediction <- IS_Ex1a[ISidx,'7']
    
    illusionstrengths <- headingdir$avgheading[which(headingdir$internalspeed == IS)]
    
    points(rep(prediction,length(illusionstrengths)), illusionstrengths, col='red')
    
  }
  
  axis(side=1, at=seq(0,40,10))
  axis(side=2, at=seq(0,40,10))
  
}
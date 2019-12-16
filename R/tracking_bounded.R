library('svglite')
source('R/common.R')

# Data handling -----

getBoundedTrackingTimeNormalizedSegments <- function(segmentpoints=101) {
  
  # use these participants:
  participants <- c(1,3,5,6,7,8)
  participants <- c(3,6,7,8) # 1 and 5 are authors
  
  # read all the data into one data frame:
  # BTdata <- NA
  BTSdata <- NA
  
  for (ppno in participants) {
    
    ppdf <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno), stringsAsFactors = F)
    ppdf$participant <- ppno
    
    # # get all raw data in one frame?
    # if (is.data.frame(BTdata)) {
    #   BTdata <- rbind(BTdata, ppdf)
    # } else {
    #   BTdata <- ppdf
    # }
    
    # segments:
    trials <- unique(ppdf$trial_no)
    #print(trials)
    for (trialnum in trials) {
      #print(length(trialnum))
      trialdf <- ppdf[which(ppdf$trial_no == trialnum),]
      # normalize for external direction:
      trialdf$handx_pix <- trialdf$handx_pix * trialdf$externalDirection[1]
      
      # split into segments:
      tracksegments <- segmentMultiPassTrial(trialdf, velocityCutoff = 0.10)
      #print(tracksegments)
      for (seg.no in c(1:length(tracksegments))) {
        seg <- tracksegments[[seg.no]]
        segmentdf <- trialdf[c(seg[1]:seg[2]),]
        #segmentdf$segment_no <- seg.no
        
        if (dim(segmentdf)[1] < 32) {
          # skip segments that are less than 1 second (out of ~2 seconds)
          #cat('segment too short...\n')
          next()
        } else {
          #cat('GREAT SEGMENT!\n')
        }
        
        # if the trajectory goes down in the segment, we rotate the whole thing:
        if (segmentdf$gabory_pix[1] > segmentdf$gabory_pix[dim(segmentdf)[1]]) {
          # which is done fastest by flipping both the X and Y coordinates:
          
          
          segmentdf$handx_pix <- rev(segmentdf$handx_pix)
          segmentdf$handy_pix <- rev(segmentdf$handy_pix)
          # probably also for the gabor:
          segmentdf$gabory_pix <- rev(segmentdf$gabory_pix)
          segmentdf$gaborx_pix <- rev(segmentdf$gaborx_pix)
          
          
          #cat('rotate trajectory\n')
          # print(ppno)
          # print(trialnum)
          # print(seg.no)
          #cat(sprintf('rotate for participant %d, trial %d, segment %d\n', ppno, trial_no, seg.no))
        } # else {
        #   cat('non-rotated trajectory\n')
        # }
        
        # now set the origin to (0,0) for all segments:
        segmentdf$handx_pix <- segmentdf$handx_pix - segmentdf$handx_pix[1]
        segmentdf$handy_pix <- segmentdf$handy_pix - segmentdf$handy_pix[1]
        
        # make new vectors for new data frame:
        participant <-       rep(ppno,                           segmentpoints)
        trial_no <-          rep(segmentdf$trial_no[1],          segmentpoints)
        fixationside <-      rep(segmentdf$fixationside[1],      segmentpoints)
        internalSpeed <-     rep(segmentdf$internalSpeed[1],     segmentpoints)
        externalDirection <- rep(segmentdf$externalDirection[1], segmentpoints)
        segment_no <-        rep(seg.no,                         segmentpoints)
        sample_no <-         seq(segmentpoints)
        # step <- segmentdf$step # always 2: forget it
        # gabororientation <- segmentdf$gabororientation # always 0: forget it
        # gaborphase # could be interpolated, but doesn't seem that important: forget it
        
        # interpolate these ones:
        gaborsplined <- getSplinedTrajectory(x=segmentdf$gaborx_pix, y=segmentdf$gabory_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.01)
        gaborx_pix <- gaborsplined$x
        gabory_pix <- gaborsplined$y
        
        handsplined <- getSplinedTrajectory(x=segmentdf$handx_pix, y=segmentdf$handy_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.01)
        handx_pix <- handsplined$x
        handy_pix <- handsplined$y
        time_ms <- handsplined$t # should be the exact same as gabor splined time, but not checking 
        
        segmentdf <- data.frame(participant, trial_no, fixationside, internalSpeed, externalDirection, segment_no, sample_no, time_ms, gaborx_pix, gabory_pix, handx_pix, handy_pix)
        
        #print(str(segmentdf))
        
        if (is.data.frame(BTSdata)) {
          BTSdata <- rbind(BTSdata, segmentdf)
        } else {
          BTSdata <- segmentdf
        }
      }
    }
  }
  
  write.csv(BTSdata, file='data/bounded_tracking/normalized_segments.csv', row.names = F, quote = F)
  
}

getBoundedTrackingStandardizedSegments <- function(segmentpoints=101) {
  
  # use these participants:
  participants <- c(1,3,5,6,7,8)
  participants <- c(3,6,7,8) # 1 and 5 are authors
  
  # read all the data into one data frame:
  # BTdata <- NA
  BTSdata <- NA
  
  for (ppno in participants) {
    
    ppdf <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno), stringsAsFactors = F)
    ppdf$participant <- ppno
    
    # # get all raw data in one frame?
    # if (is.data.frame(BTdata)) {
    #   BTdata <- rbind(BTdata, ppdf)
    # } else {
    #   BTdata <- ppdf
    # }
    
    # segments:
    trials <- unique(ppdf$trial_no)
    #print(trials)
    for (trialnum in trials) {
      #print(length(trialnum))
      trialdf <- ppdf[which(ppdf$trial_no == trialnum),]
      # normalize for external direction:
      trialdf$handx_pix <- trialdf$handx_pix * trialdf$externalDirection[1]
      
      # split into segments:
      tracksegments <- segmentMultiPassTrial(trialdf, velocityCutoff = NA, type='gaborreversal')
      #print(tracksegments)
      for (seg.no in c(1:length(tracksegments))) {
        seg <- tracksegments[[seg.no]]
        segmentdf <- trialdf[c(seg[1]:seg[2]),]
        #segmentdf$segment_no <- seg.no
        
        if (dim(segmentdf)[1] < 32) {
          # skip segments that are less than 1 second (out of ~2 seconds)
          #cat('segment too short...\n')
          next()
        } else {
          #cat('GREAT SEGMENT!\n')
        }
        
        # if the trajectory goes down in the segment, we rotate the whole thing:
        if (segmentdf$gabory_pix[1] > segmentdf$gabory_pix[dim(segmentdf)[1]]) {
          # which is done fastest by flipping both the X and Y coordinates:
          
          
          segmentdf$handx_pix <- rev(segmentdf$handx_pix)
          segmentdf$handy_pix <- rev(segmentdf$handy_pix)
          # probably also for the gabor:
          segmentdf$gabory_pix <- rev(segmentdf$gabory_pix)
          segmentdf$gaborx_pix <- rev(segmentdf$gaborx_pix)
          
        } 
        
        # now set the origin to (0,0) for all segments:
        segmentdf$handx_pix <- segmentdf$handx_pix - segmentdf$handx_pix[1]
        segmentdf$handy_pix <- segmentdf$handy_pix - segmentdf$handy_pix[1]
        
        # make new vectors for new data frame:
        participant <-       rep(ppno,                           segmentpoints)
        trial_no <-          rep(segmentdf$trial_no[1],          segmentpoints)
        fixationside <-      rep(segmentdf$fixationside[1],      segmentpoints)
        internalSpeed <-     rep(segmentdf$internalSpeed[1],     segmentpoints)
        externalDirection <- rep(segmentdf$externalDirection[1], segmentpoints)
        segment_no <-        rep(seg.no,                         segmentpoints)
        sample_no <-         seq(segmentpoints)
        # step <- segmentdf$step # always 2: forget it
        # gabororientation <- segmentdf$gabororientation # always 0: forget it
        # gaborphase # could be interpolated, but doesn't seem that important: forget it
        
        # interpolate these ones:
        gaborsplined <- getSplinedTrajectory(x=segmentdf$gaborx_pix, y=segmentdf$gabory_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.1)
        gaborx_pix <- gaborsplined$x
        gabory_pix <- gaborsplined$y
        
        handsplined <- getSplinedTrajectory(x=segmentdf$handx_pix, y=segmentdf$handy_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.1)
        handx_pix <- handsplined$x
        handy_pix <- handsplined$y
        time_ms <- handsplined$t # should be the exact same as gabor splined time, but not checking 
        
        segmentdf <- data.frame(participant, trial_no, fixationside, internalSpeed, externalDirection, segment_no, sample_no, time_ms, gaborx_pix, gabory_pix, handx_pix, handy_pix)
        
        #print(str(segmentdf))
        
        if (is.data.frame(BTSdata)) {
          BTSdata <- rbind(BTSdata, segmentdf)
        } else {
          BTSdata <- segmentdf
        }
      }
    }
  }
  
  write.csv(BTSdata, file='data/bounded_tracking/standardized_segments.csv', row.names = F, quote = F)
  
}

getSegmentDirections <- function(allsegmentdata) {
  
  allsegmentdata$direction <- NA
  
  seg.IDs <- unique(allsegmentdata[c('participant', 'trial_no', 'segment_no')])
  # 1178 segments from 6 participants (almost 200 each: 22 got removed)
  
  for (seg.no in c(1:dim(seg.IDs)[1])) {
    seg.idx <- which( allsegmentdata$participant == seg.IDs$participant[seg.no] &
                      allsegmentdata$trial_no    == seg.IDs$trial_no[seg.no]    &
                      allsegmentdata$segment_no  == seg.IDs$segment_no[seg.no]    )
    segment <- allsegmentdata[seg.idx,]
    allsegmentdata$direction[seg.idx] <- c(NA, ( atan2(diff(segment$handy_pix), diff(segment$handx_pix)) / pi ) * 180)
  }
  
  return(allsegmentdata)
  
}

# Figures -----

plotBoundedTrackingRaw <- function(target='inline') {
  
  participants <- c(1,3,5,6,7,8)
  participants <- c(3,6,7,8)
  
  if (target=='svg') {
    svglite(file='doc/Fig03.svg',width=6,height=6)
  }
  
  par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
  
  colors <- getColors()
  
  red <- colors$yorkred
  
  plot(-1000,-1000,main='',ylim=c(0.5,5.5),xlim=c(0.5,length(participants)+1),xlab='participant',ylab='internal speed [cps]',asp=1,bty='n',ax=F)

  speeds <- c(-3,-1,0,1,3)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speedidx in c(1:length(speeds))) {
      
      speed <- speeds[speedidx]
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) 
      
      for (trialno in trials) {
        
        idx <- which(sdf$trial_no == trialno)
        
        t <- sdf$time_ms[idx]
        t <- (t - t[1]) / 1000
        x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
        y <- sdf$handy_pix[idx]
        
        x <- (x / 960)
        x <- x + ppidx
        
        y <- (y / 960)
        y <- y + speedidx
        
        lines(x,y,col=red$t,lw=2)
        
      }
      
    }
    
  }
  
  axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
  axis(side=2,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

plotBoundedTracking <- function(target='inline') {
  
  colors <- getColors()

  #rawsegments <- read.csv('data/bounded_tracking/normalized_segments.csv', stringsAsFactors = F)  
  stdsegments <- read.csv('data/bounded_tracking/standardized_segments.csv', stringsAsFactors = F)  
  
  stdsegments <- getSegmentDirections(stdsegments)
  stdsegments$direction[which(stdsegments$direction < 0)] <- NA
  stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
  
  
  if (target=='svg') {
    svglite(file='doc/Fig03b.svg',width=6,height=6)
  }
  
  # 5 conditions (columns)
  # row 1: heading distribution
  # row 2-5: participants
  
  internalSpeeds <- sort(unique(stdsegments$internalSpeed))
  participants <- sort(unique(stdsegments$participant))
  
  par(mar=c(4.1,4.1,2.1,0.1))
  
  plot(-1000,-1000,main='',ylim=c(0.5,length(participants)+1.5),xlim=c(0.5,5.5),ylab='participant',xlab='internal speed [cps]',asp=1,bty='n',ax=F)
  
  outlierTrials <- NA
  
  for (internalSpeed.idx in c(1:length(internalSpeeds))) {
    
    internalSpeed <- internalSpeeds[internalSpeed.idx]
    
    freq2D <- NA
    avgDir <- c()
    
    for (participant in participants) {
      
      # this is the part of the data we're dealing with now:
      idx <- which(stdsegments$internalSpeed == internalSpeed & stdsegments$participant == participant)
      
      # we put it into a normalized 2D histogram:
      pfreq <- hist2d(x=stdsegments$direction[idx], y=stdsegments$sample_no[idx], nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
      #pfreq$freq2D <- sqrt(pfreq$freq2D / sum(pfreq$freq2D))
      
      if (!is.matrix(freq2D)) {
        freq2D <- pfreq$freq2D
      } else {
        freq2D <- freq2D + pfreq$freq2D
      }
      
      # also get the average, to plot on top of the polar heat map:
      PPavgDir <- aggregate(direction ~ sample_no, data = stdsegments[idx,], FUN=mean, na.rm=T)
      avgDir <- c(avgDir, PPavgDir$direction)
      
      # determin the extra-ordinary trials: mean +/- 3 std? (for every timepoint?)
      #PPstdDir <- aggregate(direction ~ sample_no, data = allsegments[idx,], FUN=sd, na.rm=T)
      stdDir <- sd(stdsegments$direction[idx], na.rm=T)
      outliers <- which(stdsegments$direction[idx] - mean(stdsegments$direction[idx], na.rm=T) > (4 * stdDir))
      
      if (length(outliers) > 3) {
        #cat(sprintf('speed: %d, participant: %d\n', internalSpeed, participant))
        outlierdf <- unique(stdsegments[idx[outliers],c('participant','internalSpeed','trial_no')])
        if (is.data.frame(outlierTrials)) {
          outlierTrials <- rbind(outlierTrials, outlierdf)
        } else {
          outlierTrials <- outlierdf
        }
      }
      
      
    }
    
    polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main=sprintf('%d cps', internalSpeed), overlay=TRUE, origin=c(internalSpeed.idx,4.65), scale=0.65)
    
    avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
    text(internalSpeed.idx, 5.5, sprintf('%0.1f°',90 - ( ( mean(avgDir) / pi) * 180 ) ) )
    scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30)) * 0.65
    adX <- (cos(avgDir) * scale) + internalSpeed.idx
    adY <- (sin(avgDir) * scale) + 4.65
    lines(adX,adY,col=colors[['blue']]$s,lw=2)
    
  }
  
  speeds <- c(-3,-1,0,1,3)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speedidx in c(1:length(speeds))) {
      
      speed <- speeds[speedidx]
      
      outlier_trials <- outlierTrials$trial_no[which(outlierTrials$participant == ppno & outlierTrials$internalSpeed == speed)]
      
      #plot(-1000,-1000,main='',ylim=c(-0.5,0.5),xlim=c(-0.5,0.5),xlab='',ylab='',asp=1,bty='n',ax=F)
      
      sdf <- df[which(df$internalSpeed == speed),]
      
      trials <- unique(sdf$trial_no) 
      
      for (trialno in trials) {
        
        idx <- which(sdf$trial_no == trialno)
        
        t <- sdf$time_ms[idx]
        t <- (t - t[1]) / 1000
        x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
        y <- sdf$handy_pix[idx]
        
        x <- (x / 960)
        x <- x + speedidx
        
        y <- (y / 960)
        y <- y + ppidx
        
        col <- '#0000000f'
        if (trialno %in% outlier_trials) {
          col <- colors[['yorkred']]$s
        }
        
        lines(x,y,col=col,lw=2)
        
      }
      
    }
    
  }
  
  axis(side=2,at=c(1:length(participants)),labels = sprintf('%d',participants))
  axis(side=1,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
  
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

placeHolderOne <- function(target='inline') {
  
  colors <- getColors()
  blue <- colors[['blue']]$s
  
  if (target=='svg') {
    svglite(file='doc/Fig03b.svg',width=7,height=4)
  }
  
  # start polar direction heat maps
  
  allsegments <- read.csv('data/bounded_tracking/standardized_segments.csv', stringsAsFactors = F)
  allsegments <- getSegmentDirections(allsegments)
  allsegments <- allsegments[which(!is.na(allsegments$direction)),]
  
  allsegments$direction[which(allsegments$direction < 0)] <- NA
  
  internalSpeeds <- sort(unique(allsegments$internalSpeed))
  participants <- sort(unique(allsegments$participant))
  
  #par(mfrow=c(length(internalSpeeds),6),mar=c(4.5,0.1,2.1,0.1))
  par(mfrow=c(1,length(internalSpeeds)),mar=c(4.5,0.1,2.1,0.1))
  
  for (internalSpeed in internalSpeeds) {
    
    freq2D <- NA
    avgDir <- c()
    
    for (participant in participants) {
      
      # this is the part of the data we're dealing with now:
      idx <- which(allsegments$internalSpeed == internalSpeed & allsegments$participant == participant)
      
      # we put it into a normalized 2D histogram:
      pfreq <- hist2d(x=allsegments$direction[idx], y=allsegments$sample_no[idx], nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
      #pfreq$freq2D <- sqrt(pfreq$freq2D / sum(pfreq$freq2D))
      
      if (!is.matrix(freq2D)) {
        freq2D <- pfreq$freq2D
      } else {
        freq2D <- freq2D + pfreq$freq2D
      }
      
      # also get the average, to plot on top of the polar heat map:
      PPavgDir <- aggregate(direction ~ sample_no, data = allsegments[idx,], FUN=mean, na.rm=T)
      avgDir <- c(avgDir, PPavgDir$direction)
      
    }
    #print(pfreq$freq2D)
    
    polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main=sprintf('internal speed: %d', internalSpeed))
    
    avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
    
    # print(avgDir)
    scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30))
    adX <- (cos(avgDir) * scale)
    adY <- (sin(avgDir) * scale)
    lines(adX,adY,col=blue)
    
  }
  
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}
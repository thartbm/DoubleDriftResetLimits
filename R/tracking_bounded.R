library('svglite')
library('ez')

source('R/common.R')

# Data handling -----

getBoundedTrackingStandardizedSegments <- function(segmentpoints=101, version=1) {
  
  # use these participants:
  participants <- c(1,3,5,6,7,8)
  participants <- c(3,6,7,8) # 1 and 5 are authors
  folderstr <- 'data/bounded_tracking/'
  filestr <- 'bounded_tracking_'
  
  if (version == 2) {
    participants <- c(1,2,9,10,11,12,13)
    participants <- c(2,9,10,11,12,13) # 1 is author
    folderstr <- 'data/bounded_tracking_V2/'
    filestr <- 'bounded_tracking_V2_'
  }
  
  # read all the data into one data frame:
  # BTdata <- NA
  BTSdata <- NA
  
  for (ppno in participants) {
    
    ppdf <- read.csv(sprintf('%s%sp%02d.csv',folderstr,filestr,ppno), stringsAsFactors = F)
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
        
        if (version == 2) {
          segmentdf$handx_pix <- segmentdf$handx_pix * sign(segmentdf$internalSpeed[1])
        }
        
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
  
  write.csv(BTSdata, file=sprintf('%sstandardized_segments.csv',folderstr), row.names = F, quote = F)
  
}

getSegmentDirections <- function(version=1) {
  
  folderstr <- 'data/bounded_tracking/'
  if (version == 2) {
    folderstr <- 'data/bounded_tracking_V2/'
  }
  
  allsegmentdata <- read.csv(sprintf('%sstandardized_segments.csv', folderstr), stringsAsFactors = F) 
  
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
  
  write.csv(allsegmentdata, sprintf('%ssegment_directions.csv', folderstr), row.names = F, quote = F)
  
}


getAverageHeading <- function(mirror=TRUE) {
  
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
  
  
  if (mirror) {
    idx <- which(headingdir$internalspeed < 0)
    headingdir$avgheading[idx] <- headingdir$avgheading[idx] * -1 
    headingdir$internalspeed[idx] <- headingdir$internalspeed[idx] * -1 
    
    headingdir <- aggregate(cbind(avgheading) ~ internalspeed + participant, data=headingdir, FUN=mean)
    
  }
  
  return(headingdir)
  
}


# Figures -----


plotBoundedTracking <- function(target='inline') {
  
  colors <- getColors()
  
  stdsegments <- read.csv('data/bounded_tracking/segment_directions.csv', stringsAsFactors = F)  
  
  stdsegments$direction[which(stdsegments$direction < 0)] <- NA
  stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
  
  
  if (target=='svg') {
    svglite(file='doc/Fig03b.svg',width=8,height=5)
  }
  
  
  layout(matrix(c(1,2,1,3), nrow = 2, ncol = 2, byrow = T), heights=c(1.25,2.25), widths=c(1,1))
  
  # 5 conditions or internal speeds (columns)
  # row 1: heading distribution
  # row 2-5: participants
  
  internalSpeeds <- sort(unique(stdsegments$internalSpeed))
  participants <- sort(unique(stdsegments$participant))
  
  par(mar=c(3.75,3.75,2.1,0.1))
  
  
  plot(-1000,-1000,main='online tracking',
       ylim=c(0.75,(length(participants)*1.5)+0.75),xlim=c(0.5,5.5),
       ylab='',xlab='',
       asp=1,bty='n',ax=F)
  
  title(xlab='internal speed [cps]', line=2.5)
  title(ylab='participant', line=2.5)
  
  
  # four choice trials that _might_ have a reset...
  outlierTrials <- data.frame( 'participant'   = c(  8,  7,  3, 8  ),
                               'internalSpeed' = c( -3,  1,  3, 3  ),
                               'trial_no'      = c( 33,  1, 10, 25 ))
  
  # outlierTrials <- outlierTrials[which(outlierTrials$trial_no > 9),]
  # print(outlierTrials)
  
  speeds <- c(-3,-1,0,1,3)
  
  for (ppidx in c(1:length(participants))) {
    
    ppno <- participants[ppidx]
    
    df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
    
    df <- df[which(df$step == 2),]
    
    for (speedidx in c(1:length(speeds))) {
      
      speed <- speeds[speedidx]
      
      outlier_trials <- outlierTrials$trial_no[which(outlierTrials$participant == ppno & outlierTrials$internalSpeed == speed)]
      
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
        #x <- x + ppidx
        
        y <- (y / 960)
        y <- y + (ppidx * 1.5)
        #y <- y + speedidx
        
        col <- '#0000000f'
        if (trialno %in% outlier_trials) {
          col <- colors[['yorkred']]$s
        }
        
        lines(x,y,col=col,lw=2)
        
      }
      
    }
    
  }
  
  axis(side=2,at=seq(1:length(participants))*1.5,labels=sprintf('%d',c(1:length(participants))))
  axis(side=1,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
  
  
  plot(-1000,-1000,main='heading',ylim=c(1,1.7),xlim=c(0.5,5.5),ylab='',xlab='',asp=1,bty='n',ax=F)
  
  title(xlab='internal speed [cps]', line=2.5)
  # title(ylab='participant', line=2.5)
  
  #outlierTrials <- NA
  
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
      
    }
    
    polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, 
                 mincol=c(1,1,1), border=NA, ylim=c(0,1), 
                 main=sprintf('%d cps', internalSpeed), 
                 overlay=TRUE, origin=c(internalSpeed.idx,1), scale=0.65)
    
    avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
    text(internalSpeed.idx, 1.85, sprintf('%0.1f°',90 - ( ( mean(avgDir) / pi) * 180 ) ) )
    scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30)) * 0.65
    adX <- (cos(avgDir) * scale) + internalSpeed.idx
    adY <- (sin(avgDir) * scale) + 1
    lines(adX,adY,col=colors[['blue']]$s,lw=2)
    
  }
  
  axis(side=1,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
  
  
  df <- getAverageHeading()
  
  internalspeed <- df$internalspeed / 0.58 # in cm/s 
  externalspeed <- 13.5             / 2    # in cm/s
  
  xcoords <- atan(internalspeed / externalspeed)
  
  
  plot(-1000,-1000,main='illusion strength',
       xlab=expression(paste(tan^{-1}, (V[i]/V[e]), ' [°]')),ylab='illusion strength [°]',
       xlim=c(pi*-((0.25)/9),pi/4),ylim=c(-5,45),
       bty='n',ax=F)
  
  angles <- c(0,pi/4)
  lines(angles,(angles/pi)*180,col='gray',lty=2)
  lines(angles,0.81*((angles/pi)*180),col='black',lty=1)
  
  
  points(xcoords, df$avgheading, col=colors$lightblue$s, pch=1, cex=1.0)
  
  agg.df <- aggregate(avgheading ~ internalspeed, data=df, FUN=mean)
  
  points(atan((agg.df$internalspeed / 0.58) / externalspeed), agg.df$avgheading, col=colors$yorkred$s, pch=1, cex=1.5)
  
  
  
  # get the best k for this data:
  X <- (xcoords/pi)*180
  Y <- as.numeric(unlist(df$avgheading))
  linmod <- lm(Y ~ X - 1)
  slope <- summary(linmod)$coefficients['X','Estimate']
  
  # plot that as a line:
  lines(angles,slope*((angles/pi)*180),col=colors$purple$s,lty=1)
  
  
  
  legend(x=0, y=45, 
         legend=c('Cavanagh & Tse (2019)', 'participants', 'average'), 
         col=c('black', colors$lightblue$s, colors$yorkred$s), 
         pch=c(NA,1,1), lty=c(1,0,0), 
         bty='n', cex=1)
  
  #axis(side=1,at=seq(0,pi/4,pi/8),labels=c('0',expression(pi/8),expression(pi/4)))
  axis(side=1,at=seq(0,pi/4,pi/12),labels=sprintf('%d',seq(0,45,15)))
  axis(side=2,at=seq(0,45,15))
  
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

# plotBoundedTrackingOld <- function(target='inline') {
#   
#   colors <- getColors()
# 
#   stdsegments <- read.csv('data/bounded_tracking/segment_directions.csv', stringsAsFactors = F)  
# 
#   stdsegments$direction[which(stdsegments$direction < 0)] <- NA
#   stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
#   
#   
#   if (target=='svg') {
#     svglite(file='doc/Fig03b.svg',width=6,height=6)
#   }
#   
#   # 5 conditions or internal speeds (columns)
#   # row 1: heading distribution
#   # row 2-5: participants
#   
#   internalSpeeds <- sort(unique(stdsegments$internalSpeed))
#   participants <- sort(unique(stdsegments$participant))
#   
#   par(mar=c(3.75,3.75,2.1,0.1),mfrow=c(1,1))
#   
#   plot(-1000,-1000,main='',ylim=c(0.5,length(participants)+2),xlim=c(0.5,5.5),ylab='',xlab='',asp=1,bty='n',ax=F)
#   
#   title(xlab='internal speed [cps]', line=2.5)
#   title(ylab='participant', line=2.5)
#   
#   #outlierTrials <- NA
#   
#   for (internalSpeed.idx in c(1:length(internalSpeeds))) {
#     
#     internalSpeed <- internalSpeeds[internalSpeed.idx]
#     
#     freq2D <- NA
#     avgDir <- c()
#     
#     for (participant in participants) {
#       
#       # this is the part of the data we're dealing with now:
#       idx <- which(stdsegments$internalSpeed == internalSpeed & stdsegments$participant == participant)
#       
#       # we put it into a normalized 2D histogram:
#       pfreq <- hist2d(x=stdsegments$direction[idx], y=stdsegments$sample_no[idx], nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
#       #pfreq$freq2D <- sqrt(pfreq$freq2D / sum(pfreq$freq2D))
#       
#       if (!is.matrix(freq2D)) {
#         freq2D <- pfreq$freq2D
#       } else {
#         freq2D <- freq2D + pfreq$freq2D
#       }
#       
#       # also get the average, to plot on top of the polar heat map:
#       PPavgDir <- aggregate(direction ~ sample_no, data = stdsegments[idx,], FUN=mean, na.rm=T)
#       avgDir <- c(avgDir, PPavgDir$direction)
#       
#       # # determin the extra-ordinary trials: mean +/- 3 std? (for every timepoint?)
#       # #PPstdDir <- aggregate(direction ~ sample_no, data = allsegments[idx,], FUN=sd, na.rm=T)
#       # stdDir <- sd(stdsegments$direction[idx], na.rm=T)
#       # outliers <- which(stdsegments$direction[idx] - mean(stdsegments$direction[idx], na.rm=T) > (4 * stdDir))
#       # 
#       # if (length(outliers) > 3) {
#       #   #cat(sprintf('speed: %d, participant: %d\n', internalSpeed, participant))
#       #   outlierdf <- unique(stdsegments[idx[outliers],c('participant','internalSpeed','trial_no')])
#       #   if (is.data.frame(outlierTrials)) {
#       #     outlierTrials <- rbind(outlierTrials, outlierdf)
#       #   } else {
#       #     outlierTrials <- outlierdf
#       #   }
#       # }
#       
#       
#     }
#     
#     polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main=sprintf('%d cps', internalSpeed), overlay=TRUE, origin=c(internalSpeed.idx,5), scale=0.65)
#     
#     avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
#     text(internalSpeed.idx, 5.85, sprintf('%0.1f°',90 - ( ( mean(avgDir) / pi) * 180 ) ) )
#     scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30)) * 0.65
#     adX <- (cos(avgDir) * scale) + internalSpeed.idx
#     adY <- (sin(avgDir) * scale) + 5
#     lines(adX,adY,col=colors[['blue']]$s,lw=2)
#     
#   }
#   
#   # four choice trials that _might_ have a reset...
#   outlierTrials <- data.frame( 'participant'   = c(  8,  7,  3, 8  ),
#                                'internalSpeed' = c( -3,  1,  3, 3  ),
#                                'trial_no'      = c( 33,  1, 10, 25 ))
#   
#   # outlierTrials <- outlierTrials[which(outlierTrials$trial_no > 9),]
#   # print(outlierTrials)
#   
#   speeds <- c(-3,-1,0,1,3)
#   
#   for (ppidx in c(1:length(participants))) {
#     
#     ppno <- participants[ppidx]
#     
#     df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
#     
#     df <- df[which(df$step == 2),]
#     
#     for (speedidx in c(1:length(speeds))) {
#       
#       speed <- speeds[speedidx]
#       
#       outlier_trials <- outlierTrials$trial_no[which(outlierTrials$participant == ppno & outlierTrials$internalSpeed == speed)]
#       
#       #plot(-1000,-1000,main='',ylim=c(-0.5,0.5),xlim=c(-0.5,0.5),xlab='',ylab='',asp=1,bty='n',ax=F)
#       
#       sdf <- df[which(df$internalSpeed == speed),]
#       
#       trials <- unique(sdf$trial_no) 
#       
#       for (trialno in trials) {
#         
#         idx <- which(sdf$trial_no == trialno)
#         
#         t <- sdf$time_ms[idx]
#         t <- (t - t[1]) / 1000
#         x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
#         y <- sdf$handy_pix[idx]
#         
#         x <- (x / 960)
#         x <- x + speedidx
#         #x <- x + ppidx
#         
#         y <- (y / 960)
#         y <- y + ppidx
#         #y <- y + speedidx
#         
#         col <- '#0000000f'
#         if (trialno %in% outlier_trials) {
#           col <- colors[['yorkred']]$s
#         }
#         
#         lines(x,y,col=col,lw=2)
#         
#       }
#       
#     }
#     
#   }
#   
#   axis(side=2,at=c(1:length(participants),5.4),labels = c(sprintf('%d',participants),'heading'))
#   axis(side=1,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
#   
#   
#   if (target %in% c('svg')) {
#     dev.off()
#   }
#   
# }


plotBoundedTracking_V2 <- function(target='inline') {
  
  colors <- getColors()
  
  stdsegments <- read.csv('data/bounded_tracking_V2/segment_directions.csv', stringsAsFactors = F)  
  
  stdsegments$direction[which(stdsegments$direction < 0)] <- NA
  stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
    
  
  if (target=='svg') {
    svglite(file='doc/Fig03e.svg',width=6,height=6)
  }
  
  layout(matrix(c(1,2,7,3,4,7,5,6,7), 3, 3, byrow = TRUE), widths=c(1,1,3), heights=c(1,1,1))
  
  participants <- sort(unique(stdsegments$participant))
  
  par(mar=c(4.1,4.1,2.1,0.1))
  
  freq2D <- NA
  avgDir <- c()
  
  for (participant in participants) {
    
    # this is the part of the data we're dealing with now:
    idx <- which(stdsegments$participant == participant)
    
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
    
    # plot this participant:
    polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main=sprintf('participant: %d', participant), overlay=FALSE)
    
    scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30))
    adX <- (cos((PPavgDir$direction/180)*pi) * scale)
    adY <- (sin((PPavgDir$direction/180)*pi) * scale)
    lines(adX,adY,col=colors[['blue']]$s,lw=2)
    # text(0,0,)
  }
  
  polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main='group average', overlay=FALSE)
  
  ppAvgDirs <-  90 - colMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE)  ) 
  print(ppAvgDirs)
  print(mean(ppAvgDirs))
  
  avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
  text(0, 1.2, sprintf('%0.1f°',90 - ( ( mean(avgDir) / pi) * 180 ) ) )
  #scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30))
  adX <- (cos(avgDir) * scale)
  adY <- (sin(avgDir) * scale)
  lines(adX,adY,col=colors[['blue']]$s,lw=2)
  
  
  if (target %in% c('svg')) {
    dev.off()
  }
  
}

# Statistics -----

analyzeBoundedTracking <- function(target='inline') {
  

  stdsegments <- read.csv('data/bounded_tracking/segment_directions.csv', stringsAsFactors = F)  
  
  stdsegments$direction[which(stdsegments$direction < 0)] <- NA
  stdsegments <- stdsegments[which(!is.na(stdsegments$direction)),]
  
  internalSpeeds <- sort(unique(stdsegments$internalSpeed))
  participants <- sort(unique(stdsegments$participant))
  
  # build data frame for data analyses:
  participant <- c()
  internal_speed <- c()
  direction <- c()
  
  for (internalSpeed.idx in c(1:length(internalSpeeds))) {
    
    intSpeed <- internalSpeeds[internalSpeed.idx]
    
    freq2D <- NA
    avgDir <- c()
    
    for (ppno in participants) {
      
      # this is the part of the data we're dealing with now:
      idx <- which(stdsegments$internalSpeed == intSpeed & stdsegments$participant == ppno)
      
      PPavgDir <- aggregate(direction ~ sample_no, data = stdsegments[idx,], FUN=mean, na.rm=T)
      
      participant <- c(participant, ppno)
      internal_speed <- c(internal_speed, intSpeed)
      direction <- c(direction, 90 - mean(PPavgDir$direction) )
      
    }
    
  }
  
  # now we get a fairly small data set to analyse:
  anaData <- data.frame(participant, internal_speed, direction)
  
  # first an F-test (or one-factort ANOVA) to see if internal speed affects heading:
  anaData$participant <- as.factor(anaData$participant)
  anaData$internal_speed <- as.factor(anaData$internal_speed)
  
  if ('ez' %in% installed.packages()) {
    
    AOVobj <-  ezANOVA(data=anaData, dv=direction, wid=participant, within=internal_speed, type=3)
    
    cat('\n**\n** repeated measures F-test on a model predicting heading from internal speed:\n**\n\n')
    print(AOVobj)
    
  }
  
  # post-hoc test: which internal speeds result in different headings from each other?
  # Tukey's HSD? why not...
  # because `TukeyHSD()` can't deal with repeated measures designs... 
  
  # this has too many comparisons, that are all two-sided
  # print( pairwise.t.test( x=anaData$direction, g=anaData$internal_speed, paired=TRUE, p.adjust.method = 'fdr') )
  
  # with luck we only need these four comparison, all of them paired, one-sided t-tests:
  comparisons <- list( '-3 -1' = list(-3, -1, 'l'),
                       '-1  0' = list(-1,  0, 'l'),
                       ' 0  1' = list( 0,  1, 'l'),
                       ' 1  3' = list( 1,  3, 'l')
  )
  pvals <- list()
  for (comparison in names(comparisons)) {
    
    isps <- comparisons[[comparison]]
    
    tt <- t.test(anaData$direction[which(anaData$internal_speed == isps[[1]])],
                 anaData$direction[which(anaData$internal_speed == isps[[2]])],
                 alternative=isps[[3]],
                 paired=TRUE)
    # cat(sprintf('\ninternal speed: %s\n\n',comparison))
    # print(tt)
    
    pvals[[comparison]] <- tt$p.value
    
  }
  
  cat('\n**\n** FDR-corrected p-values from 4 paired, one-sided t-tests on mean heading\n** (between neighbouring internal speeds)\n**\n\n')
  print(p.adjust(p=pvals, method='fdr'))
  

}

# Code Graveyard -----


# getBoundedTrackingTimeNormalizedSegments <- function(segmentpoints=101) {
#   
#   # use these participants:
#   participants <- c(1,3,5,6,7,8)
#   participants <- c(3,6,7,8) # 1 and 5 are authors
#   
#   # read all the data into one data frame:
#   # BTdata <- NA
#   BTSdata <- NA
#   
#   for (ppno in participants) {
#     
#     ppdf <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno), stringsAsFactors = F)
#     ppdf$participant <- ppno
#     
#     # # get all raw data in one frame?
#     # if (is.data.frame(BTdata)) {
#     #   BTdata <- rbind(BTdata, ppdf)
#     # } else {
#     #   BTdata <- ppdf
#     # }
#     
#     # segments:
#     trials <- unique(ppdf$trial_no)
#     #print(trials)
#     for (trialnum in trials) {
#       #print(length(trialnum))
#       trialdf <- ppdf[which(ppdf$trial_no == trialnum),]
#       # normalize for external direction:
#       trialdf$handx_pix <- trialdf$handx_pix * trialdf$externalDirection[1]
#       
#       # split into segments:
#       tracksegments <- segmentMultiPassTrial(trialdf, velocityCutoff = 0.10)
#       #print(tracksegments)
#       for (seg.no in c(1:length(tracksegments))) {
#         seg <- tracksegments[[seg.no]]
#         segmentdf <- trialdf[c(seg[1]:seg[2]),]
#         #segmentdf$segment_no <- seg.no
#         
#         if (dim(segmentdf)[1] < 32) {
#           # skip segments that are less than 1 second (out of ~2 seconds)
#           #cat('segment too short...\n')
#           next()
#         } else {
#           #cat('GREAT SEGMENT!\n')
#         }
#         
#         # if the trajectory goes down in the segment, we rotate the whole thing:
#         if (segmentdf$gabory_pix[1] > segmentdf$gabory_pix[dim(segmentdf)[1]]) {
#           # which is done fastest by flipping both the X and Y coordinates:
#           
#           
#           segmentdf$handx_pix <- rev(segmentdf$handx_pix)
#           segmentdf$handy_pix <- rev(segmentdf$handy_pix)
#           # probably also for the gabor:
#           segmentdf$gabory_pix <- rev(segmentdf$gabory_pix)
#           segmentdf$gaborx_pix <- rev(segmentdf$gaborx_pix)
#           
#           
#           #cat('rotate trajectory\n')
#           # print(ppno)
#           # print(trialnum)
#           # print(seg.no)
#           #cat(sprintf('rotate for participant %d, trial %d, segment %d\n', ppno, trial_no, seg.no))
#         } # else {
#         #   cat('non-rotated trajectory\n')
#         # }
#         
#         # now set the origin to (0,0) for all segments:
#         segmentdf$handx_pix <- segmentdf$handx_pix - segmentdf$handx_pix[1]
#         segmentdf$handy_pix <- segmentdf$handy_pix - segmentdf$handy_pix[1]
#         
#         # make new vectors for new data frame:
#         participant <-       rep(ppno,                           segmentpoints)
#         trial_no <-          rep(segmentdf$trial_no[1],          segmentpoints)
#         fixationside <-      rep(segmentdf$fixationside[1],      segmentpoints)
#         internalSpeed <-     rep(segmentdf$internalSpeed[1],     segmentpoints)
#         externalDirection <- rep(segmentdf$externalDirection[1], segmentpoints)
#         segment_no <-        rep(seg.no,                         segmentpoints)
#         sample_no <-         seq(segmentpoints)
#         # step <- segmentdf$step # always 2: forget it
#         # gabororientation <- segmentdf$gabororientation # always 0: forget it
#         # gaborphase # could be interpolated, but doesn't seem that important: forget it
#         
#         # interpolate these ones:
#         gaborsplined <- getSplinedTrajectory(x=segmentdf$gaborx_pix, y=segmentdf$gabory_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.01)
#         gaborx_pix <- gaborsplined$x
#         gabory_pix <- gaborsplined$y
#         
#         handsplined <- getSplinedTrajectory(x=segmentdf$handx_pix, y=segmentdf$handy_pix, t=segmentdf$time_ms, length.out=segmentpoints, spar=0.01)
#         handx_pix <- handsplined$x
#         handy_pix <- handsplined$y
#         time_ms <- handsplined$t # should be the exact same as gabor splined time, but not checking 
#         
#         segmentdf <- data.frame(participant, trial_no, fixationside, internalSpeed, externalDirection, segment_no, sample_no, time_ms, gaborx_pix, gabory_pix, handx_pix, handy_pix)
#         
#         #print(str(segmentdf))
#         
#         if (is.data.frame(BTSdata)) {
#           BTSdata <- rbind(BTSdata, segmentdf)
#         } else {
#           BTSdata <- segmentdf
#         }
#       }
#     }
#   }
#   
#   write.csv(BTSdata, file='data/bounded_tracking/normalized_segments.csv', row.names = F, quote = F)
#   
# }


# plotBoundedTrackingRaw <- function(target='inline') {
#   
#   participants <- c(1,3,5,6,7,8)
#   participants <- c(3,6,7,8)
#   
#   if (target=='svg') {
#     svglite(file='doc/Fig03.svg',width=6,height=6)
#   }
#   
#   par(mfrow=c(1,1),mar=c(4.5,4.1,0.1,0.1))
#   
#   colors <- getColors()
#   
#   red <- colors$yorkred
#   
#   plot(-1000,-1000,main='',ylim=c(0.5,5.5),xlim=c(0.5,length(participants)+1),xlab='participant',ylab='internal speed [cps]',asp=1,bty='n',ax=F)
#   
#   speeds <- c(-3,-1,0,1,3)
#   
#   for (ppidx in c(1:length(participants))) {
#     
#     ppno <- participants[ppidx]
#     
#     df <- read.csv(sprintf('data/bounded_tracking/bounded_tracking_p%02d.csv',ppno))
#     
#     df <- df[which(df$step == 2),]
#     
#     for (speedidx in c(1:length(speeds))) {
#       
#       speed <- speeds[speedidx]
#       
#       sdf <- df[which(df$internalSpeed == speed),]
#       
#       trials <- unique(sdf$trial_no) 
#       
#       for (trialno in trials) {
#         
#         idx <- which(sdf$trial_no == trialno)
#         
#         t <- sdf$time_ms[idx]
#         t <- (t - t[1]) / 1000
#         x <- sdf$handx_pix[idx] * sdf[idx,]$externalDirection[1]
#         y <- sdf$handy_pix[idx]
#         
#         x <- (x / 960)
#         x <- x + ppidx
#         
#         y <- (y / 960)
#         y <- y + speedidx
#         
#         lines(x,y,col=red$t,lw=2)
#         
#       }
#       
#     }
#     
#   }
#   
#   axis(side=1,at=c(1:length(participants)),labels = sprintf('%d',participants))
#   axis(side=2,at=c(1:length(speeds)),labels = sprintf('%d',speeds))
#   
#   if (target %in% c('svg')) {
#     dev.off()
#   }
#   
# }



# placeHolderOne <- function(target='inline') {
#   
#   colors <- getColors()
#   blue <- colors[['blue']]$s
#   
#   if (target=='svg') {
#     svglite(file='doc/Fig03b.svg',width=7,height=4)
#   }
#   
#   # start polar direction heat maps
#   
#   allsegments <- read.csv('data/bounded_tracking/standardized_segments.csv', stringsAsFactors = F)
#   allsegments <- getSegmentDirections(allsegments)
#   allsegments <- allsegments[which(!is.na(allsegments$direction)),]
#   
#   allsegments$direction[which(allsegments$direction < 0)] <- NA
#   
#   internalSpeeds <- sort(unique(allsegments$internalSpeed))
#   participants <- sort(unique(allsegments$participant))
#   
#   #par(mfrow=c(length(internalSpeeds),6),mar=c(4.5,0.1,2.1,0.1))
#   par(mfrow=c(1,length(internalSpeeds)),mar=c(4.5,0.1,2.1,0.1))
#   
#   for (internalSpeed in internalSpeeds) {
#     
#     freq2D <- NA
#     avgDir <- c()
#     
#     for (participant in participants) {
#       
#       # this is the part of the data we're dealing with now:
#       idx <- which(allsegments$internalSpeed == internalSpeed & allsegments$participant == participant)
#       
#       # we put it into a normalized 2D histogram:
#       pfreq <- hist2d(x=allsegments$direction[idx], y=allsegments$sample_no[idx], nbins=NA, edges=list(seq(0,180,length.out=61), seq(1,101,4)+0.5))
#       #pfreq$freq2D <- sqrt(pfreq$freq2D / sum(pfreq$freq2D))
#       
#       if (!is.matrix(freq2D)) {
#         freq2D <- pfreq$freq2D
#       } else {
#         freq2D <- freq2D + pfreq$freq2D
#       }
#       
#       # also get the average, to plot on top of the polar heat map:
#       PPavgDir <- aggregate(direction ~ sample_no, data = allsegments[idx,], FUN=mean, na.rm=T)
#       avgDir <- c(avgDir, PPavgDir$direction)
#       
#     }
#     #print(pfreq$freq2D)
#     
#     polarHeatMap(x=pfreq$x.edges, y=pfreq$y.edges+30, z=freq2D, mincol=c(1,1,1), border=NA, ylim=c(0,1), main=sprintf('internal speed: %d', internalSpeed))
#     
#     avgDir <- (rowMeans( matrix(avgDir, ncol=length(participants), byrow=FALSE) ) / 180) * pi
#     
#     # print(avgDir)
#     scale <- ((PPavgDir$sample_no+30) / (max(PPavgDir$sample_no)+30))
#     adX <- (cos(avgDir) * scale)
#     adY <- (sin(avgDir) * scale)
#     lines(adX,adY,col=blue)
#     
#   }
#   
#   
#   if (target %in% c('svg')) {
#     dev.off()
#   }
#   
# }

# do the psychophysics -----

getPSEs <- function(groups=c('exp','ctrl'), overwrite=FALSE) {
  
  for (group in groups) {
    
    # check if the file is already there, skip if it is:
    if (file.exists(sprintf('data/gaborjump/%s_PSEs.csv', group))) {
      next
    }
    
    # extracted data:
    df <- read.csv(sprintf('data/gaborjump/gaborjump_%s_data.csv',group), stringsAsFactors = FALSE)
    
    # invert leftward trials:
    ext1 <- which(df$exmotion == 1)
    df$gapshift[ext1] <- -1 * df$gapshift[ext1]
    df$response[ext1] <- -1 * df$response[ext1]
    
    # we want to scale between 0 and 1:
    df$response[which(df$response == -1)] <- 0
    
    # get mean proportion of right-ward responses per condition:
    agg_df <- aggregate(response ~ participant + path_length + stimdur + exmotion + gapshift, data=df, FUN=mean)
    
    conditions <- expand.grid('path_length'=unique(agg_df$path_length),
                              #'exmotion'=unique(agg_df$exmotion),
                              'stimdur'=unique(agg_df$stimdur)
    )
    
    
    participant <- c()
    path_length <- c()
    stimdur     <- c()
    #exmotion    <- c()
    PSE         <- c()
    std         <- c()
    
    
    # need to know the participants, so we can analyze their data separately:
    participants <- unique(agg_df$participant)
    
    # 12 PSEs per participant:
    #layout(mat=matrix(c(1:12),ncol = 4,nrow = 3, byrow = TRUE))
    
    gapshifts_df <- data.frame('gapshift'=seq(-4,4,length.out = 161))
    
    for (ppno in participants) {
      
      p_df <- agg_df[which(agg_df$participant == ppno),]
      
      for (c.row in c(1:dim(conditions)[1])) {
        c_df <- df[which(df$participant == ppno &
                           df$path_length == conditions$path_length[c.row] &
                           #df$exmotion    == conditions$exmotion[c.row] &
                           df$stimdur     == conditions$stimdur[c.row]),]
        #print(c_df)
        #model <- glm(response ~ gapshift, data=c_df, family = binomial(link="probit"))
        
        #model <- glm(response ~ gapshift, data=c_df, family = binomial(mafc.probit(2)))
        #print(summary(model))
        
        # this is for a logit link function:
        # m   <- -coef(model)[[1]]/coef(model)[[2]]
        # std <- 1/coef(model)[[2]]
        # cat(sprintf('\n\nmean: %0.3f, sd: %0.3f\n\n',m,std))
        
        #print(model$coefficients)
        
        
        a_c_df <- aggregate(response ~ gapshift, data=c_df, FUN=mean)
        #plot(a_c_df$gapshift, a_c_df$response, col='red', main='',xlab='',ylab='',xlim=c(-4,4),ylim=c(0,1))
        
        #lines(gapshifts_df$gapshift, predict(object=model, newdata=gapshifts_df))
        #print(predict(object=model, newdata=gapshifts_df))
        
        #cndfit <- fit_cndf(x=a_c_df$gapshift,y=a_c_df$response)
        cndfit <- fit_cndf(x=c_df$gapshift,y=c_df$response)
        #print(cndfit)
        #lines(seq(-4,4,length.out = 161),pnorm(q=seq(-4,4,length.out = 161),mean=cndfit['mu'],sd=cndfit['sigma']),col='blue')
        
        participant <- c(participant, ppno)
        path_length <- c(path_length, conditions$path_length[c.row])
        stimdur     <- c(stimdur, conditions$stimdur[c.row])
        #exmotion    <- c(exmotion, conditions$exmotion[c.row])
        PSE         <- c(PSE, cndfit['mu'])
        std         <- c(std, cndfit['sigma'])
        
      }
      
    }
    
    ppno <- 'group'
    
    for (c.row in c(1:dim(conditions)[1])) {
      c_df <- df[which(  df$path_length == conditions$path_length[c.row] &
                           #df$exmotion    == conditions$exmotion[c.row] &
                           df$stimdur     == conditions$stimdur[c.row]),]
      #model <- glm(response ~ gapshift, data=c_df, family = binomial(link="probit"))
      
      a_c_df <- aggregate(response ~ gapshift, data=c_df, FUN=mean)
      cndfit <- fit_cndf(x=c_df$gapshift,y=c_df$response)
      
      
      
      participant <- c(participant, ppno)
      path_length <- c(path_length, conditions$path_length[c.row])
      stimdur     <- c(stimdur, conditions$stimdur[c.row])
      #exmotion    <- c(exmotion, conditions$exmotion[c.row])
      PSE         <- c(PSE, cndfit['mu'])
      std         <- c(std, cndfit['sigma'])
      
    }
    
    
    df <- data.frame(participant,
                     path_length,
                     stimdur,
                     #                 exmotion,
                     PSE,
                     std)
    
    write.csv(x         = df, 
              file      = sprintf('data/gaborjump/%s_PSEs.csv',group),
              row.names = FALSE)
  }
}

library('optimx')

fit_cndf <- function(x,y) {
  
  # define search grip points:
  mu=seq(min(x), max(x), length.out = 41)
  sigma=seq(.4, diff(range(x))*2, length.out = 41)
  
  # make search grid data frames:
  searchgrid <- expand.grid('mu'=mu, 'sigma'=sigma)

  # get error for each point in the grid:
  MSEs <- apply(searchgrid,MARGIN=c(1),FUN=cdf_err,x=x,y=y)
  
  # get some of the best points in the grid:
  topgrid <- searchgrid[order(MSEs)[c(1,5,9)],]
  
  # do the actual fitting:
  allFits <- do.call("rbind",
                     apply( topgrid,
                            MARGIN=c(1),
                            FUN=optimx,
                            fn=cdf_err,
                            method='L-BFGS-B',
                            lower=c(min(x),.2),
                            upper=c(max(x),diff(range(x))),
                            x=x,
                            y=y) )
  
  # pick the best fit:
  optfit <- allFits[order(allFits$value, decreasing = TRUE)[1],]
  
  # we're only interested in the distribution moments for now:
  return(c('mu'=optfit$mu,'sigma'=optfit$sigma))
  
}

cdf_err <- function(par, x, y) {
  
  return( mean( (y - pnorm(q=x, mean=par['mu'], sd=par['sigma']))^2 ) )
  
}

# stats on psychophsics -----

library('ez')

ANOVAonPSEs <- function() {
  
  for (group in c('exp','ctrl')) {
    
    df <- read.csv(sprintf('data/gaborjump/%s_PSEs.csv',group), stringsAsFactors = FALSE)
    
    df <- df[which(df$participant != 'group'),]
    
    df$participant <- as.factor(df$participant)
    df$path_length <- as.factor(df$path_length)
    df$stim_dur <- as.factor(df$stimdur)
    
    EA <- ez::ezANOVA( data = df,
                       dv = PSE,
                       wid = participant,
                       within = c(path_length, stim_dur),
                       detailed=TRUE)
    
    # add partial eta squared column:
    EA <- ezANOVA.pes(EA)
    print(EA)
    
  }
  
}
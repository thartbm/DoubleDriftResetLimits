# check / download data -----

library('osfr')

# this function will download and unzip the data from OSF:

OSFdata <- function(overwrite=FALSE, removezip=FALSE, extradata=FALSE) {
  
  repo <- osfr::osf_retrieve_node('72ndu')
  
  dirs <- osfr::osf_ls_files(repo, type='folder')
  
  idx <- which(dirs$name == 'data')
  
  datadir <- osfr::osf_ls_files(dirs[idx,])
  
  zipfiles <- c('bounded_tracking.zip','onepass_V4.zip')
  if (extradata) {
    zipfiles <- c(zipfiles, 'infinite_tracking.zip', 'pilots2.zip', 'CVRdemo.zip')
  }
  
  if (overwrite) {
    conflicts <- 'overwrite'
  } else {
    conflicts <- 'skip'
  }
  
  for (zipfile in zipfiles) {
    
    idx <- which(datadir$name == zipfile)
    
    if (overwrite | !file.exists(sprintf('data/%s',zipfile))) {
      osfr::osf_download(datadir[idx,], path='data/', conflicts=conflicts)
    }
    
  }
  
  for (zipfile in zipfiles) {
    
    fullzipfile <- sprintf('data/%s',zipfile)
    unzip(fullzipfile, exdir='data')
    if (removezip) {
      file.remove(fullzipfile)
    }
    
  }
  
}


# code graveyard -----

# the stuff below is old-fashioned
# superseded by osfr functionality

#source('R/tracking_reset_pause.R')

# downloadRawData <- function(overwrite=TRUE, experiments=c('bounded_tracking', 'bounded_tracking_V2', 'onePass_V4')) {
#   
#   
#   for (experiment in experiments) {
#     
#     if (experiment == 'bounded_tracking') {
#       
#       folder = 'data/bounded_tracking'
#       
#       if (!dir.exists(folder)) {
#         dir.create(folder)
#       }
#       
#       resources <- c( 
#         
#         'bounded_tracking_p01.csv' = 'https://osf.io/xrbp7/download',
#         'bounded_tracking_p03.csv' = 'https://osf.io/2p3nc/download',
#         'bounded_tracking_p05.csv' = 'https://osf.io/trzdx/download',
#         'bounded_tracking_p06.csv' = 'https://osf.io/qywdc/download',
#         'bounded_tracking_p07.csv' = 'https://osf.io/59pcd/download',
#         'bounded_tracking_p08.csv' = 'https://osf.io/sfjc6/download',
#         
#         'bounded_tracking_participants.csv' = 'https://osf.io/gtycq/download'
#         
#       )
#       
#       downloadFiles(files=resources, folder=folder, overwrite=overwrite)
#       
#       next()
#       
#     }
#     
#     if (experiment == 'onePass_V4') {
#       
#       folder = 'data/onePass_V4'
#       
#       if (!dir.exists(folder)) {
#         dir.create(folder)
#       }
#       
#       # participants 1 and 7 are not used
#       # I am participant 1 (I think),
#       # participant 7 had the opposite percept of what was expected
#       # (despite doing the bounded tracking task too, and having normal percepts there)
#       
#       resources <- c( 
#         
#         'onepass_V4_p01.csv' = 'https://osf.io/cnbxm/download',
#         'onepass_V4_p02.csv' = 'https://osf.io/uvz4y/download',
#         'onepass_V4_p03.csv' = 'https://osf.io/zpckg/download',
#         'onepass_V4_p04.csv' = 'https://osf.io/5wkp7/download',
#         'onepass_V4_p05.csv' = 'https://osf.io/6u85g/download',
#         'onepass_V4_p06.csv' = 'https://osf.io/bvqt4/download',
#         'onepass_V4_p07.csv' = 'https://osf.io/a6e43/download',
#         'onepass_V4_p08.csv' = 'https://osf.io/vpw3n/download',
#         'onepass_V4_p09.csv' = 'https://osf.io/tm3c8/download',
#         'onepass_V4_p10.csv' = 'https://osf.io/8u9rb/download',
#         'onepass_V4_p11.csv' = 'https://osf.io/v4d8p/download',
#         
#         'onepass_V4_participants.csv' = 'https://osf.io/e2k8c/download'
#         
#       )
#       
#       downloadFiles(files=resources, folder=folder, overwrite=overwrite)
#       
#       next()
#       
#     }
#     
#     if (experiment == 'bounded_tracking_V2') {
#       
#       folder = 'data/bounded_tracking_V2'
#       
#       if (!dir.exists(folder)) {
#         dir.create(folder)
#       }
# 
#       resources <- c( 
#         
#         'bounded_tracking_V2_p01.csv' = 'https://osf.io/akyf4/download',
#         'bounded_tracking_V2_p02.csv' = 'https://osf.io/gzkqf/download',
#         'bounded_tracking_V2_p09.csv' = 'https://osf.io/wdq7p/download',
#         'bounded_tracking_V2_p10.csv' = 'https://osf.io/8g4q9/download',
#         'bounded_tracking_V2_p11.csv' = 'https://osf.io/axqcy/download',
#         'bounded_tracking_V2_p12.csv' = 'https://osf.io/fjpz6/download',
#         'bounded_tracking_V2_p13.csv' = 'https://osf.io/cyj49/download',
#         
#         'bounded_tracking_V2_participants.csv' = 'https://osf.io/jzp5m/download'
#         
#         # 'reset_p01.csv' = 'https://osf.io/739dp/download',
#         # 'reset_p02.csv' = 'https://osf.io/w7kht/download',
#         # 'reset_p09.csv' = 'https://osf.io/k29v3/download',
#         # 'reset_p10.csv' = 'https://osf.io/vb83k/download',
#         # 'reset_p11.csv' = 'https://osf.io/9rj8s/download',
#         # 'reset_p12.csv' = 'https://osf.io/4n36j/download',
#         # 'reset_p13.csv' = 'https://osf.io/vs6bw/download'
#         
#       )
#       
#       downloadFiles(files=resources, folder=folder, overwrite=overwrite)
#       
#       next()
#       
#     }
#     
#     cat(sprintf('Not a recognized experiment: %s\n',experiment))
#     
#   }
#   
# }
# 
# 
# downloadFiles <- function(files, folder='./', overwrite=TRUE) {
#   
#   for (filename in names(files)) {
#     
#     filepath <- sprintf('%s/%s',folder,filename)
#     
#     if (overwrite | !file.exists(filepath)) {
#       
#       url = as.character(files[filename])
#       
#       cat(sprintf("Downloading: '%s' from '%s'\n", filename, url))
#       
#       download.file(url = url, 
#                     destfile = filepath, 
#                     method = 'auto', 
#                     quiet = FALSE, 
#                     mode = "wb")
#       
#       
#     } else {
#       
#       cat(sprintf("File exists, not downloading: '%s'\n", filepath))
#       
#     }
#     
#   }
#   
# }


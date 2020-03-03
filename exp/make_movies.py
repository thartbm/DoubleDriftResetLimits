from virtual_setup import *
from psychopy import visual
import pandas as pd
from numpy import isnan, diff, median, int16, floor, log10, array
import os


def makeBoundedTrackingMovie(participant, trials, stylus=True, useFfmpeg=True):
  
  # set up the virtual environment
  # as similar to the real experiment as possible
  cfg = {}
  cfg['fullscr'] = False
  cfg['flip'] = False
  
  cfg = createWindow(cfg)
  cfg = createStimuli(cfg)
  
  # load data for participant
  data = pd.read_csv('../data/bounded_tracking/bounded_tracking_p%02d.csv'%participant, header=0)
  
  # loop through trials
  for trial_no in trials:
    
    trial = data[data['trial_no'] == trial_no]
    trial = trial[trial['step'] < 3]
    
    r = 1000/median(diff(trial['time_ms']))
    
    fixationside = trial['fixationside'].iloc[1]
    cfg['point'].pos = [fixationside * (cfg['width'] / 3.), 0]
    
    cfg['instruction'].text = 'fixate here'
    cfg['instruction'].pos = cfg['point'].pos + (0, 80)
    
    nrows = len(trial)
    
    magnitude = int16(floor(log10(nrows))+1)
    
    for rown in range(nrows):
      
      # draw fixation... if step == 1 or 2
      if trial['step'].iloc[rown] > 0:
        cfg['point'].draw()
      # instruction only before the gabor starts moving:
      if trial['step'].iloc[rown] == 1:
        cfg['instruction'].draw()
      
      # draw gabor
      if not(isnan(trial['gabory_pix'].iloc[rown])):
        x = trial['gaborx_pix'].iloc[rown]
        y = trial['gabory_pix'].iloc[rown]
        cfg['gabor'].setPos([x, y])
        cfg['gabor'].ori = trial['gabororientation'].iloc[rown]
        cfg['gabor'].phase = (trial['gaborphase'].iloc[rown], 0.0)
        
        cfg['gabor'].draw()
        
      # draw stylus:
      if stylus:
        x = trial['handx_pix'].iloc[rown]
        y = trial['handy_pix'].iloc[rown]
        cfg['stylus'].setPos([x,y])
        cfg['stylus'].draw()
      
      cfg['win'].flip()
      cfg['win'].getMovieFrame()
      
    fileName = '../doc/movies/BoundedTracking_p%02d_t%03d.mp4'%(participant, trial_no)
    
    if useFfmpeg:
      
      # export each frame:
      cfg['win'].saveMovieFrames('../doc/movies/frame.png', codec='libx264', fps=r, clearFrames=True)
    
      ffcmd = 'ffmpeg -i ../doc/movies/frame%%0%dd.png'%(magnitude)
      # ffcmd = ffcmd + ' -vcodec copy'    # keep input encoding: conflicts libx264, large files
      # ffcmd = ffcmd + ' -b 4M'           # bitrate: conflicts with lossless setting 
      ffcmd = ffcmd + ' -c:v libx264'
      ffcmd = ffcmd + ' -preset veryslow'  # slow, but well-compressed
      ffcmd = ffcmd + ' -r %0.2f'%r        # use the calculated frame rate
      ffcmd = ffcmd + ' -crf 0'            # lossless
      ffcmd = ffcmd + ' ' + fileName
      
      os.system(ffcmd)
      os.system('rm ../doc/movies/frame*.png')
      
    else:
      
      # let pymovie handle it with default settings (looks bad, IMO) 
      cfg['win'].saveMovieFrames(fileName, codec='libx264', fps=r, clearFrames=True)

  
  cfg['win'].close()


def makeBoundedTrackingV2Movie(participant, trials, stylus=True, useFfmpeg=True):
  
  # set up the virtual environment
  # as similar to the real experiment as possible
  cfg = {}
  cfg['fullscr'] = False
  cfg['flip'] = False
  
  cfg = createWindow(cfg)
  cfg = createStimuli(cfg)
  
  # load data for participant
  data = pd.read_csv('../data/bounded_tracking_V2/bounded_tracking_V2_p%02d.csv'%participant, header=0)
  
  # loop through trials
  for trial_no in trials:
    
    trial = data[data['trial_no'] == trial_no]
    trial = trial[trial['step'] < 3]
    
    r = 1000/median(diff(trial['time_ms']))
    
    fixationside = trial['fixationside'].iloc[1]
    cfg['point'].pos = [fixationside * (cfg['width'] / 3.), 0]
    
    cfg['instruction'].text = 'fixate here'
    cfg['instruction'].pos = cfg['point'].pos + (0, 80)
    
    nrows = len(trial)
    
    magnitude = int16(floor(log10(nrows))+1)
    
    for rown in range(nrows):
      
      # draw fixation... if step == 1 or 2
      if trial['step'].iloc[rown] > 0:
        cfg['point'].draw()
      # instruction only before the gabor starts moving:
      if trial['step'].iloc[rown] == 1:
        cfg['instruction'].draw()
      
      # draw gabor
      if not(isnan(trial['gabory_pix'].iloc[rown])):
        x = trial['gaborx_pix'].iloc[rown]
        y = trial['gabory_pix'].iloc[rown]
        cfg['gabor'].setPos([x, y])
        cfg['gabor'].ori = trial['gabororientation'].iloc[rown]
        cfg['gabor'].phase = (trial['gaborphase'].iloc[rown], 0.0)
        
        cfg['gabor'].draw()
        
      # draw stylus:
      if stylus:
        x = trial['handx_pix'].iloc[rown]
        y = trial['handy_pix'].iloc[rown]
        cfg['stylus'].setPos([x,y])
        cfg['stylus'].draw()
      
      cfg['win'].flip()
      cfg['win'].getMovieFrame()
      
    
    fileName = '../doc/movies/BoundedTracking_V2_p%02d_t%03d.mp4'%(participant, trial_no)
    
    if useFfmpeg:
      
      # export each frame:
      cfg['win'].saveMovieFrames('../doc/movies/frame.png', codec='libx264', fps=r, clearFrames=True)
    
      ffcmd = 'ffmpeg -i ../doc/movies/frame%%0%dd.png'%(magnitude)
      # ffcmd = ffcmd + ' -vcodec copy'    # keep input encoding: conflicts libx264, large files
      # ffcmd = ffcmd + ' -b 4M'           # bitrate: conflicts with lossless setting 
      ffcmd = ffcmd + ' -c:v libx264'
      ffcmd = ffcmd + ' -preset veryslow'  # slow, but well-compressed
      ffcmd = ffcmd + ' -r %0.2f'%r        # use the calculated frame rate
      ffcmd = ffcmd + ' -crf 0'            # lossless
      ffcmd = ffcmd + ' ' + fileName
      
      os.system(ffcmd)
      os.system('rm ../doc/movies/frame*.png')
      
    else:
      
      # let pymovie handle it with default settings (looks bad, IMO) 
      cfg['win'].saveMovieFrames(fileName, codec='libx264', fps=r, clearFrames=True)


  # reproduce each frame, store in movie
  
  cfg['win'].close()

def makeOnePassV4Movie(participant, trials, stylus=True, useFfmpeg=True):
  
  # set up the virtual environment
  # as similar to the real experiment as possible
  cfg = {}
  cfg['fullscr'] = False
  cfg['flip'] = False
  
  cfg = createWindow(cfg)
  cfg = createStimuli(cfg)
  
  # load data for participant
  data = pd.read_csv('../data/onePass_V4/onepass_V4_p%02d.csv'%participant, header=0)
  
  # loop through trials
  for trial_no in trials:
    
    trial = data[data['trial_no'] == trial_no]
    
    # we get step == 99...
    #trial = trial[trial['step'] < 3]
    
    trial_stim = trial[trial['step']  < 4 ]
    trial_resp = trial[trial['step'] == 99]
    r = 1000/median( list(diff(trial_stim['time_ms'])) + list(diff(trial_resp['time_ms'])) )
    
    fixationside = trial['fixationside'].iloc[1]
    cfg['point'].pos = [fixationside * (cfg['width'] / 3.), 0]
    
    cfg['instruction'].text = 'fixate here'
    cfg['instruction'].pos = cfg['point'].pos + (0, 80)
    
    nrows = len(trial)
    
    magnitude = int16(floor(log10(nrows))+1)
    
    tracex_pix = []
    tracey_pix = []
    
    for rown in range(nrows):
      
      # draw fixation... if step == 1 or 2
      if trial['step'].iloc[rown] > 0:
        cfg['point'].draw()
      # instruction only before the gabor starts moving:
      if trial['step'].iloc[rown] == 1:
        cfg['instruction'].draw()
      
      # draw gabor
      if not(isnan(trial['gabory_pix'].iloc[rown])) and trial['step'].iloc[rown] > 1:
        # and step > 1
        x = trial['gaborx_pix'].iloc[rown]
        y = trial['gabory_pix'].iloc[rown]
        cfg['gabor'].setPos([x, y])
        cfg['gabor'].ori = trial['gabororientation'].iloc[rown]
        cfg['gabor'].phase = (trial['gaborphase'].iloc[rown], 0.0)
        
        cfg['gabor'].draw()
      
      if trial['step'].iloc[rown] == 99:
        #print(rown)
        tracex_pix = tracex_pix + [trial['handx_pix'].iloc[rown]]
        tracey_pix = tracey_pix + [trial['handy_pix'].iloc[rown]]
        
        #vertices = sp.array([tracex_pix,tracey_pix]).T
        vertices = array([tracex_pix,tracey_pix]).T
        trace = visual.ShapeStim(cfg['win'], vertices=vertices, lineColor=(255,0,0), lineColorSpace='rgb255', closeShape=False, lineWidth=3)
        trace.draw()
      
      
      # draw stylus:
      if stylus:
        x = trial['handx_pix'].iloc[rown]
        y = trial['handy_pix'].iloc[rown]
        if not(isnan(x)):
          cfg['stylus'].setPos([x,y])
          cfg['stylus'].draw()
      
      cfg['win'].flip()
      cfg['win'].getMovieFrame()
      

    fileName = '../doc/movies/onePass_V4_p%02d_t%03d.mp4'%(participant, trial_no)
    
    if useFfmpeg:
      
      # export each frame:
      cfg['win'].saveMovieFrames('../doc/movies/frame.png', codec='libx264', fps=r, clearFrames=True)
    
      ffcmd = 'ffmpeg -i ../doc/movies/frame%%0%dd.png'%(magnitude)
      # ffcmd = ffcmd + ' -vcodec copy'    # keep input encoding: conflicts libx264, large files
      # ffcmd = ffcmd + ' -b 4M'           # bitrate: conflicts with lossless setting 
      ffcmd = ffcmd + ' -c:v libx264'
      ffcmd = ffcmd + ' -preset veryslow'  # slow, but well-compressed
      ffcmd = ffcmd + ' -r %0.2f'%r        # use the calculated frame rate
      ffcmd = ffcmd + ' -crf 0'            # lossless
      ffcmd = ffcmd + ' ' + fileName
      
      os.system(ffcmd)
      os.system('rm ../doc/movies/frame*.png')
      
    else:
      
      # let pymovie handle it with default settings (looks bad, IMO) 
      cfg['win'].saveMovieFrames(fileName, codec='libx264', fps=r, clearFrames=True)


  # reproduce each frame, store in movie
  
  cfg['win'].close()

from virtual_setup import *
import pandas as pd
from numpy import isnan


def makeBoundedTrackingMovie(participant, trials, stylus=True):
  
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
    
    trial = data[data['trial_no'] == 1]
    trial = trial[trial['step'] < 3]
    
    fixationside = trial['fixationside'].iloc[1]
    cfg['point'].pos = [fixationside * (cfg['width'] / 3.), 0]
    
    cfg['instruction'].text = 'fixate here'
    cfg['instruction'].pos = cfg['point'].pos + (0, 80)
    
    nrows = len(trial)
    
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
      x = trial['handx_pix'].iloc[rown]
      y = trial['handy_pix'].iloc[rown]
      cfg['stylus'].setPos([x,y])
      cfg['stylus'].draw()
      
      cfg['win'].flip()
      cfg['win'].getMovieFrame()
      
    fileName = '../doc/movies/BoundedTracking_p%02d_t%03d.mp4'%(participant, trial_no)
    cfg['win'].saveMovieFrames(fileName, codec='libx264', fps=30, clearFrames=True)


  # reproduce each frame, store in movie
  
  cfg['win'].close()

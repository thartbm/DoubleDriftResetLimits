import numpy as np
import scipy as sp
from setup import *

# this function is meant to create a high-resolution gabor to illustrate what we do:

def createExampleGabors():
  
  # use stuff from setup file as much as possible:
  cfg = {}
  cfg['flip'] = False
  cfg['fullscr'] = False
  
  # create fullscreen, upside up window:
  cfg = createWindow(cfg, resolution=[120,120])
  cfg = createStimuli(cfg)
  
  # change some settings to increase the resolution:
  cfg['gabor'].size = 100
  cfg['gabor'].sf = 1.5/100
  
  for phase in np.arange(0,350,15):
    cfg['gabor'].phase = (phase/360., 0.0)
  
    # put on the screen:
    cfg['gabor'].draw()
    cfg['win'].flip()
  
    # grab the current frame, and save it to a file:
    cfg['win'].getMovieFrame()
    cfg['win'].saveMovieFrames('background_gabor_%d.png'%(phase))
  
  # release window:
  cfg['win'].close()

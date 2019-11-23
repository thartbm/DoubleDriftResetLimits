import scipy as sp
from setup import *

# this function is meant to create a high-resolution gabor to illustrate what we do:

def createExampleGabor():
  
  # use stuff from setup file as much as possible:
  cfg = {}
  cfg['flip'] = False
  cfg['fullscr'] = True
  
  # create fullscreen, upside up window:
  cfg = createWindow(cfg)
  cfg = createStimuli(cfg)
  
  # change some settings to increase the resolution:
  cfg['gabor'].size = 1000
  cfg['gabor'].sf = 1.5/1000
  cfg['gabor'].phase = (0.80, 0.0)
  
  # put on the screen:
  cfg['gabor'].draw()
  cfg['win'].flip()
  
  # grab the current frame, and save it to a file:
  cfg['win'].getMovieFrame()
  cfg['win'].saveMovieFrames('gabor1000.png')
  
  # release window:
  cfg['win'].close()

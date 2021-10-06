# do imports:

import numpy as np
import scipy as sp
import pandas as pd

#import pygame
#from pygame.locals import *

import psychopy
from psychopy import visual, core, data, event, logging, sound, gui

import time

import random
from random import seed
#import os
#from Xlib import display # to use mouse position

from ctypes import *



def createWindow(cfg, resolution=[1680, 1050]):
  
  viewScale = [1, 1]
  if cfg['flip']:
    viewScale = [1, -1]

  win = visual.Window(resolution, fullscr=cfg['fullscr'], units='pix', viewScale=viewScale, waitBlanking=True)
  
  
  cfg['win'] = win
  winsize = win.size
  cfg['width'] = winsize[0]
  cfg['height'] = winsize[1]
  cfg['winpos'] = cfg['win'].pos # might be necessary to subtract this from collected mouse coordinates?
  
  return(cfg)

def addMouse(cfg):
  
  try:
    class myMouse:
      Xlib = CDLL("libX11.so.6")
      display = Xlib.XOpenDisplay(None)
      if display == 0: sys.exit(2) # no display or can't be accessed...
      w = Xlib.XRootWindow(display, c_int(cfg['monitorIndex']-1))
      (root_id, child_id) = (c_uint32(), c_uint32())
      (root_x, root_y, win_x, win_y) = (c_int(), c_int(), c_int(), c_int())
      mask = c_uint()

      # this for asking Tk for monitor info, not here:
      #width = root.winfo_screenwidth()
      #height = root.winfo_screenheight()

      def Pos(self):
        #print('X11 mouse')
        ret = self.Xlib.XQueryPointer(self.display, c_uint32(self.w), byref(self.root_id), byref(self.child_id), byref(self.root_x), byref(self.root_y), byref(self.win_x), byref(self.win_y), byref(self.mask))
        if ret == 0: sys.exit(1)
        return [self.root_x.value - (cfg['width']/2), -1 * (self.root_y.value - (cfg['height']/2)), time.time()] # c_int can't be used by regular Python to do math, but the values of c_ints are ints - also, we return the current time
    
  except:
    # Xlib is not available (not on Linux?)
    # use Psychopy:
    class myMouse:

      def Pos(self):
        #print('PsychoPy mouse')
        [X,Y] = mouse.getPos()
        return [X,Y,time.time()]

  cfg['mouse'] = myMouse()
  
  return(cfg)

def createStimuli(cfg):

  # create a cursor stimulus object:
  cfg['cursor'] = visual.Circle(win=cfg['win'], pos = [0,0], radius=15, lineWidth=5)
  cfg['cursor'].setFillColor(color=(95,95,95), colorSpace='rgb255')
  cfg['cursor'].setLineColor(color=(255,255,255), colorSpace='rgb255')
  cfg['cursor'].interpolate = True

  # create a fixation stimulus object:
  cfg['point'] = visual.Circle(win=cfg['win'], pos = [0,0], radius=15, lineWidth=5)
  cfg['point'].setFillColor(color=(255,255,255), colorSpace='rgb255')
  cfg['point'].setLineColor(color=(95,95,95), colorSpace='rgb255')
  cfg['point'].interpolate = True

  # create a text object for instructions:
  cfg['instruction'] = visual.TextStim(cfg['win'], text='instruction', font='', pos=(0, 0), flipVert=cfg['flip'])

  # create a gabor patch object:
  gaborsize = 100
  cfg['gabor'] = visual.GratingStim(
                        
                        win=cfg['win'], 
                        units='pix',
                        tex='sin', 
                        mask='gauss', 
                        size=[gaborsize, gaborsize], 
                        sf=1.5/gaborsize,
                        ori=0.0, 
                          
                        )
  
  # CROSS shape used as target to put the cursor on, among other stuff:
  cfg['cross'] = visual.ShapeStim(win=cfg['win'],
                                pos= [0,0], 
                                vertices=((.2,.2),(1,.2),(1,-.2),(.2,-.2),(.2,-1),(-.2,-1),(-.2,-.2),(-1,-.2),(-1,.2),(-.2,.2),(-.2,1),(.2,1)), 
                                ori=45,
                                size=15,
                                lineWidth=3,
                                units='pix' )
  cfg['cross'].setFillColor(color=(255,0,0), colorSpace='rgb255')
  cfg['cross'].setLineColor(color=(95,0,0), colorSpace='rgb255')
  cfg['cross'].interpolate = True
  
  # LINE shape used to indicate the percept:
  
  cfg['line'] = visual.ShapeStim( win       = cfg['win'],
                                  pos       = [0,0],
                                  vertices  = ((1,0),(-1,0)),   # ((0,0),(-1,0))
                                  ori       = 0,
                                  size      = 75,
                                  lineWidth = 6 )
  cfg['line'].setLineColor(color = (0,0,255), colorSpace='rgb255')
  
  # ARROW shape used to indicate the percept:
  
  cfg['arrow'] = visual.ShapeStim( win       = cfg['win'],
                                   pos       = [0,0],
#                                   vertices  = (-.1,-.1),(1,-.1),(1,-.3),(1.3,0),(1,.3),(1,.1),(-.1,.1)),   # ((0,0),(-1,0))
                                   vertices  = ((0,0),(-1.5,0)),
                                   ori       = 0,
                                   size      = 75,
                                   lineWidth = 6 )
  cfg['arrow'].setLineColor(color = (0,0,255), colorSpace='rgb255')
  
  
  cfg['slider'] = visual.ShapeStim(win            = cfg['win'],
                                   pos            = [0,0],
                                   units          = 'pix',
                                   vertices       = ((0,0),(-1,3),(1,3)),
                                   size           = 15,
                                   lineWidth      = 0,
                                   fillColor      = (0,0,255),
                                   fillColorSpace = 'rgb255' )
  
  
  # new stimulus object for demo movies:
  cfg['stylus'] = visual.ShapeStim(win            = cfg['win'],
                                   pos            = [0,0],
                                   units          = 'pix',
                                   vertices       = ((0,0),(-.5,4./3),(-.5,8),(.5,8),(.5,4./3)),
                                   size           = 30,
                                   ori            = 30,
                                   opacity        = 0.5,
                                   lineWidth      = 6 )
  cfg['stylus'].setLineColor(color = (255,0,255), colorSpace='rgb255')
  
  
  
  class ruler:

    def __init__(self, cfg, nticks=40):
      
      # save this stuff in the ruler object:
      self.nticks = nticks
      self.cfg = cfg
      
      # create ruler tick stuff:
      self.x = np.linspace(-(cfg['height'] / 2), (cfg['height'] / 2), nticks)
      self.y1 = (cfg['height']/4) - (cfg['height']/50)
      self.y2 = (cfg['height']/4) + (cfg['height']/50)
      
      # create all the lines:
      self.ticklines = []
      for tickn in range(nticks):
        self.ticklines.append(visual.Line(cfg['win'],start=(self.x[tickn],self.y1),end=(self.x[tickn],self.y2),lineWidth=4,lineColor=(0,0,0), lineColorSpace='rgb255'))
      
    
    def draw(self):
      for tickline in self.ticklines:
        tickline.draw()
  
  cfg['ruler'] = ruler(cfg)

  # make a grid for retracing trials?
  
  return(cfg)


def getParticipantID(cfg):
  
  validID = False
  while not(validID):
    print (validID)
    str_id = raw_input('participant ID (integer): ')
    
    int_id = int(str_id)

    if '%d'%int_id == str_id:
      validID = True
    

  cfg['id'] = int_id
  
  return(cfg)

def foldout(a):
  # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/
  
  r=[[]]
  for x in a:
    r = [ i + [y] for y in x for i in r ]

  return(pd.DataFrame(r))

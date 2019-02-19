import time
import scipy as sp
import pandas as pd

from psychopy import visual, core, data, event, logging, sound, gui

from pyglet.window import key

def onePassTrial(cfg):
  
  # used to read out keyboard status:
  keyboard = key.KeyStateHandler()
  cfg['win'].winHandle.push_handlers(keyboard)
  
  # getting trial parameters:
  trialno = cfg['trial']
  
  # stimulus parameters:
  internalMovement  = cfg['trialdefinitions']['internalMovement'][trialno]
  externalMovement  = cfg['trialdefinitions']['externalMovement'][trialno]
  
  # psychophysics:
  recordTrace       = cfg['trialdefinitions']['recordTrace'][trialno]
  recordPercept     = cfg['trialdefinitions']['recordPercept'][trialno]
  perceptTask       = cfg['trialdefinitions']['perceptTask'][trialno]
  arrowPosition     = cfg['trialdefinitions']['arrowPosition'][trialno]
  traceTime         = cfg['trialdefinitions']['traceTime'][trialno]
  
  #TDrulerPercept['recordTrace']                = False
  #TDrulerPercept['recordPercept']              = True
  #TDrulerPercept['perceptTask']                = 'ruler'
  #TDrulerPercept['arrowPosition']              = None
  #TDrulerPercept['traceTime']                  = None
  
  
  trialduration = (0.5 / externalMovement) # trial duration will always be exactly enough for a single pass
  
  # set location of fixation point:
  cfg['point'].pos = [cfg['trialdefinitions']['fixationSide'][trialno] * (cfg['width'] / 3.), 0]
  
  # we need a few vectors to collect data in:
  handx_pix = []
  handy_pix = []
  time_s = []
  gaborx_pix = []
  gabory_pix = []
  gaborphase = []
  gabororientation = []
  step = []
  
  # random phase:
  gaborPhaseOffset = sp.rand()
  
  
  # first the mouse / pen cursor has to be brought to the starting position (to counter all that spatial drift)
  trialstarttime = time.time()
  mousepos = cfg['mouse'].Pos()
  
  if (recordTrace == 1): # wait until the pen is at the start position
    
    cfg['cross'].pos = [0,-(cfg['height'] / 4)] # start position always at the bottom!
    
    while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
      cfg['cross'].draw()
      cfg['cursor'].pos = mousepos[:2]
      cfg['cursor'].draw()
      
      cfg['win'].flip()
      #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
      #cfg['frameno'] = cfg['frameno'] + 1
      #cfg['win'].getMovieFrame(buffer='front')

      
      handx_pix.append(mousepos[0])
      handy_pix.append(mousepos[1])
      time_s.append(mousepos[2] - trialstarttime)
      gaborx_pix.append(sp.NaN)
      gabory_pix.append(sp.NaN)
      gaborphase.append(sp.NaN)
      gabororientation.append(sp.NaN)
      step.append(0)
      
      mousepos = cfg['mouse'].Pos()
  
  # then the gabor appears, and stays at the starting position with no internal movement
  # currently set to 1.5 seconds, enough to start fixating?
  
  cfg['instruction'].text = 'fixate here'
  cfg['instruction'].pos = cfg['point'].pos + (0, 80)

  starttime = time.time()
  
  while (time.time() - starttime < 1.5):
    
    cfg['gabor'].phase = (gaborPhaseOffset, 0.0)
    cfg['gabor'].setPos([0,-(cfg['height'] / 4)])
    
    cfg['point'].draw()
    cfg['instruction'].draw()
    cfg['gabor'].draw()
    
    cfg['win'].flip()
    #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
    #cfg['frameno'] = cfg['frameno'] + 1
    #cfg['win'].getMovieFrame(buffer='front')
    
    if recordTrace & (traceTime == 'online'):
      handx_pix.append(mousepos[0])
      handy_pix.append(mousepos[1])
    else:
      handx_pix.append(sp.NaN)
      handy_pix.append(sp.NaN)

    time_s.append(mousepos[2] - trialstarttime)
    gaborx_pix.append(0)
    gabory_pix.append(0)
    gaborphase.append(cfg['gabor'].phase[0])
    gabororientation.append(cfg['gabor'].ori)
    step.append(1)
    
    mousepos = cfg['mouse'].Pos()
  
  # when the gabor starts moving, the trial duration starts
  
  starttime = time.time()
  
  while (time.time() - starttime < trialduration):
    
    elapsed = time.time() - starttime
    
    cfg['gabor'].phase = ((elapsed * internalMovement) + gaborPhaseOffset, 0.0)
    
    # Y = (cfg['height'] / 4) * (1 - (abs((((elapsed-trialduration)*(externalMovement/0.5))%2)) * 2))

    Y = (1 - (abs((((elapsed)*(externalMovement/0.5))%2) - 1) * 2)) * (cfg['height'] / 4)
    
    cfg['gabor'].setPos([0,Y])
    
    cfg['point'].draw()
    cfg['gabor'].draw()
    
    cfg['win'].flip()
    #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
    #cfg['frameno'] = cfg['frameno'] + 1
    #cfg['win'].getMovieFrame(buffer='front')
    
    if recordTrace & (traceTime == 'online'):
      handx_pix.append(mousepos[0])
      handy_pix.append(mousepos[1])
    else:
      handx_pix.append(sp.NaN)
      handy_pix.append(sp.NaN)

    time_s.append(mousepos[2] - trialstarttime)
    gaborx_pix.append(0)
    gabory_pix.append(Y)
    gaborphase.append(cfg['gabor'].phase[0])
    gabororientation.append(cfg['gabor'].ori)
    step.append(2)
    
    mousepos = cfg['mouse'].Pos()
  
  # just for good measure, we have a blank screen at the end of the trial
  # this ensures people stop moving... so, only for recordTrace trials
  
  if recordTrace:
    
    starttime = time.time()
    
    while (time.time() - starttime < 0.75):
      
      cfg['point'].draw()
      cfg['win'].flip()
      #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
      #cfg['frameno'] = cfg['frameno'] + 1
      #cfg['win'].getMovieFrame(buffer='front')
    
      if recordTrace & (traceTime == 'online'):
        handx_pix.append(mousepos[0])
        handy_pix.append(mousepos[1])
      else:
        handx_pix.append(sp.NaN)
        handy_pix.append(sp.NaN)
      
      time_s.append(mousepos[2] - trialstarttime)
      gaborx_pix.append(sp.NaN)
      gabory_pix.append(sp.NaN)
      gaborphase.append(sp.NaN)
      gabororientation.append(sp.NaN)
      step.append(3)
  
  # now record the percept if applicable:
  
  percept = sp.NaN
  if recordPercept & (perceptTask == 'arrow'):

    lineAngle = cfg['trialdefinitions']['perceptAngle'][trialno]
    cfg['line'].ori = lineAngle
    if (arrowPosition == 'point'):
      cfg['line'].pos = cfg['point'].pos
    if (arrowPosition == 'start'):
      cfg['line'].pos = [0,-(cfg['height'] / 4)]
    
    event.clearEvents()
    
    perceptRecorded = False
    starttime = time.time()
    
    while not perceptRecorded:
      
      # read keyboard status
      keysPressed = event.getKeys(keyList=['return'])
      if 'return' in keysPressed:
        percept = cfg['line'].ori
        perceptRecorded = True
        event.clearEvents()
      
      # see the left / right keys continuously... have to use pyglet stuff
      
      if keyboard[key.LEFT]:
        cfg['line'].ori = cfg['line'].ori - 1
        event.clearEvents()
        if cfg['line'].ori < 0:
          cfg['line'].ori = 0
      if keyboard[key.RIGHT]:
        cfg['line'].ori = cfg['line'].ori + 1
        event.clearEvents()
        if cfg['line'].ori > 180:
          cfg['line'].ori = 180

      
      ## if enter: end while loop and record stuff
      #if len(keysPressed): print(keysPressed)
      ## if +/- adapt angle
      #if 'left' in keysPressed:
      #  cfg['line'].ori = cfg['line'].ori - 1
      #  event.clearEvents()
      #  if cfg['line'].ori < 0:
      #    cfg['line'].ori = 0
      #if 'right' in keysPressed:
      #  cfg['line'].ori = cfg['line'].ori + 1
      #  event.clearEvents()
      #  if cfg['line'].ori > 180:
      #    cfg['line'].ori = 180
      
      
            
      # draw stuff on screen
      cfg['line'].draw()
      cfg['win'].flip()
      #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
      #cfg['frameno'] = cfg['frameno'] + 1
      #cfg['win'].getMovieFrame(buffer='front')
      
  if recordPercept & (perceptTask == 'ruler'):
    
    # move the cross from left to right?
    cfg['cross'].pos = [0, (cfg['height'] / 4)]
    
    event.clearEvents()
    
    perceptRecorded = False
    starttime = time.time() # RT not recorded?
    
    while not perceptRecorded:
      
      # read keyboard status
      keysPressed = event.getKeys(keyList=['return'])
      if 'return' in keysPressed:
        Y = cfg['cross'].pos[1] + (cfg['height'] / 4)
        X = cfg['cross']
        perceptangle = (np.arctan2(Y,X) / np.pi) * 180
        percept = perceptangle
        perceptRecorded = True
        event.clearEvents()
      
      # see the left / right keys continuously... have to use pyglet stuff
      
      if keyboard[key.LEFT]:
        cfg['cross'].pos[0] = cfg['cross'].pos[0] - 1
        event.clearEvents()
        if cfg['line'].ori < (cfg['width'] / -2):
          cfg['line'].ori = (cfg['width'] / -2)
      if keyboard[key.RIGHT]:
        cfg['cross'].pos[0] = cfg['cross'].pos[0] + 1
        event.clearEvents()
        if cfg['line'].ori > (cfg['width'] / 2):
          cfg['line'].ori = (cfg['width'] / 2):      
      
            
      # draw stuff on screen
      cfg['line'].draw()
      cfg['win'].flip()
    
    
  # finalize stuff
  
  nsamples = len(handx_pix)

  trial_data = pd.DataFrame(
    {
     'trial_no'          : [trialno + 1] * nsamples,
     'fixationside'      : [cfg['trialdefinitions']['fixationSide'][trialno]] * nsamples,
     'internalMovement'  : [internalMovement] * nsamples,
     'externalMovement' : [externalMovement] * nsamples,
     'step'              : step,
     'time_ms'           : [t * 1000 for t in time_s],
     'gaborx_pix'        : gaborx_pix,
     'gabory_pix'        : gabory_pix,
     'gaborphase'        : gaborphase,
     'gabororientation'  : gabororientation,
     'handx_pix'         : handx_pix,
     'handy_pix'         : handy_pix,
     'percept'           : [percept] * nsamples
    })

  trial_data = trial_data[['trial_no', 'fixationside', 'internalMovement', 'externalMovement', 'step', 'time_ms', 'gaborx_pix', 'gabory_pix', 'gaborphase', 'gabororientation', 'handx_pix', 'handy_pix', 'percept']]
  
  trial_data.to_csv('../data/onepass/trials/onepass_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index=False, float_format='%0.3f')
  
  return(cfg)

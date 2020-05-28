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
  taskname          = cfg['trialdefinitions']['taskname'][trialno]
  
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
  
  if recordTrace & (traceTime == 'online'):
    
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
    cfg['arrow'].ori = lineAngle
    if (arrowPosition == 'point'):
      cfg['arrow'].pos = cfg['point'].pos
    if (arrowPosition == 'start'):
      cfg['arrow'].pos = [0,-(cfg['height'] / 4)]
    
    event.clearEvents()
    
    perceptRecorded = False
    starttime = time.time()
    
    while not perceptRecorded:
      
      # read keyboard status
      keysPressed = event.getKeys(keyList=['return'])
      if 'return' in keysPressed:
        percept = 90 - (cfg['arrow'].ori - 90)
        perceptRecorded = True
        event.clearEvents()
      
      # see the left / right keys continuously... have to use pyglet stuff
      
      if keyboard[key.LEFT]:
        cfg['arrow'].ori = cfg['arrow'].ori - 1
        event.clearEvents()
        if cfg['arrow'].ori < 0:
          cfg['arrow'].ori = 0
      if keyboard[key.RIGHT]:
        cfg['arrow'].ori = cfg['arrow'].ori + 1
        event.clearEvents()
        if cfg['arrow'].ori > 180:
          cfg['arrow'].ori = 180
      
      # draw stuff on screen
      cfg['arrow'].draw()
      cfg['win'].flip()
      #cfg['win']._getFrame().save('../frames/onepass%06d.png'%cfg['frameno'])
      #cfg['frameno'] = cfg['frameno'] + 1
      #cfg['win'].getMovieFrame(buffer='front')
      
  if recordPercept & (perceptTask == 'ruler'):
    
    pausestarttime = time.time()
    
    while (time.time() - pausestarttime < 1):
      cfg['point'].draw()
      cfg['win'].flip()

    # move the cross from left to right?
    sliderpos = [0, (cfg['height'] / 4)]

    event.clearEvents()
    
    perceptRecorded = False
    #starttime = time.time() # RT not recorded?
    
    while not perceptRecorded:
      
      # read keyboard status
      keysPressed = event.getKeys(keyList=['return'])
      if 'return' in keysPressed:
        Y = cfg['slider'].pos[1] + (cfg['height'] / 4)
        X = cfg['slider'].pos[0]
        percept = (sp.arctan2(Y,X) / sp.pi) * 180
        perceptRecorded = True
        event.clearEvents()
      
      # see the left / right keys continuously... have to use pyglet stuff
      
      # make step size the log of the number of seconds (+1) since the key was started to be pushed down?
      if keyboard[key.LEFT]:
        sliderpos[0] = (sliderpos[0] - 5)
        event.clearEvents()
        if sliderpos[0] < (-1 * (cfg['width'] / 2)):
          sliderpos[0] = (cfg['width'] / 2)
      if keyboard[key.RIGHT]:
        sliderpos[0] = (sliderpos[0] + 5)
        event.clearEvents()
        if sliderpos[0] > (cfg['width'] / 2):
          sliderpos[0] = (cfg['width'] / 2)      
      
      cfg['slider'].pos = sliderpos
      #print(cfg['slider'].pos) 
      # draw stuff on screen
      cfg['ruler'].draw()
      cfg['slider'].draw()
      cfg['win'].flip()
  
  
  # delayed tracing:
  
  
  
  #trialstarttime = time.time()
  mousepos = cfg['mouse'].Pos()
  
  if recordTrace & (traceTime == 'delayed'):
    
    traceRecorded = False
    
    while not traceRecorded:
    
      # first the mouse / pen cursor has to be brought to the starting position (to counter all that spatial drift)
    
      cfg['cross'].pos = [0,-(cfg['height'] / 4)] # start position always at the bottom!
    
      while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
        cfg['cross'].draw()
        cfg['cursor'].pos = mousepos[:2]
        cfg['cursor'].draw()
        
        cfg['win'].flip()
        
        # do we record this?
        #handx_pix.append(mousepos[0])
        #handy_pix.append(mousepos[1])
        #time_s.append(mousepos[2] - trialstarttime)
        #gaborx_pix.append(sp.NaN)
        #gabory_pix.append(sp.NaN)
        #gaborphase.append(sp.NaN)
        #gabororientation.append(sp.NaN)
        #step.append(4)
      
        mousepos = cfg['mouse'].Pos()
      
      traceEnded = False
      
      tracex_pix = []
      tracey_pix = []
      tracetime_s = []
      
      while not traceEnded:
        # now record the actual trace...
        mousepos = cfg['mouse'].Pos()
        
        tracex_pix.append(mousepos[0])
        tracey_pix.append(mousepos[1])
        tracetime_s.append(mousepos[2] - trialstarttime)
        
        # draw the trace
        vertices = sp.array([tracex_pix,tracey_pix]).T
        trace = visual.ShapeStim(cfg['win'], vertices=vertices, lineColor=(255,0,0), lineColorSpace='rgb255', closeShape=False, lineWidth=3)
        trace.draw()
        cfg['win'].flip()

        keysPressed = event.getKeys(keyList=['return', 'delete'])
        if 'return' in keysPressed:
          # put the trace in the file?
          # determine number of samples in recorded trace:
          trace_samples = len(tracex_pix)
          empty = [sp.NaN] * trace_samples
          # these were collected:
          handx_pix = handx_pix + tracex_pix
          handy_pix = handy_pix + tracey_pix
          time_s = time_s + tracetime_s
          # these have to be filled with default/empty values
          gaborx_pix = gaborx_pix + empty
          gabory_pix = gabory_pix + empty
          gaborphase = gaborphase + empty
          gabororientation = gabororientation + empty
          trace_step = [99] * trace_samples
          step = step + trace_step
          # clear up and end the loops
          event.clearEvents()
          traceEnded = True
          traceRecorded = True
        if 'delete' in keysPressed:
          traceEnded = True
  
  # finalize stuff
  
  nsamples = len(handx_pix)
  
  #print(len(handx_pix), len(handy_pix), len(time_s), len(gaborx_pix), len(step))
  
  trial_data = pd.DataFrame(
    {
     'trial_no'          : [trialno + 1] * nsamples,
     'fixationside'      : [cfg['trialdefinitions']['fixationSide'][trialno]] * nsamples,
     'internalMovement'  : [internalMovement] * nsamples,
     'externalMovement'  : [externalMovement] * nsamples,
     'step'              : step,
     'time_ms'           : [t * 1000 for t in time_s],
     'gaborx_pix'        : gaborx_pix,
     'gabory_pix'        : gabory_pix,
     'gaborphase'        : gaborphase,
     'gabororientation'  : gabororientation,
     'handx_pix'         : handx_pix,
     'handy_pix'         : handy_pix,
     'percept'           : [percept] * nsamples,
     'taskname'          : [taskname] * nsamples
    })
  
  trial_data = trial_data[['taskname', 'trial_no', 'fixationside', 'internalMovement', 'externalMovement', 'step', 'time_ms', 'gaborx_pix', 'gabory_pix', 'gaborphase', 'gabororientation', 'handx_pix', 'handy_pix', 'percept']]
  
  trial_data.to_csv('../data/onepass_V4/trials/onepass_V4_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index=False, float_format='%0.3f')
  
  return(cfg)

import time
import scipy as sp
import pandas as pd


def resetTrial(cfg):
  
  # set some defining variables:
  
  trialno = cfg['trial']
  
  internalMovement  = cfg['trialdefinitions']['internalMovement'][trialno]
  resetPausePass    = cfg['trialdefinitions']['resetPausePass'][trialno]
  resetPauseOffset  = cfg['trialdefinitions']['resetPauseOffset'][trialno]
  externalDirection = cfg['trialdefinitions']['externalDirection'][trialno]
  externalSpeed     = cfg['externalSpeed']
  
  cfg['point'].pos = [cfg['trialdefinitions']['fixationSide'][trialno] * (cfg['width'] / 3), 0]

  gaborPhaseOffset = sp.rand()
  
  resetPauseStart = (resetPausePass - 0.5) / externalSpeed
  
  # we need a few vectors to collect data in:
  handx_pix = []
  handy_pix = []
  time_s = []
  gaborx_pix = []
  gabory_pix = []
  gaborphase = []
  gabororientation = []
  step = []
  
  
  # first the mouse / pen cursor has to be brought to the starting position (to counter all that spatial drift)
  
  trialstarttime = time.time()

  mousepos = cfg['mouse'].Pos()
  cfg['cross'].pos = [0,0]
  while sp.sqrt(sum(sp.array(mousepos[:2])**2)) > 10:
    cfg['cross'].draw()
    cfg['cursor'].pos = mousepos[:2]
    cfg['cursor'].draw()
    
    cfg['win'].flip()
    
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
    cfg['gabor'].setPos([0,0])
    
    cfg['point'].draw()
    cfg['instruction'].draw()
    cfg['gabor'].draw()
    
    cfg['win'].flip()
    
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    gaborx_pix.append(0)
    gabory_pix.append(0)
    gaborphase.append(cfg['gabor'].phase[0])
    gabororientation.append(cfg['gabor'].ori)
    step.append(1)
    
    mousepos = cfg['mouse'].Pos()
  
  # when the gabor starts moving, the trial duration starts
  
  starttime = time.time()
  
  while (time.time() - starttime < cfg['trial_duration']):
    
    elapsed = time.time() - starttime

    cfg['gabor'].phase = (gaborPhaseOffset + ((abs( ( (elapsed / (0.5 / externalSpeed)) + 1.5)%2 - 1) - 0.5) * 2 * internalMovement), 0.0)
    
    
    cfg['point'].draw()
    
    if (elapsed < resetPauseStart):
      cfg['gabor'].phase = (gaborPhaseOffset + ((abs( ( (elapsed / (0.5 / externalSpeed)) + 1.5)%2 - 1) - 0.5) * 2 * internalMovement), 0.0)
      Y = (abs( ( (elapsed / (0.5 / cfg['externalSpeed'])) + 1.5)%2 - 1) - 0.5) * 2 * (cfg['height'] / 4) * externalDirection
      cfg['gabor'].setPos([0,Y])
      cfg['gabor'].draw()
    if (elapsed > resetPauseStart + .250):
      cfg['gabor'].phase = (gaborPhaseOffset + ((abs( ( ((elapsed-.25) / (0.5 / externalSpeed)) + 1.5)%2 - 1) - 0.5) * 2 * internalMovement), 0.0)
      Y = (abs( ( ((elapsed-.25) / (0.5 / cfg['externalSpeed'])) + 1.5)%2 - 1) - 0.5) * 2 * (cfg['height'] / 4) * externalDirection
      cfg['gabor'].setPos([0,Y])
      cfg['gabor'].draw()
    
    cfg['win'].flip()
    
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    gaborx_pix.append(0)
    gabory_pix.append(Y)
    gaborphase.append(cfg['gabor'].phase[0])
    gabororientation.append(cfg['gabor'].ori)
    step.append(2)
    
    mousepos = cfg['mouse'].Pos()
  
  # just for good measure, we have a blank screen at the end of the trial
  # this ensures people stop moving
  
  starttime = time.time()
  
  while (time.time() - starttime < 1.5):
    
    cfg['point'].draw()
    cfg['win'].flip()
  
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    gaborx_pix.append(sp.NaN)
    gabory_pix.append(sp.NaN)
    gaborphase.append(sp.NaN)
    gabororientation.append(sp.NaN)
    step.append(3)
  
  # finalize stuff
  
  nsamples = len(handx_pix)

  trial_data = pd.DataFrame(
    {
     'trial_no'          : [trialno + 1] * nsamples,
     'fixationside'      : [cfg['trialdefinitions']['fixationSide'][trialno]] * nsamples,
     'internalSpeed'     : [internalMovement] * nsamples,
     'externalDirection' : [externalDirection] * nsamples,
     'step'              : step,
     'time_ms'           : [t * 1000 for t in time_s],
     'gaborx_pix'        : gaborx_pix,
     'gabory_pix'        : gabory_pix,
     'gaborphase'        : gaborphase,
     'gabororientation'  : gabororientation,
     'handx_pix'         : handx_pix,
     'handy_pix'         : handy_pix
    })

  trial_data = trial_data[['trial_no', 'fixationside', 'internalSpeed', 'externalDirection', 'step', 'time_ms', 'gaborx_pix', 'gabory_pix', 'gaborphase', 'gabororientation', 'handx_pix', 'handy_pix']]
  
  trial_data.to_csv('../data/reset/trials/reset_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index=False, float_format='%0.3f')
  
  return(cfg)

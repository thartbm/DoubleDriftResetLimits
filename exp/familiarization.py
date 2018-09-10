import scipy as sp
import pandas as pd
import random
import time
from setup import foldout

def targetTrial(cfg):
  
  trialno = cfg['trial']
  
  d = cfg['height'] / 4.
  h = cfg['height'] / 8.
  
  
  #targets = [(d,d),(d,-d),(-d,d),(-d,-d),(d,0),(-d,0),(0,d),(0,-d)]
  coords = [d,-d,h,-h,0]
  #targets = pd.concat([foldout([coords, [d,-d]]), foldout([[d,-d], coords])]).values.tolist()
  targets = foldout([coords, coords]).values.tolist()
  targets = targets[0:(len(targets)-1)]
  random.shuffle(targets)
    
  # we need a few vectors to collect data in:
  handx_pix = []
  handy_pix = []
  time_s = []
  targetx_pix = []
  targety_pix = []
  step = []
  
  
  # first the mouse / pen cursor has to be brought to the starting position (to counter all that spatial drift)
  
  trialstarttime = time.time()
  
  cfg['cross'].pos = [0,0]
  
  mousepos = cfg['mouse'].Pos()
  while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
    cfg['cross'].draw()
    cfg['cursor'].pos = mousepos[:2]
    cfg['cursor'].draw()
      
    cfg['win'].flip()
    # cfg['win'].getMovieFrame(buffer='back')
    # cfg['win']._getFrame().save('../frames/familiarization%06d.png'%cfg['frameno'])
    # cfg['frameno'] = cfg['frameno'] + 1

      
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    targetx_pix.append(0)
    targety_pix.append(0)
    step.append(0)
    
    mousepos = cfg['mouse'].Pos()  
  
  for target_idx in range(len(targets)):
    cfg['cross'].pos = targets[target_idx]
  
    while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
      cfg['cross'].draw()
      cfg['cursor'].pos = mousepos[:2]
      cfg['cursor'].draw()
      
      cfg['win'].flip()
      # cfg['win'].getMovieFrame(buffer='back')
      # cfg['win']._getFrame().save('../frames/familiarization%06d.png'%cfg['frameno'])
      # cfg['frameno'] = cfg['frameno'] + 1

      
      handx_pix.append(mousepos[0])
      handy_pix.append(mousepos[1])
      time_s.append(mousepos[2] - trialstarttime)
      targetx_pix.append(targets[target_idx][0])
      targety_pix.append(targets[target_idx][1])
      step.append(target_idx+1)
      
      mousepos = cfg['mouse'].Pos()  
  
  cfg['cross'].pos = [0,0]
  
  while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
    cfg['cross'].draw()
    cfg['cursor'].pos = mousepos[:2]
    cfg['cursor'].draw()
      
    cfg['win'].flip()
    # cfg['win'].getMovieFrame(buffer='back')
    # cfg['win']._getFrame().save('../frames/familiarization%06d.png'%cfg['frameno'])
    # cfg['frameno'] = cfg['frameno'] + 1
     
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    targetx_pix.append(0)
    targety_pix.append(0)
    step.append(len(targets))
    
    mousepos = cfg['mouse'].Pos()  
  
  nsamples = len(handx_pix)

  trial_data = pd.DataFrame(
    {
     'trial_no'          : [trialno + 1] * nsamples,
     'step'              : step,
     'time_ms'           : [t * 1000 for t in time_s],
     'targetx_pix'       : targetx_pix,
     'targety_pix'       : targety_pix,
     'handx_pix'         : handx_pix,
     'handy_pix'         : handy_pix
    })

  trial_data = trial_data[['trial_no', 'step', 'time_ms', 'targetx_pix', 'targety_pix', 'handx_pix', 'handy_pix']]
  
  trial_data.to_csv('../data/familiarization/trials/target_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index=False, float_format='%0.3f')
  
  return(cfg)

def trackingTrial(cfg):
  
  passduration = 3
  
  trialno = cfg['trial']
  
  d = cfg['height'] / 4.
  h = cfg['height'] / 8.
  
  
  #targets = [(d,d),(d,-d),(-d,d),(-d,-d),(d,0),(-d,0),(0,d),(0,-d)]
  coords = [d,-d,h,-h,0]
  #targets = pd.concat([foldout([coords, [d,-d]]), foldout([[d,-d], coords])]).values.tolist()
  targets = foldout([coords, coords]).values.tolist()
  targets = targets[0:(len(targets)-1)]
  random.shuffle(targets)
    
  # we need a few vectors to collect data in:
  handx_pix = []
  handy_pix = []
  time_s = []
  targetx_pix = []
  targety_pix = []
  step = []
  
  
  # first the mouse / pen cursor has to be brought to the starting position (to counter all that spatial drift)
  
  trialstarttime = time.time()
  
  cfg['cross'].pos = [0,0]
  
  mousepos = cfg['mouse'].Pos()
  while sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) > 10:
    cfg['cross'].draw()
    cfg['cursor'].pos = mousepos[:2]
    cfg['cursor'].draw()
      
    cfg['win'].flip()
    # cfg['win'].getMovieFrame(buffer='back')
    # cfg['win']._getFrame().save('../frames/familiarization%06d.png'%cfg['frameno'])
    # cfg['frameno'] = cfg['frameno'] + 1
      
    handx_pix.append(mousepos[0])
    handy_pix.append(mousepos[1])
    time_s.append(mousepos[2] - trialstarttime)
    targetx_pix.append(0)
    targety_pix.append(0)
    step.append(0)
    
    mousepos = cfg['mouse'].Pos()  
  
  startpos = [0,0]
  
  for target_idx in range(len(targets)):
    
    targetpos = targets[target_idx]
    #print targetpos
    targetstarttime = time.time()
    
    #print targetstarttime
    #print targetstarttime + passduration
    #print ((targetstarttime + passduration) > time.time())

    while ((targetstarttime + passduration) > time.time()):
            
      crossposX = startpos[0] + (((mousepos[2] - targetstarttime) / passduration) * (targetpos[0] - startpos[0]))
      crossposY = startpos[1] + (((mousepos[2] - targetstarttime) / passduration) * (targetpos[1] - startpos[1]))
      
      cfg['cross'].pos = [crossposX, crossposY]
      
      if sp.sqrt(sum(sp.array([mousepos[0]-cfg['cross'].pos[0], mousepos[1]-cfg['cross'].pos[1]])**2)) < 20:
        cfg['cursor'].setLineColor(color=(0,255,0), colorSpace='rgb255')
      else:
        cfg['cursor'].setLineColor(color=(255,0,0), colorSpace='rgb255')
      cfg['cross'].draw()
      cfg['cursor'].pos = mousepos[:2]
      cfg['cursor'].draw()
      
      cfg['win'].flip()
      # cfg['win'].getMovieFrame(buffer='back')
      # cfg['win']._getFrame().save('../frames/familiarization%06d.png'%cfg['frameno'])
      # cfg['frameno'] = cfg['frameno'] + 1
      
      
      handx_pix.append(mousepos[0])
      handy_pix.append(mousepos[1])
      time_s.append(mousepos[2] - trialstarttime)
      targetx_pix.append(crossposX)
      targety_pix.append(crossposY)
      step.append(target_idx+1)
      
      mousepos = cfg['mouse'].Pos()
    
    startpos = targetpos
  
  cfg['cursor'].setLineColor(color=(255,255,255), colorSpace='rgb255')

  nsamples = len(handx_pix)

  trial_data = pd.DataFrame(
    {
     'trial_no'          : [trialno + 1] * nsamples,
     'step'              : step,
     'time_ms'           : [t * 1000 for t in time_s],
     'targetx_pix'       : targetx_pix,
     'targety_pix'       : targety_pix,
     'handx_pix'         : handx_pix,
     'handy_pix'         : handy_pix
    })

  trial_data = trial_data[['trial_no', 'step', 'time_ms', 'targetx_pix', 'targety_pix', 'handx_pix', 'handy_pix']]
  
  trial_data.to_csv('../data/familiarization/trials/tracking_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index=False, float_format='%0.3f')
  
  return(cfg)


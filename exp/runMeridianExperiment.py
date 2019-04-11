from setup import *
from meridian import *
import pandas as pd

# start building a config dictionary for the experiment:
cfg = {}

# get participant number and seed random number generator based on that:
cfg = getParticipantID(cfg)
seed(sum([ord(c) for c in 'illusory motion reset tracking']) + (cfg['id'] * 9999))


# add a window for visual stimuli:
cfg['fullscr']      = True
cfg['monitorIndex'] = 1
cfg['flip']         = True

#cfg = createWindow(cfg, resolution=[1920, 1080])
cfg = createWindow(cfg, resolution=[1680, 1050])

mouse = event.Mouse(win=cfg['win'])
mouse.setVisible(False)

# add a 'mouse' object for pen positions,
# use XLib if possible as it is more accurate:

# this creates a mouse object in the cfg dictionary
# it has a single method: Pos()
# that returns vector with an X and Y coordinate, and a time stamp
# it either uses X11, and if that fails it uses PsychoPy
cfg = addMouse(cfg)

cfg['frameno'] = 1

# create some psychopy objects that will be used as stimuli:
cfg = createStimuli(cfg)

cfg['externalSpeed']                    = .125 # Hz: how often can the cursor go up and down the whole range (= 2 passes) in a second
# these two should not be used:
Npasses                                 = 1
cfg['trial_duration']                   = (0.5 / cfg['externalSpeed']) * (Npasses + .5)


# there will be 4 starting positions with 4 external movement paths and 4 orientations of the gabor
# these are combined with two internal motion directions
# and two (or more?) fixation points that are *below* the stimulus area

startX                                  = [1, -1]
startY                                  = [1, -1]
internalMotion                          = [-3, 3]
fixationOffset                          = [-1./3, 1./3]
repetitions                             = 2

# create all desired combinations:
TD                                      = pd.concat([foldout(a=[startX,startY, internalMotion, fixationOffset])]*repetitions, ignore_index=True)
TD.columns                              = ['startX', 'startY', 'internalMotion', 'fixationOffset']

TDmeridian = TD

blockrepeats = 4

#instructiontrials = [0,TD.shape[0]]
instructiontrials = [task * TD.shape[0] for task in range(blockrepeats)]

instructiontexts = ['retrace the patch after seeing it'] * blockrepeats

for br in range(blockrepeats):
  
  # randomize:
  TDmeridian_1  = TDmeridian.sample(frac=1).reset_index(drop=True)
    
  TDmeridian_1['recordTrace']                = True
  TDmeridian_1['recordPercept']              = False
  TDmeridian_1['perceptTask']                = None
  TDmeridian_1['arrowPosition']              = None
  TDmeridian_1['traceTime']                  = 'delayed'
  TDmeridian_1['taskname']                   = 're-trace'
    
  if ('trialdefinitions' in cfg.keys()):
    # join tables for both trial types:
    cfg['trialdefinitions']                 = pd.concat([cfg['trialdefinitions'],TDmeridian_1], ignore_index=True)
  else:
    cfg['trialdefinitions']                 = TDmeridian_1


Ntrials = cfg['trialdefinitions'].shape[0]
#print cfg['trialdefinitions']

cfg['trialdefinitions']['fixationSide'] = [random.choice([-1,1]) for _ in range(Ntrials)]

doTrials = range(Ntrials)
#doTrials = range(54,72)

for trialno in doTrials:
  
  if trialno in instructiontrials:
    cfg['instruction'].text = instructiontexts[instructiontrials.index(trialno)]
    cfg['instruction'].pos = (0,0)
    cfg['instruction'].draw()
    cfg['win'].flip()
    event.waitKeys(keyList=['space'])
  
  print 'trial %d of %d'%(trialno+1, Ntrials)
  
  cfg['trial'] = trialno
  
  cfg = meridianTrial(cfg)
  

# after the trials have been done, we concatenate the trials into one big file:

cfg['instruction'].text = 'creating data file'
cfg['instruction'].pos = (0,0)
cfg['instruction'].draw()
cfg['win'].flip()

participant_data = 0

for trialno in doTrials:
  
  trial_data = pd.DataFrame.from_csv('../data/meridian/trials/meridian_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index_col=None)
  
  if isinstance(participant_data, pd.DataFrame):
    participant_data = pd.concat([participant_data, trial_data])
  else:
    participant_data = trial_data
  
participant_data.to_csv('../data/meridian/meridian_p%02d.csv'%(cfg['id']), index=False, float_format='%0.3f')

#cfg['instruction'].text = 'creating movie'
#cfg['instruction'].pos = (0,0)
#cfg['instruction'].draw()
#cfg['win'].flip()
#cfg['win'].saveMovieFrames('../frames/p%02d.mp4'%cfg['id'], codec='libx264', fps=60, clearFrames=True)

# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


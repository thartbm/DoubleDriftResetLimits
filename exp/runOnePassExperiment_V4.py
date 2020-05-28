from setup import *
from onepass_V4 import *
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

cfg = createWindow(cfg, resolution=[1920, 1080])

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


# there will be 6 internal cursor movements, that induce various degrees of illusion:
internalMovement                        = [-4, -2, -1, 1, 2, 4]
externalMovement                        = [1./8, 1./4, 1./2]
perceptAngle                            = [89,91]
repetitions                             = 2

internalMovement                        = [-4,-3,-2, 2,3,4]
externalMovement                        = [1./8, 1./6]
perceptAngle                            = [90]
repetitions                             = 3

# create all desired combinations:
TD                                      = pd.concat([foldout(a=[internalMovement,externalMovement, perceptAngle])]*repetitions, ignore_index=True)
TD.columns                              = ['internalMovement', 'externalMovement', 'perceptAngle']

# copy for different trial types:
TDarrowPercept                          = TD
TDtraceDelayed                          = TD


blockrepeats = 4

#instructiontrials = [0,TD.shape[0]]
instructiontrials = [task * TD.shape[0] for task in range(2*blockrepeats)]

instructions = ['report initial movement direction of the patch','retrace the patch after seeing it']

instructiontexts = []

for br in range(blockrepeats):
  
  # randomize:
  TDarrowPercept_1  = TDarrowPercept.sample(frac=1).reset_index(drop=True)
  TDtraceDelayed_1  = TDtraceDelayed.sample(frac=1).reset_index(drop=True)
  
  TDarrowPercept_1['recordTrace']                = False
  TDarrowPercept_1['recordPercept']              = True
  TDarrowPercept_1['perceptTask']                = 'arrow'
  TDarrowPercept_1['arrowPosition']              = 'start' # instead of 'fixation'
  TDarrowPercept_1['traceTime']                  = None
  TDarrowPercept_1['taskname']                   = 'arrow'
  
  TDtraceDelayed_1['recordTrace']                = True
  TDtraceDelayed_1['recordPercept']              = False
  TDtraceDelayed_1['perceptTask']                = None
  TDtraceDelayed_1['arrowPosition']              = None
  TDtraceDelayed_1['traceTime']                  = 'delayed'
  TDtraceDelayed_1['taskname']                   = 're-trace'
  
  if (cfg['id'] % 2 == 0):
    TDs                                     = pd.concat([TDarrowPercept_1, TDtraceDelayed_1], ignore_index=True)
    instructiontexts += [instructions[0], instructions[1]]
  else:
    TDs                                     = pd.concat([TDtraceDelayed_1, TDarrowPercept_1], ignore_index=True)
    instructiontexts += [instructions[1], instructions[0]]
  
  if ('trialdefinitions' in cfg.keys()):
    # join tables for both trial types:
    cfg['trialdefinitions']                 = pd.concat([cfg['trialdefinitions'],TDs], ignore_index=True)
  else:
    cfg['trialdefinitions']                 = TDs


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
  
  cfg = onePassTrial(cfg)
  

# after the trials have been done, we concatenate the trials into one big file:

cfg['instruction'].text = 'creating data file'
cfg['instruction'].pos = (0,0)
cfg['instruction'].draw()
cfg['win'].flip()

participant_data = 0

for trialno in doTrials:
  
  trial_data = pd.DataFrame.from_csv('../data/onepass_V4/trials/onepass_V4_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index_col=None)
  
  if isinstance(participant_data, pd.DataFrame):
    participant_data = pd.concat([participant_data, trial_data])
  else:
    participant_data = trial_data
  
participant_data.to_csv('../data/onepass_V4/onepass_V4_p%02d.csv'%(cfg['id']), index=False, float_format='%0.3f')

#cfg['instruction'].text = 'creating movie'
#cfg['instruction'].pos = (0,0)
#cfg['instruction'].draw()
#cfg['win'].flip()
#cfg['win'].saveMovieFrames('../frames/p%02d.mp4'%cfg['id'], codec='libx264', fps=60, clearFrames=True)

# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


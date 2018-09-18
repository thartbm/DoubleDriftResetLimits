from setup import *
from onepass import *
import pandas as pd

# start building a config dictionary for the experiment:
cfg = {}

# get participant number and seed random number generator based on that:
cfg = getParticipantID(cfg)
seed(sum([ord(c) for c in 'illusory motion reset tracking']) + (cfg['id'] * 9999))


# add a window for visual stimuli:
cfg['fullscr']      = True
cfg['monitorIndex'] = 1
cfg['flip']         = False

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


# there will be 3 internal cursor movements: right, left and none:
internalMovement                        = [-3, -2, -1, 1, 2, 3]
externalMovement                        = [1./8, 1./6, 1./4]
perceptAngle                            = [-60, 60]
repetitions                             = 1

internalMovement                        = [-3, 3]
externalMovement                        = [1./4]
perceptAngle                            = [30, 150]
repetitions                             = 1

# create all desired combinations:
TD                                      = pd.concat([foldout(a=[internalMovement,externalMovement, perceptAngle])]*repetitions, ignore_index=True)
TD.columns                              = ['internalMovement', 'externalMovement', 'perceptAngle']

# copy for different trial types:
TDtrace                                 = TD
TDpercept                               = TD

# randomize:
TDtrace   = TDtrace.sample(frac=1).reset_index(drop=True)
TDpercept = TDpercept.sample(frac=1).reset_index(drop=True)

# add tracking and perceptual test parameters:
TDtrace['recordTrace']                  = True
TDtrace['recordPercept']                = False
TDpercept['recordTrace']                = False
TDpercept['recordPercept']              = True

# join tables for both trial types:
cfg['trialdefinitions']                 = pd.concat([TDtrace, TDpercept], ignore_index=True)

Ntrials = cfg['trialdefinitions'].shape[0]
print cfg['trialdefinitions']

cfg['trialdefinitions']['fixationSide'] = [random.choice([-1,1]) for _ in range(Ntrials)]

doTrials = range(Ntrials)
#doTrials = [4,5,6,7]

for trialno in doTrials:
  
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
  
  trial_data = pd.DataFrame.from_csv('../data/onepass/trials/onepass_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index_col=None)
  
  if isinstance(participant_data, pd.DataFrame):
    participant_data = pd.concat([participant_data, trial_data])
  else:
    participant_data = trial_data
  
  participant_data.to_csv('../data/onepass/onepass_p%02d.csv'%(cfg['id']), index=False, float_format='%0.3f')

#cfg['instruction'].text = 'creating movie'
#cfg['instruction'].pos = (0,0)
#cfg['instruction'].draw()
#cfg['win'].flip()
#cfg['win'].saveMovieFrames('../frames/p%02d.mp4'%cfg['id'], codec='libx264', fps=60, clearFrames=True)

# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


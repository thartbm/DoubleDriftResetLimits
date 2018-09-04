from setup import *
from boundedtracking import *


# start building a config dictionary for the experiment:
cfg = {}

# get participant number and seed random number generator based on that:
cfg = getParticipantID(cfg)
seed(sum([ord(c) for c in 'illusory cursor tracking']) + (cfg['id'] * 9999))


# add a window for visual stimuli:
cfg['fullscr'] = True
cfg['monitorIndex'] = 1
cfg['flip'] = False
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



# create some psychopy objects that will be used as stimuli:
cfg = createStimuli(cfg)


# there will be 3 internal cursor movements: right, left and none:
internalMovement           = [-3, -1, 0, 1, 3]
repetitions                = 1
cfg['internalMovements']   = internalMovement * repetitions
random.shuffle(cfg['internalMovements'])

Ntrials = len(cfg['internalMovements'])

cfg['trial_duration']      = 12
cfg['externalSpeed']       = .25 # Hz: how often can the cursor go up and down the whole range in a second
cfg['externalDirection']   = [random.choice([-1,1]) for _ in range(Ntrials)]
cfg['fixationSide']        = [random.choice([-1,1]) for _ in range(Ntrials)]

for trialno in range(Ntrials):
  
  print 'trial %d of %d'%(trialno+1, Ntrials)
  
  cfg['trial'] = trialno
  
  cfg = boundedTrackingTrial(cfg)
  

# after the trials have been done, we concatenate the trials into one big file:

participant_data = 0

for trialno in range(Ntrials):
  
  trial_data = pd.DataFrame.from_csv('../data/boundtrack_trials/boundtrack_p%02d_t%03d.csv'%(cfg['id'], trialno+1), index_col=None)
  
  if isinstance(participant_data, pd.DataFrame):
    participant_data = pd.concat([participant_data, trial_data])
  else:
    participant_data = trial_data
  
  participant_data.to_csv('../data/boundtrack_full/bounded_tracking_p%02d.csv'%(cfg['id']), index=False, float_format='%0.3f')

# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


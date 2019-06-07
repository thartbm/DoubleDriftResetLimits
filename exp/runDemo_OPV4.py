from setup import *
from demo_onepass_V4 import *
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

angles                                  = [50, 75, 105, 130]

fixationsides                           = [1, -1]
# add left & right fixation points?

TD                                      = pd.concat([foldout(a=[angles, fixationsides])])
TD.columns                              = ['angle', 'fixationSide']

TD = TD.sample(frac=1).reset_index(drop=True)

TD['internalSpeed'] = [sp.NaN] * TD.shape[0]

cfg['trialdefinitions'] = TD

Ntrials = cfg['trialdefinitions'].shape[0]
#print cfg['trialdefinitions']

print(cfg['trialdefinitions'])

instructiontrials = [0]
instructiontext = 'adjust the speed so the gabor moves straight'

#print(instructiontexts)


doTrials = range(Ntrials)

for trialno in doTrials:
  
  if trialno in instructiontrials:
    #print(trialno)
    #print(instructiontexts[trialno])
    cfg['instruction'].text = instructiontext
    cfg['instruction'].draw()
    cfg['win'].flip()
    event.waitKeys(keyList=['space'])
  
  print 'trial %d of %d'%(trialno+1, Ntrials)
  
  cfg['trial'] = trialno
  
  cfg = adjustTrial(cfg)
  

# put collected responses in a common framework:
td = cfg['trialdefinitions']
td['angdev'] = abs(90 - td['angle'])
td['speeddev'] = [td['internalSpeed'][idx] if td['angle'][idx] > 90 else -1 * td['internalSpeed'][idx] for idx in range(td.shape[0])]


# store the responses:
td.to_csv('../data/CVRdemo/CVRdemo_OPV4_adjusts_p%04d.csv'%(cfg['id']), index=False, float_format='%0.3f')

# calculate the median absolute speed deviation for each angle deviation from 90
bounds = td.groupby('angdev').median()['speeddev']

# stranslate to a range of illusion strengths, that _should_ go up from 15 degrees to 40 in steps of 2.5 degrees: 
strengths = sp.arange(bounds.values[0], bounds.values[1]+(sp.diff(bounds.values)/10), sp.diff(bounds.values)/10)








internalMovement = list(strengths) + list(-1* strengths)

# ========================== second part

externalMovement                        = [1./8]
perceptAngle                            = [90]
repetitions                             = 1

# these two should not be used:
Npasses                                 = 1
cfg['trial_duration']                   = (0.5 / cfg['externalSpeed']) * (Npasses + .5)


# create all desired combinations:
TD                                      = pd.concat([foldout(a=[internalMovement,externalMovement, perceptAngle])]*repetitions, ignore_index=True)
TD.columns                              = ['internalMovement', 'externalMovement', 'perceptAngle']

# copy for different trial types:
TDtraceDelayed                          = TD


blockrepeats = 1

#instructiontrials = [0,TD.shape[0]]
instructiontrials = [task * TD.shape[0] for task in range(blockrepeats)]

instructions = ['retrace the gabor path up to a reset point']

#instructiontexts = []

# randomize:
TDtraceDelayed_1  = TDtraceDelayed.sample(frac=1).reset_index(drop=True)
  
TDtraceDelayed_1['recordTrace']                = True
TDtraceDelayed_1['recordPercept']              = False
TDtraceDelayed_1['perceptTask']                = None
TDtraceDelayed_1['arrowPosition']              = None
TDtraceDelayed_1['traceTime']                  = 'delayed'
TDtraceDelayed_1['taskname']                   = 're-trace'
  
#TDs                                     = pd.concat([TDarrowPercept_1, TDtraceDelayed_1], ignore_index=True)
#instructiontexts += [instructions[0], instructions[1]]
  
cfg['trialdefinitions']                 = TDtraceDelayed_1


Ntrials = cfg['trialdefinitions'].shape[0]
#print cfg['trialdefinitions']

cfg['trialdefinitions']['fixationSide'] = [random.choice([-1,1]) for _ in range(Ntrials)]

doTrials = range(Ntrials)
#doTrials = range(54,72)

for trialno in doTrials:
  
  if trialno in instructiontrials:
    cfg['instruction'].text = instructions[trialno]
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
  
  trial_data = pd.DataFrame.from_csv('../data/CVRdemo/trials/onepass_V4_p%04d_t%03d.csv'%(cfg['id'], trialno+1), index_col=None)
  
  if isinstance(participant_data, pd.DataFrame):
    participant_data = pd.concat([participant_data, trial_data])
  else:
    participant_data = trial_data
  
participant_data.to_csv('../data/CVRdemo/CVRdemo_OPV4_resets_p%04d.csv'%(cfg['id']), index=False, float_format='%0.3f')

#cfg['instruction'].text = 'creating movie'
#cfg['instruction'].pos = (0,0)
#cfg['instruction'].draw()
#cfg['win'].flip()
#cfg['win'].saveMovieFrames('../frames/p%02d.mp4'%cfg['id'], codec='libx264', fps=60, clearFrames=True)

# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


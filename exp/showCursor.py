
from setup import *

def showCursor(cfg,duration=5):
  
  starttime = time.time()
  
  while (time.time() - starttime < duration):
    
    cfg['point'].draw()
    mousepos = cfg['mouse'].Pos()
    print(mousepos)

    cfg['gabor'].setPos(mousepos[0:2])
    cfg['gabor'].phase = ((time.time() - starttime) * 3, 0.0)
    cfg['gabor'].draw()

    cfg['cross'].setPos([0,-400])
    cfg['cross'].phase = ((time.time() - starttime) * 3, 0.0)
    cfg['cross'].draw()
    
    cfg['win'].flip()

# start building a config dictionary for the experiment:
cfg = {}

# add a window for visual stimuli:
cfg['fullscr'] = True
cfg['monitorIndex'] = 1
cfg['flip'] = True
cfg = createWindow(cfg)

# add a 'mouse' object for pen positions,
# use XLib if possible as it is more accurate:
mouse = event.Mouse(win=cfg['win'])
mouse.setVisible(False)
#cfg['mouse'] = mouse
# this creates a mouse object in the cfg dictionary
# it has a single method: Pos()
# that returns vector with an X and Y coordinate, and a time stamp
# it either uses X11, and if that fails it uses PsychoPy
cfg = addMouse(cfg)

cfg = createStimuli(cfg)

# this runs a test function, that should not be used anymore:
showCursor(cfg)


# cleanly exit the experiment with the computer in a usable state:
mouse.setVisible(True)
cfg['win'].close()


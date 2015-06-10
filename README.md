# patatap

![patatap](https://cloud.githubusercontent.com/assets/364725/8074534/e3c6a096-0f6e-11e5-8cf1-5f489be04e52.gif)

A quil sketch inspired by the fantastic [patatap](http://www.patatap.com/). It
has no sound, as it is intended to be used as a VJ tool (by recieving commands
over osc).

## Project layout:

* `core.clj` - main entry point
* `easings.clj` - easings taken from [tween-clj](https://github.com/gstamp/tween-clj).
* `ui.clj` - basic ui for quil - slider / checkbox
* `shapes.clj` - the shapes used

## use

    lein run in base dir

### keyboard commands

* u - show ui
* r - rendering on / off (to `/frames` directory)
* a - wipe 
* s - veil 
* d - prism 
* f - prism 
* g - prism 
* h - clay 
* j - piston
* k - piston
* l - piston
* z - confetti 
* x - glimmer 
* c - donut 
* v - frisbee 

## osc control

This sketch can be controlled by osc commands - eg, from ableton live or pure
data. The command endpoints are as follows:

  * /snare
  * /hat
  * /kick
  * /piano
  * /accent1
  * /accent2
  * /accent3

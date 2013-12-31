# `what` parameter of `scan` (unused)
what = list(target_side = "character",
target_time = "numeric",
cue_side = "character",
trials.thisRepN = "integer",
trials.thisTrialN = "integer",
trials.thisN = "integer",
trials.thisIndex = "integer",
block0.thisRepN = "integer",
block0.thisTrialN = "integer",
block0.thisN = "integer",
block0.thisIndex = "integer",
block1.thisRepN = "integer",
block1.thisTrialN = "integer",
block1.thisN = "integer",
block1.thisIndex = "integer",
trialKey.keys = "character",
trialKey.rt = "numeric",
date = "character",
frameRate = "numeric",
expName = "character",
session = "character",
participant = "character")

data0 = read.csv(file = file)
data0

#dataAll = data.frame(data0, data1)

data1 = subset(data0, select = -c(X))
data1
file = "Filip_2013_XII_31_1536.csv"

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

sep = ","
skip = 1
fileEncoding = "UTF-8"
encoding = "UTF-8"

data = scan(file = file, what = what, sep = sep, skip = skip, fileEncoding = fileEncoding, encoding = encoding, flush = TRUE)
data
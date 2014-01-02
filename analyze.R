# Execute this script in the directory "ior-data/participants"

participantsFile = "participants.csv"

library(reshape)

participantColClasses = list(
path = "character",
name = "character",
age = "integer",
sex = "factor",
date = "Date",
computer = "factor",
version = "factor",
commit = "factor")

partDataColClasses = list(
target_side = "factor",
target_time = "numeric",
cue_side = "factor",
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
expName = "factor",
session = "factor",
participant = "factor")

participants = read.csv(file = participantsFile, colClasses = participantColClasses, fileEncoding = "UTF-8")
participants$id <- row.names(participants) # anonymous participant id
participantsSub = subset(participants, select = c(id, path))

procParticipant <- function(participant) {
partData <- read.csv(file = participant$path, colClasses = partDataColClasses, fileEncoding = "UTF-8")
partData <- subset(partData, select = -c(X)) # empty column added by PsychoPy
partData$participantId <- rep(participant$id, times = nrow(partData))
partData
}

res = by(participantsSub, 1:nrow(participantsSub), procParticipant)

resres = reshape::merge_all(res)

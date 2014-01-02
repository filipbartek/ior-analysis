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
partData$participantId <- rep(participant$id, times = nrow(partData))
return(partData)
}

dataList = by(participantsSub, 1:nrow(participantsSub), procParticipant)

dataAll = reshape::merge_all(dataList)

# Now `dataAll` contains all PsychoPy output data in one data frame with extra column `participantId` specifying the participant

dataAll = subset(dataAll, select = -c(X)) # empty column added by PsychoPy

dataAll$block <- NA_integer_
dataAll <- transform(dataAll, block = ifelse(!is.na(block0.thisRepN), 0, block))
dataAll <- transform(dataAll, block = ifelse(!is.na(block1.thisRepN), 1, block))

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

# block
dataAll$block <- NA
dataAll <- transform(dataAll, block = ifelse(!is.na(trials.thisRepN), "trials", block))
dataAll <- transform(dataAll, block = ifelse(!is.na(block0.thisRepN), "block0", block))
dataAll <- transform(dataAll, block = ifelse(!is.na(block1.thisRepN), "block1", block))
dataAll$block <- factor(dataAll$block, levels = c("trials", "block0", "block1"), ordered = TRUE)

# thisRepN
dataAll <- transform(dataAll,
	thisRepN = ifelse(!is.na(trials.thisRepN), trials.thisRepN,
		ifelse(!is.na(block0.thisRepN), block0.thisRepN,
			ifelse(!is.na(block1.thisRepN), block1.thisRepN, NA)
		)
	)
)
dataAll <- subset(dataAll, select = -c(trials.thisRepN, block0.thisRepN, block1.thisRepN))

# thisTrialN
dataAll <- transform(dataAll,
	thisTrialN = ifelse(!is.na(trials.thisTrialN), trials.thisTrialN,
		ifelse(!is.na(block0.thisTrialN), block0.thisTrialN,
			ifelse(!is.na(block1.thisTrialN), block1.thisTrialN, NA)
		)
	)
)
dataAll <- subset(dataAll, select = -c(trials.thisTrialN, block0.thisTrialN, block1.thisTrialN))

# thisN
dataAll <- transform(dataAll,
	thisN = ifelse(!is.na(trials.thisN), trials.thisN,
		ifelse(!is.na(block0.thisN), block0.thisN,
			ifelse(!is.na(block1.thisN), block1.thisN, NA)
		)
	)
)
dataAll <- subset(dataAll, select = -c(trials.thisN, block0.thisN, block1.thisN))

# thisIndex
dataAll <- transform(dataAll,
	thisIndex = ifelse(!is.na(trials.thisIndex), trials.thisIndex,
		ifelse(!is.na(block0.thisIndex), block0.thisIndex,
			ifelse(!is.na(block1.thisIndex), block1.thisIndex, NA)
		)
	)
)
dataAll <- subset(dataAll, select = -c(trials.thisIndex, block0.thisIndex, block1.thisIndex))

dataAll <- subset(dataAll, select = c(participantId, block, target_side, target_time, cue_side, trialKey.rt))

dataBlocks <- subset(dataAll, block %in% c("block0", "block1"))
dataBlocks <- subset(dataAll, select = -c(block))

dataRes <- dataBlocks

dataRes <- transform(dataRes, exp.rt = (target_time / 1000) + 0.5)
dataRes <- transform(dataRes, delay = trialKey.rt - exp.rt)
dataRes <- transform(dataRes, corrDelay = (delay >= 0 & delay <= 0.5))
dataRes <- transform(dataRes, corr = (target_side == "none" & is.na(trialKey.rt)) | (target_side != "none" & !is.na(corrDelay) & corrDelay))

getSuccRate <- function(trials) {
succCount = nrow(subset(trials, corr))
succR <- succCount / nrow(trials)
return(c(nrow(trials), succCount, succR))
}

succRate <- by(dataRes, dataRes$participantId, getSuccRate)
succRate
getSuccRate(dataRes)

# Execute this script in the directory "ior-data/participants"

library(reshape)
library(Hmisc)

# Change working directory to the one that contains experiment data
# Assumes we're on TTT-HP computer
setwd("C:/Users/TTT/Documents/GitHub/ior-data/participants")

participantsFile = "participants.csv"

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

# Remove empty column added by PsychoPy
dataAll = subset(dataAll, select = -c(X))

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

# Save data in a CSV file
dataOut = dataAll
dataOut <- subset(dataOut, select = c(participantId, block, target_side, target_time, cue_side, trialKey.rt))
write.csv(x = dataOut, file = "data.csv")

# Only keep interesting columns
dataAll <- subset(dataAll, select = c(participantId, block, target_side, target_time, cue_side, trialKey.rt))

# Discard training trials
dataBlocks <- subset(dataAll, block %in% c("block0", "block1"))

# Merge blocks
dataBlocks <- subset(dataAll, select = -c(block))

dataRes <- dataBlocks

# Determine correctness of answers
dataRes <- transform(dataRes, exp.rt = (target_time / 1000) + 0.5)
dataRes <- transform(dataRes, delay = trialKey.rt - exp.rt)
dataRes <- transform(dataRes, corrDelay = (delay >= 0 & delay <= 0.5))
dataRes <- transform(dataRes, corr = (target_side == "none" & is.na(trialKey.rt)) | (target_side != "none" & !is.na(corrDelay) & corrDelay))

# Remove uninteresting columns
dataRes <- subset(dataRes, select = c(participantId, target_side, target_time, cue_side, delay, corr))

# Print success rate for every participant and total
aggregate(dataRes$corr, list(participantId = dataRes$participantId), mean)
mean(dataRes$corr)

# Print success rates for various interesting subsets
aggregate(dataRes$corr, list(target_side = dataRes$target_side), mean)
aggregate(dataRes$corr, list(target_time = dataRes$target_time), mean)
aggregate(dataRes$corr, list(cue_side = dataRes$cue_side), mean)
aggregate(dataRes$corr, list(target_side = dataRes$target_side, cue_side = dataRes$cue_side), mean)
aggregate(dataRes$corr, list(target_side = dataRes$target_side, target_time = dataRes$target_time, cue_side = dataRes$cue_side), mean)

dataSides <- dataRes
# Discard trials with target_side none or central
dataSides <- subset(dataSides, target_side %in% c("left", "right"))

# Add `cued` column
dataSides <- transform(dataSides, cued = ifelse(target_side == "left" | target_side == "right", (as.character(target_side) == as.character(cue_side)), NA))

# Print means
aggregate(dataSides$corr, list(cued = dataSides$cued), mean)
aggregate(dataSides$corr, list(cued = dataSides$cued, target_time = dataSides$target_time), mean)

# Discard trials with answers given too early or too late or not at all
dataSides <- subset(dataSides, corr == TRUE)

# Remove uninteresting columns
dataSides <- subset(dataSides, select = c(participantId, target_time, delay, cued))

# Change unit of delay to ms from s
dataSides$delay <- dataSides$delay * 1000

# Print means of delay across cued and target_time
aggregate(dataSides$delay, list(cued = dataSides$cued, target_time = dataSides$target_time), mean)

# How quick are the participants?
aggregate(dataSides$delay, list(dataSides$participantId), mean)

# Plot reaction times
# Prepare means and sds
sidesMean <- aggregate(dataSides$delay, list(target_time = dataSides$target_time, cued = dataSides$cued), mean)
sidesSd <- aggregate(dataSides$delay, list(target_time = dataSides$target_time, cued = dataSides$cued), sd)
fMean <- subset(sidesMean, cued == FALSE, select = -c(cued))
fSd <- subset(sidesSd, cued == FALSE, select = -c(cued))
tMean <- subset(sidesMean, cued == TRUE, select = -c(cued))
tSd <- subset(sidesSd, cued == TRUE, select = -c(cued))

# Plot main data
#svg(filename = "main.svg")
png(filename = "main.png", width = 960, height = 960, pointsize = 24)
errbar(fMean$target_time - 1, fMean$x, fMean$x + fSd$x, fMean$x - fSd$x, col = "red", xlim = c(0, 500), xlab = NA, ylab = NA,
	lty = "solid", type = "o", xaxt = "n")
title(main = "Reaction times by interval", xlab = "Interval (ms)", ylab = "Reaction time (ms)")
par(new=TRUE)
errbar(tMean$target_time + 1, tMean$x, tMean$x + tSd$x, tMean$x - tSd$x, add = TRUE, col = "green", xlab = NA, ylab = NA,
	lty = "solid", type = "o", xaxt = "n")
legend("topright", title="Cue", c("Uncued", "Cued"), fill=c("red", "green"))
axis(1, c(50, 200, 350, 403), c("50", "200", "350", "403"))

# Plot linear fits
mTrue <- lm(delay~target_time, subset(dataSides, cued == TRUE))
abline(mTrue$coef, lty = "dashed", col = "green")
mFalse <- lm(delay~target_time, subset(dataSides, cued == FALSE))
abline(mFalse$coef, lty = "dashed", col = "red")

# Plot intersection of linear fits
cm <- rbind(coef(mTrue),coef(mFalse)) # Coefficient matrix
p <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
points(c(p[1]), c(p[2]))
dev.off()

# Print position of the intersection
print(p)

# Convert logical `cued` to a factor with labels "Uncued" and "Cued"
dataSidesPretty <- dataSides
dataSidesPretty$cued <- factor(dataSidesPretty$cued, labels = c("Uncued", "Cued"))

# Boxplot of the 6 groups
#svg(filename = "boxplot.svg")
png(filename = "boxplot.png", width = 960, height = 960, pointsize = 24)
boxplot(delay~cued*target_time, dataSidesPretty, col = c("red", "green"), names = c("50", "50", "200", "200", "350", "350"),
	main = "Reaction time distribution by cue and interval", ylab = "Reaction time (ms)", xlab = "Interval (ms)")
legend("bottomleft", title="Cue", c("Uncued", "Cued"), fill=c("red", "green"))
dev.off()

# ANOVA - Analysis of variance
aov.ex = aov(delay~cued*target_time, dataSidesPretty)
summary(aov.ex)
#print(model.tables(aov.ex, "means"),digits=3)

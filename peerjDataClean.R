library("pander")
library("pastecs")
library("lubridate")
library("ggplot2")
library("lattice")
library("quantreg")
library("gmodels")
library("psych")
library("Hmisc")

# Import data
peerjMetadata <- read.csv("~/Dropbox/workspace/peerj/data-processed/peerjMetadata.csv")

# Update variables to the appropriate class

## Convert to date objects
peerjMetadata$RecdDate   <- as.Date(peerjMetadata$RecdDate, "%m/%d/%y")
peerjMetadata$AcceptDate <- as.Date(peerjMetadata$AcceptDate, "%m/%d/%y")
peerjMetadata$PubDate    <- as.Date(peerjMetadata$PubDate, "%m/%d/%y")

## Convert to proper NAs
peerjMetadata$NoReviewers <- as.integer(gsub("na", NA, peerjMetadata$NoReviewers))

## Convert to proper NAs and correct factors
peerjMetadata$PeerAnon <- gsub("na", NA, peerjMetadata$PeerAnon)
peerjMetadata$PeerAnon <- factor(peerjMetadata$PeerAnon,
                                 levels = c(0, 1, 2),
                                 labels = c("Anonymous",
                                            "Public", "Mixed"))

## Convert to proper NAs and correct factors
peerjMetadata$NoRevisions <- as.integer(gsub("na", NA, peerjMetadata$NoRevisions))
peerjMetadata$NoRevisions <- ordered(peerjMetadata$NoRevisions,
                                     levels = c(0, 1, 2),
                                     labels = c("No Revisions",
                                                "One Revision",
                                                "Two Revisions"))

## Convert to proper NAs and correct factors
peerjMetadata$RevStanding1 <- gsub("na", NA, peerjMetadata$RevStanding1)
peerjMetadata$RevStanding1 <- ordered(peerjMetadata$RevStanding1, levels =
                                        c("None", "Minor", "Major"))


## Convert to proper NAs and correct factors
peerjMetadata$RevStanding2 <- gsub("na", NA, peerjMetadata$RevStanding2)
peerjMetadata$RevStanding2 <- ordered(peerjMetadata$RevStanding2, levels =
                                        c("Minor", "Major"))

## Convert to proper NAs and correct factors
peerjMetadata$Correction <- gsub("na", NA, peerjMetadata$Correction)
peerjMetadata$Correction <- factor(peerjMetadata$Correction,
                                   levels = "Minor")

## Convert to date objects
peerjMetadata$DataCol1 <- as.Date(peerjMetadata$DataCol1, "%m/%d/%y")
peerjMetadata$DataCol2 <- as.Date(peerjMetadata$DataCol2, "%m/%d/%y")
peerjMetadata$DataCol3 <- as.Date(peerjMetadata$DataCol3, "%m/%d/%y")
peerjMetadata$DataCol4 <- as.Date(peerjMetadata$DataCol4, "%m/%d/%y")
peerjMetadata$DataCol5 <- as.Date(peerjMetadata$DataCol5, "%m/%d/%y")

## Create new date variables, save as new data frame
accept2pubDays   <- peerjMetadata$PubDate - peerjMetadata$AcceptDate
recvd2pubDays    <- peerjMetadata$PubDate - peerjMetadata$RecdDate
recvd2acceptDays <- peerjMetadata$AcceptDate - peerjMetadata$RecdDate
speedOfReview    <- cbind(accept2pubDays, recvd2acceptDays, recvd2pubDays)
speedOfReview    <- as.data.frame(speedOfReview)
rm(accept2pubDays, recvd2pubDays, recvd2acceptDays)

### Save class difftime as class numeric
### peerjMetadata$r2pub <- as.numeric(recvd2pubDays)    # received date to publication date
### peerjMetadata$r2acc <- as.numeric(recvd2acceptDays) # received date to accept date
### peerjMetadata$a2pub <- as.numeric(accept2pubDays)   # accept date to publication date

# Subset data:
## Subset data frame by articles before launch and articles after launch
pjBeforeLaunch <- subset(peerjMetadata, peerjMetadata$RecdDate < "2013-02-12")
pjAfterLaunch  <- subset(peerjMetadata, peerjMetadata$RecdDate >= "2013-02-12")

## Add new variables for subsetted data
## Speed of Review and Publication
speedAccept2pubDaysBEFORE   <- pjBeforeLaunch$PubDate - pjBeforeLaunch$AcceptDate
speedRecvd2pubDaysBEFORE    <- pjBeforeLaunch$PubDate - pjBeforeLaunch$RecdDate
speedRecvd2acceptDaysBEFORE <- pjBeforeLaunch$AcceptDate - pjBeforeLaunch$RecdDate

speedAccept2pubDaysAFTER   <- pjAfterLaunch$PubDate - pjAfterLaunch$AcceptDate
sppedRecvd2pubDaysAFTER    <- pjAfterLaunch$PubDate - pjAfterLaunch$RecdDate
speedRecvd2acceptDaysAFTER <- pjAfterLaunch$AcceptDate - pjAfterLaunch$RecdDate

# Subset data:
## Subset data frame by articles with open peer review and by articles with
## close peer review
pjClosedReview <- subset(peerjMetadata,
                         peerjMetadata$PeerRevAvail == "No")
pjOpenReview   <- subset(peerjMetadata,
                         peerjMetadata$PeerRevAvail == "Yes")


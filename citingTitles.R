# Load Data
citingJournals1 <- read.csv("~/Dropbox/workspace/peerj/data-processed/citingJournals1.csv")
citingJournals2 <- read.csv("~/Dropbox/workspace/peerj/data-processed/citingJournals2.csv")

# Fix variables for citingJournals1

## Change to character class
citingJournals1$title1 <- as.character(citingJournals1$title1)
citingJournals1$title2 <- as.character(citingJournals1$title2)
citingJournals1$title3 <- as.character(citingJournals1$title3)
citingJournals1$title4 <- as.character(citingJournals1$title4)
citingJournals1$title5 <- as.character(citingJournals1$title5)
citingJournals1$title6 <- as.character(citingJournals1$title6)
citingJournals1$title7 <- as.character(citingJournals1$title7)
citingJournals1$title8 <- as.character(citingJournals1$title8)
citingJournals1$title9 <- as.character(citingJournals1$title9)
citingJournals1$title10 <- as.character(citingJournals1$title10)

## Insert proper NAs in data
citingJournals1$title1[citingJournals1$title1 == ""] <- NA
citingJournals1$title2[citingJournals1$title2 == ""] <- NA
citingJournals1$title3[citingJournals1$title3 == ""] <- NA
citingJournals1$title4[citingJournals1$title4 == ""] <- NA
citingJournals1$title5[citingJournals1$title5 == ""] <- NA
citingJournals1$title6[citingJournals1$title6 == ""] <- NA
citingJournals1$title7[citingJournals1$title7 == ""] <- NA
citingJournals1$title8[citingJournals1$title8 == ""] <- NA
citingJournals1$title9[citingJournals1$title9 == ""] <- NA
citingJournals1$title10[citingJournals1$title10 == ""] <- NA

# Fix variables for citingJournals2

## Insert proper NAs in data 
citingJournals2$SNIP2012        <- gsub("na", NA, citingJournals2$SNIP2012)
citingJournals2$SJR2012         <- gsub("na", NA, citingJournals2$SJR2012)
citingJournals2$ArchivePolicy   <- gsub("na", NA, citingJournals2$ArchivePolicy)
citingJournals2$OAStatus        <- gsub("na", NA, citingJournals2$OAStatus)

## Convert titles to character class
citingJournals2$title           <- as.character(citingJournals2$title)

## Convert to numeric class
citingJournals2$SJR2012  <- as.numeric(citingJournals2$SJR2012)
citingJournals2$SNIP2012 <- as.numeric(citingJournals2$SNIP2012)

## Convert to ordered factors, order level by stronger OA
citingJournals2$ArchivePolicy <- ordered(citingJournals2$ArchivePolicy,
                                         levels = c("Green",
                                                    "Blue",
                                                    "Yellow",
                                                    "White",
                                                    "ungraded"))

## Convert to ordered factors, order level by stronger OA
citingJournals2$OAStatus <- ordered(citingJournals2$OAStatus,
                                    levels = c("DOAJ",
                                               "Paid OA",
                                               "No-paid OA"))

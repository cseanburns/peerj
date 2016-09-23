# Load libraries
#library("quantreg")
library("pander")
library("pastecs")
library("lubridate")
library("ggplot2")
library("lattice")
library("gmodels")
library("psych")
library("Hmisc")
library("reshape2")
library("pgirmess")
library("dplyr")

---

# Global Functions

## Descriptive statistics
basicInfo <- function(x) {
  round(stat.desc(as.numeric(x), norm = TRUE, p = 0.95), 4)
}

## Percentage change
perChange <- function(b, a) {
  ((b - a)/a) * 100
}

---

# RESULTS SECTION

---

# CHARACTERISTICS OF PEER REVIEW
## Clean NAs
peerjMetadata$PeerRevAvail[is.na(peerjMetadata$PeerRevAvail)] <- "No"

## How many public peer review histories are there?
table(peerjMetadata$PeerRevAvail)
chisq.test(table(peerjMetadata$PeerRevAvail))

## Are referees more or less likely to be anonymous?
## 0 = All anon; 1 = All public; 2 = Mix of Anon and Public
table(peerjMetadata$PeerAnon) # results in sum
chisq.test(table(peerjMetadata$PeerAnon))

table(peerjMetadata$PubDate,
      peerjMetadata$PeerAnon) # provides breakdown

## Table 1 in pj-tables.odt
## Revision status and number of revisions
table(peerjMetadata$NoRevisions)
chisq.test(table(peerjMetadata$NoRevisions))
## Remove the as-is article as an outlider
ap <- table(peerjMetadata$NoRevisions)
ap <- ap[-1]
chisq.test(ap)
rm(ap)

table(peerjMetadata$NoRevisions, peerjMetadata$RevStanding1)

## Save table in temp variable, then remove first row (No Revisions)
## and first column (None) for chi-square analysis
table(peerjMetadata$NoRevisions, peerjMetadata$RevStanding1)
nrrs <- table(peerjMetadata$NoRevisions, peerjMetadata$RevStanding1)
nrrs <- nrrs[-1,-1]

CrossTable(nrrs, fisher = TRUE, chisq = TRUE, expected = TRUE,
           prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE,
           sresid = TRUE, format = "SPSS")
rm(nrrs)

## Word counts of the peer review process
stat.desc(totalWordCounts)

---

# SPEED OF REVIEW AND PUBLICATION

## How long to publish after receiving manuscript?
speedRecvd2pubDays <- peerjMetadata$PubDate - peerjMetadata$RecdDate
stat.desc(as.vector(speedRecvd2pubDays))
plot(speedRecvd2pubDays, ylab = "Days from Reception to Publication",
     xlab = "Progression of Published Articles",
     main = "Reception to Publication")
stat.desc(as.vector(speedRecvd2pubDaysBEFORE))
stat.desc(as.vector(sppedRecvd2pubDaysAFTER))  

## How long to accept after receiving manuscript?
plot(speedRecvd2accept, ylab = "Days from Reception to Acceptance",
     xlab = "Progression of Published Articles",
     main = "Reception to Acceptance")
speedRecvd2accept <- peerjMetadata$AcceptDate - peerjMetadata$RecdDate
stat.desc(as.vector(speedRecvd2accept))
stat.desc(as.vector(speedRecvd2acceptDaysBEFORE))
stat.desc(as.vector(speedRecvd2acceptDaysAFTER))

## How long to publish after acceptance?
plot(speedAccept2pubDaysAFTER, ylab = "Days from Acceptance to Publication",
     xlab = "Progression of Published Articles",
     main = "Acceptance to Publication")
speedAccept2pubDays <- peerjMetadata$PubDate - peerjMetadata$AcceptDate
stat.desc(as.vector(speedAccept2pubDays))
stat.desc(as.vector(speedAccept2pubDaysBEFORE))
stat.desc(as.vector(speedAccept2pubDaysAFTER))

---

# AUTHOR CHARACTERISTICS

## AUTHOR COUNT
stat.desc(peerjMetadata$AuthCount)

## MULTINATIONAL AUTHORSHIP: COUNT AND PERCENTAGE
table(peerjMetadata$MultiNational)
table(peerjMetadata$MultiNational) / length(peerjMetadata$MultiNational)
chisq.test(ftable(peerjMetadata$MultiNational))

## Table 2 in pj-tables.odt
gender <- factor(peerjMetadata$Gender)
chisq.test(table(gender))
nationality <- factor(peerjMetadata$MultiNational,
                      levels = c("No", "Yes"), labels = c("Single", "Multi"))
firstGender <- factor(peerjMetadata$FemaleFirst,
                      levels = c("No", "Yes"),
                      labels = c("MaleFirst", "FemaleFirst"))
ftable(gender, nationality, firstGender)

CrossTable(nationality, firstGender,
           fisher = TRUE, chisq = TRUE, expected = TRUE,
           prop.c = TRUE, prop.t = TRUE, prop.chisq = FALSE,
           sresid = TRUE, format = "SPSS")

CrossTable(gender, firstGender, # assumption not met
           fisher = TRUE, chisq = TRUE, expected = TRUE,
           prop.c = TRUE, prop.t = TRUE, prop.chisq = FALSE,
           sresid = TRUE, format = "SPSS")


ct1 <- xtabs(~ gender + nationality + firstGender)
ftable(ct1)
chisq.test(ftable(ct1), simulate.p.value = TRUE)

ftable(firstGender[gender == "Mixed"])
chisq.test(ftable(firstGender[gender == "Mixed"]))

---

# SOCIAL METRICS

## DOWNLOADS

stat.desc(peerjMetadata$DownloadsMay)
stat.desc(peerjMetadata$DownloadsAug14)
perChange(stat.desc(peerjMetadata$DownloadsAug14), stat.desc(peerjMetadata$DownloadsMay))

### Tables 3a and 3b in pj-tables.odt
describeBy(peerjMetadata$DownloadsMay,
           group = month(peerjMetadata$PubDate))
describeBy(peerjMetadata$DownloadsAug14,
           group = month(peerjMetadata$PubDate))

stat.desc(pjBeforeLaunch$DownloadsMay)
stat.desc(pjBeforeLaunch$DownloadsAug14)
stat.desc(pjAfterLaunch$DownloadsMay)
stat.desc(pjAfterLaunch$DownloadsAug14)
describeBy(pjBeforeLaunch$DownloadsMay,
           group = month(pjBeforeLaunch$PubDate))
describeBy(pjAfterLaunch$DownloadsAug14,
           group = month(pjAfterLaunch$PubDate))

length(peerjMetadata$DownloadsMay[peerjMetadata$PubDate > "2013-02-28"])
length(peerjMetadata$DownloadsAug14[peerjMetadata$PubDate > "2013-02-28"])
summary(peerjMetadata$DownloadsMay[peerjMetadata$PubDate > "2013-02-28"])
summary(peerjMetadata$DownloadsAug14[peerjMetadata$PubDate > "2013-02-28"])
perChange(396, 340.5) # the medians from the above two

### Table 4 in pj-tables.odt
### Correlation Matrix
### Usage: Create matrices using each of the months. Ex:
### Usage: spearCor(peerjMetadata$UniqueVisitorsAug, peerjMetadata$PageviewsAug,
###                 peerjMetadata$SocRefUniqAug, peerjMetadata$SocRefTotalAug,
###                 peerjMetadata$TopRefUniqAug, peerjMetadata$TopRefTotalAug)
spearCor <- function(x,y,z,a,b,c){
  rcorr(cbind(x,y,z,a,b,c), type = "spearman")
}

spearCor(peerjMetadata$UniqueVisitorsAug, peerjMetadata$PageviewsAug,
         peerjMetadata$SocRefUniqAug, peerjMetadata$SocRefTotalAug,
         peerjMetadata$TopRefUniqAug, peerjMetadata$TopRefTotalAug)

spearCor(peerjMetadata$UniqueVisitorsNov, peerjMetadata$PageviewsNov,
         peerjMetadata$SocRefUniqNov, peerjMetadata$SocRefTotalNov,
         peerjMetadata$TopRefUniqNov, peerjMetadata$TopRefTotalNov)

spearCor(peerjMetadata$UniqueVisitorsFeb, peerjMetadata$PageviewsFeb,
         peerjMetadata$SocRefUniqFeb, peerjMetadata$SocRefTotalFeb,
         peerjMetadata$TopRefUniqFeb, peerjMetadata$TopRefTotalFeb)

spearCor(peerjMetadata$UniqueVisitorsMay, peerjMetadata$PageviewsMay,
         peerjMetadata$SocRefUniqMay, peerjMetadata$SocRefTotalMay,
         peerjMetadata$TopRefUniqMay, peerjMetadata$TopRefTotalMay)

spearCor(peerjMetadata$UniqueVisitorsAug14, peerjMetadata$PageviewsAug14,
         peerjMetadata$SocRefUniqAug14, peerjMetadata$SocRefTotalAug14,
         peerjMetadata$TopRefUniqAug14, peerjMetadata$TopRefTotalAug14)

## UNIQUE VISITORS
### August 2013 - August 2014, Total
stat.desc(peerjMetadata$UniqueVisitorsAug)
stat.desc(peerjMetadata$UniqueVisitorsAug14)
perChange(stat.desc(peerjMetadata$UniqueVisitorsAug14),
          stat.desc(peerjMetadata$UniqueVisitorsAug))

### August 2013 - November 2013, Partial
stat.desc(peerjMetadata$UniqueVisitorsAug)
stat.desc(peerjMetadata$UniqueVisitorsNov)
perChange(stat.desc(peerjMetadata$UniqueVisitorsNov),
          stat.desc(peerjMetadata$UniqueVisitorsAug))

### November 2013 - February 2014, Partial
stat.desc(peerjMetadata$UniqueVisitorsNov)
stat.desc(peerjMetadata$UniqueVisitorsFeb)
perChange(stat.desc(peerjMetadata$UniqueVisitorsFeb),
          stat.desc(peerjMetadata$UniqueVisitorsNov))

### February 2014 - May 2014, Partial
stat.desc(peerjMetadata$UniqueVisitorsFeb)
stat.desc(peerjMetadata$UniqueVisitorsMay)
perChange(stat.desc(peerjMetadata$UniqueVisitorsMay),
          stat.desc(peerjMetadata$UniqueVisitorsFeb))

### May 2014 - August 2014, Partial
stat.desc(peerjMetadata$UniqueVisitorsMay)
stat.desc(peerjMetadata$UniqueVisitorsAug14)
perChange(stat.desc(peerjMetadata$UniqueVisitorsAug14),
          stat.desc(peerjMetadata$UniqueVisitorsMay))

## PAGEVIEWS
### August 2013 - August 2014, Total
stat.desc(peerjMetadata$PageviewsAug)
stat.desc(peerjMetadata$PageviewsAug14)
perChange(stat.desc(peerjMetadata$PageviewsAug14),
          stat.desc(peerjMetadata$PageviewsAug))

### August 2013 - November 2013, Partial
stat.desc(peerjMetadata$PageviewsAug)
stat.desc(peerjMetadata$PageviewsNov)
perChange(stat.desc(peerjMetadata$PageviewsNov),
          stat.desc(peerjMetadata$PageviewsAug))

### November 2013 - February 2014, Partial
stat.desc(peerjMetadata$PageviewsNov)
stat.desc(peerjMetadata$PageviewsFeb)
perChange(stat.desc(peerjMetadata$PageviewsFeb),
          stat.desc(peerjMetadata$PageviewsNov))

### February 2014 - May 2014, Partial
stat.desc(peerjMetadata$PageviewsFeb)
stat.desc(peerjMetadata$PageviewsMay)
perChange(stat.desc(peerjMetadata$PageviewsMay),
          stat.desc(peerjMetadata$PageviewsFeb))

### May 2014 - August 2014, Partial
stat.desc(peerjMetadata$PageviewsMay)
stat.desc(peerjMetadata$PageviewsAug14)
perChange(stat.desc(peerjMetadata$PageviewsAug14),
          stat.desc(peerjMetadata$PageviewsMay))

### Table 5 pj-tables.odt
### How many unique visitors per date since day of publication:
### Usage: visitorsByDay(peerjMetadata$UniqueVisitorsAug,
###                      peerjMetadata$DataCol1,
###                      peerjMetadata$PubDate)
visitorsByDay <- function(x,y,z) {
  a <- x / as.numeric(y - z) # Visitors / Date of data collection - Pub Date
  print("Visitors / Day")
    print(round(stat.desc(a), 2))
  print("Days since publication")
    print(round(stat.desc(y - z), 2))
}

### How many page views per date since day of publication:
### Usage: pageviewsByDay(peerjMetadata$PagviewsAug,
###                      peerjMetadata$DataCol1,
###                      peerjMetadata$PubDate)
pageviewsByDay <- function(x,y,z) {
  a <- x / as.numeric(y - z) # Page views / Date of data collection - Pub Date
  print("Pageviews / Day")
    print(round(stat.desc(a), 2))
  print("Days since publication")
    print(round(stat.desc(y - z), 2)) # Days since publication
}

## SOCIAL REFERRALS
stat.desc(peerjMetadata$SocRefUniqAug)
stat.desc(peerjMetadata$SocRefTotalAug)

stat.desc(peerjMetadata$SocRefUniqNov)
stat.desc(peerjMetadata$SocRefTotalNov)

stat.desc(peerjMetadata$SocRefUniqFeb)
stat.desc(peerjMetadata$SocRefTotalFeb)

stat.desc(peerjMetadata$SocRefUniqMay)
stat.desc(peerjMetadata$SocRefTotalMay)

stat.desc(peerjMetadata$SocRefUniqAug14)
stat.desc(peerjMetadata$SocRefTotalAug14)

## TOP REFERRALS

stat.desc(peerjMetadata$TopRefUniqAug)
stat.desc(peerjMetadata$TopRefTotalAug)

stat.desc(peerjMetadata$TopRefUniqNov)
stat.desc(peerjMetadata$TopRefTotalNov)

stat.desc(peerjMetadata$TopRefUniqFeb)
stat.desc(peerjMetadata$TopRefTotalFeb)

stat.desc(peerjMetadata$TopRefUniqMay)
stat.desc(peerjMetadata$TopRefTotalMay)

stat.desc(peerjMetadata$TopRefUniqAug14)
stat.desc(peerjMetadata$TopRefTotalAug14)

---

# CITATION ANALYSIS
## GOOGLE SCHOLAR CITATIONS
### Friedman rank sum test with friedmanmc post hoc
gsc <- as.matrix(cbind(peerjMetadata$GSFeb,
                       peerjMetadata$GSMay,
                       peerjMetadata$GSAug14))

round(stat.desc(gsc, basic = TRUE,
                desc = TRUE, norm = TRUE,
                p = 0.95), digits = 3)
friedman.test(gsc)
friedmanmc(gsc, probs = 0.05)  
boxplot(gsc)
rm(gsc)

## SCOPUS CITATIONS
### Friedman rank sum test with friedmanmc post hoc
sc <- as.matrix(cbind(peerjMetadata$ScopusFeb,
                      peerjMetadata$ScopusMay,
                      peerjMetadata$ScopusAug14))

round(stat.desc(sc, basic = TRUE,
                desc = TRUE, norm = TRUE,
                p = 0.95), digits = 3)
friedman.test(sc)
friedmanmc(sc, probs = 0.05)
boxplot(sc)
rm(sc)

cor.test(peerjMetadata$GSFeb, peerjMetadata$ScopusFeb,
         alternative = "two.sided", method = "spearman",
         exact = FALSE)

cor.test(peerjMetadata$GSMay, peerjMetadata$ScopusMay,
         alternative = "two.sided", method = "spearman",
         exact = FALSE)

cor.test(peerjMetadata$GSAug14, peerjMetadata$ScopusAug14,
         alternative = "two.sided", method = "spearman",
         exact = FALSE)

## CITED SOURCES

### TITLES

# Table 6
# figure out percentages
# Note: citedJournals was saved as citedJournals.csv
# Note: tjfsorted was saved as journalTitlesCitedUnique.csv
# library(qcc)
citedJournals       <- read.table(file = "data-processed/citedJournals.csv", header = TRUE)
journalTitlesSorted <- sort(table(citedJournals$titles))
journalTitlesSorted <- as.matrix(journalTitlesSorted)
journalTitlesSorted <- as.data.frame(journalTitlesSorted)
journalTitlesSorted$titles      <- rownames(journalTitlesSorted)
rownames(journalTitlesSorted)   <- NULL
colnames(journalTitlesSorted)   <- c("titleCount", "titles")
journalTitlesSorted             <- arrange(journalTitlesSorted, desc(titleCount))
journalTitlesSorted$cumsum      <- cumsum(journalTitlesSorted$titleCount)
journalTitlesSorted$percWhole   <- journalTitlesSorted$titleCount / journalTitlesSorted$cumsum[973]
journalTitlesSorted$cumper      <- cumsum(journalTitlesSorted$percWhole)

# pareto.chart(journalTitlesSorted$titleCount, cumperc = seq(0, 100, by = 25), ylab2 = "finer")

# pareto plot
# x <- sort(journalTitlesSorted$titleCount, decreasing = TRUE)
# x1 <- cumsum(x) / length(x)
# plot(cumsum(x) / length(x), type = "l")
# qplot(x, x1, geom = "smooth")

# pareto plot JISTAP
# p <- ggplot(journalTitlesSorted, aes(x = titleCount, y = cumper))
# p + geom_point() +
#   xlab("Count of Total Unique Totals") +
#   ylab("Percentage")

journalTitlesSorted$rank <- rank(journalTitlesSorted$cumsum)

# Bradford zones
journalTitlesSorted$rankfactor <- cut_interval(journalTitlesSorted$cumper,
                                               labels = c('Core', 'Middle',
                                                          'Tail'), n = 3)
journalTitlesSorted$rankfactor <- ordered(journalTitlesSorted$rankfactor,
                                          levels = c("Core", "Middle",
                                                     "Tail"))

# Brute force, k = 2253/973 = ~2.3, p (partitions) = 4
# 1 : n : n^2 : n^3
# 47 : 47 * 2.3 : 47 * 2.3^2 : 47 * 2.3^3 = 975 (subtract 2 to get 973)
journalTitlesSorted$rankfactor <- c(rep('Zone 1', 47), rep('Zone 2', 108),
                                    rep('Zone 3', 248), rep('Zone 4', 570))
journalTitlesSorted$rankfactor <- ordered(journalTitlesSorted$rankfactor,
                                          levels = c("Zone 1", "Zone 2",
                                                     "Zone 3", "Zone 4"))

table(journalTitlesSorted$rankfactor)
# x,y coordinates on plots below
cbind(table(journalTitlesSorted$rankfactor),
      summarise(group_by(journalTitlesSorted, rankfactor), max(cumper)))

cbind(table(journalTitlesSorted$rankfactor),
      summarise(group_by(journalTitlesSorted, rankfactor), sum(titleCount)))

cbind(table(journalTitlesSorted$rankfactor),
      summarise(group_by(journalTitlesSorted, rankfactor), max(cumsum)))

summarise(group_by(journalTitlesSorted, rankfactor), min(cumper))


# # http://people.lis.illinois.edu/~jdownie/biblio/bradford.html
# b <- table(journalTitlesSorted$titleCount)
# b <- as.matrix(b)
# b <- as.data.frame(b)
# noOfArticles <- row.names(b)
# b$noOfArticles <- noOfArticles
# names(b) <- c("noOfJournals", "noOfArticles")
# b$noOfJournals <- as.numeric(b$noOfJournals)
# b$noOfArticles <- as.numeric(b$noOfArticles)
# b$nJ <- sort(b$noOfJournals, decreasing = FALSE)
# b$nA <- sort(b$noOfArticles, decreasing = TRUE)
# b$noOfJournals <- NULL
# b$noOfArticles <- NULL
# b$ja <- b$nJ * b$nA
# b$cores <- cut_interval(b$nA, labels = c('Tail', 'Middle', 'Core'), n = 3)
# b$rank <- rank(b$nJ, ties.method = "first")
# summarise(group_by(b, cores), max(ja))
# s <- ggplot(b, aes(x = log(rank),
#                    y = ja,
#                    colour = cores))
# s + geom_point(size = 5) +
#   theme_light() +
#   xlab("Journal Title Rank (log scale)") +
#   ylab("Total Article Titles") +
#   scale_color_brewer(name = "Bradford Zones", type = "seq", palette = 7) +
#   theme(axis.text.x = element_text(hjust = 1, size = 12, colour = "black")) +
#   theme(axis.text.y = element_text(hjust = 1, size = 12, colour = "black"))

p <- ggplot(journalTitlesSorted, aes(x = rank,
                                     y = (cumper * 100),
                                     colour = rankfactor))
p + geom_point(size = 5) +
  theme_light() +
  xlab("Journal Title Rank") +
  ylab("Cumulative Percentage of Total Journal Titles") +
  scale_color_brewer(name = "Bradford Zones", type = "seq", palette = 7) +
#   annotate("segment", x = 0, xend = 0, y = 1, yend = 100, colour = "blue") +
  annotate("text", x = 130, y = 31.8, label = "x = 47, y = 31.8%") +
#   annotate("segment", x = 47, xend = 47, y = 1, yend = 100, colour = "blue") +
  annotate("text", x = 250, y = 52.4, label = "x = 155, y = 52.4%") +
#   annotate("segment", x = 155, xend = 155, y = 1, yend = 100, colour = "blue") +
  annotate("text", x = 480, y = 72, label = "x = 403, y = 74.7%") +
#   annotate("segment", x = 403, xend = 403, y = 1, yend = 100, colour = "blue") +
  annotate("text", x = 920, y = 105, label = "x = 973, y = 100%") +
#   annotate("segment", x = 973, xend = 973, y = 1, yend = 100, colour = "blue") +
  theme(axis.text.x = element_text(hjust = 1, size = 12, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, size = 12, colour = "black"))

p <- ggplot(journalTitlesSorted, aes(x = rank,
                                     y = (cumper * 100)))

p + geom_line() +
  theme_light() +
  xlab("Journal title rank") +
  ylab("Percentage of articles (cumulative)") +
  theme(axis.text.x = element_text(hjust = 1, size = 12, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, size = 12, colour = "black"))

q <- ggplot(journalTitlesSorted, aes(x = rank,
                                     y = (cumper * 100),
                                     shape = rankfactor))
q + geom_point(size = 5) +
  theme_light() +
  xlab("Journal Title Rank (log scale)") +
  ylab("Cumulative Sum of Total Journal Titles") +
  scale_shape_discrete(name = "Bradford Zones", solid = FALSE)


# summarise(group_by(journalTitlesSorted, rankfactor), min(cumsum))
# summarise(group_by(journalTitlesSorted, rankfactor), max(cumsum))
summarise(group_by(journalTitlesSorted, rankfactor), sum(titleCount))

# the first rank group of titles (n = 3) account for 149 references
# the second rank group of titles (n = 5) account for 147 references
# the third rank group of titles (n = 1957) account for 965 references

# Out of the 973 unique journal titles, around 63.10% (n = 614) of the journal
# titles appear once and account for 63.10% (n = 614) of the total unique
# titles, around 15.72% (n = 153) appear twice and account for 78.82% (n = 767)
# of the total unique titles, around 7.71% (n = 75) appear 3 times and account
# for 86.53% (n = 842) of the total unique titles, etc.
length(journalTitlesSorted$titleCount)
jtDist <- cbind(table(journalTitlesSorted$titleCount),
                round(table(journalTitlesSorted$titleCount) /
                        sum(table(journalTitlesSorted$titleCount)), 4),
                cumsum(round(table(journalTitlesSorted$titleCount) /
                               sum(table(journalTitlesSorted$titleCount)), 4)),
                cumsum(table(journalTitlesSorted$titleCount))
)
jtDist           <- as.data.frame(as.matrix(jtDist))
jtDist$count     <- rownames(jtDist)
rownames(jtDist) <- NULL
write.table(jtDist, file="data-processed/jtDist.csv")

---

### ESTEEM (SJR / SNIP), ARCHIVE POLICY, OA STATUS
 
# Table 11
# Table 11 is derived from Table 10. The most cited titles appear 10 or more times.

# Function to show how many of articles have a title; use the following length
# in the function to count. Note, this doesn't work for journals like "Nature",
# "Science", and "Ecology" because these terms appear in the names of other
# journals. 
titleCount <- function(x) {
  a <- grep(x, citedJournals$titles)
  print(table(as.character(citedJournals$doi[a])))
}

## proceed through each titles in Table 10 that appears 10 or more times
sum(titleCount("plos one"))
length(titleCount("plos one"))

## Do the above manually for each of the problem titles
a <- grep("^nature$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

a <- grep("^science$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

a <- grep("^rna$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

a <- grep("^ecology$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

a <- grep("^cell$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

a <- grep("^molecular cell$", citedJournals$titles)
print(table(as.character(citedJournals$doi[a])))
sum(table(as.character(citedJournals$doi[a])))
length(print(table(as.character(citedJournals$doi[a]))))

# Average number of journal titles per article and unique journal titles per
# article; can confirm xy output with the following two lines:
length(citedJournals$titles[citedJournals$doi == "10.7717/peerj.1"])
length(unique(citedJournals$titles[citedJournals$doi == "10.7717/peerj.1"]))

jtCount         <- tapply(citedJournals$titles, citedJournals$doi, FUN = length)
uniqueList      <- tapply(citedJournals$titles, citedJournals$doi, FUN = unique)
jtUniq          <- lapply(uniqueList, FUN = length) # Unique by PeerJ article
jtDF            <- as.data.frame(cbind(jtCount,jtUniq))
jtDF$jtCount    <- as.numeric(jtDF$jtCount)
jtDF$jtUniq     <- as.numeric(jtDF$jtUniq)
jtDF$doi        <- row.names(jtDF)
row.names(jtDF) <- NULL
summary(jtDF)
rm(jtCount, jtUniq, uniqueList)
rm(jtDF)

# Average number of unique journal titles per PeerJ article, for the whole set
length(unique(citedJournals$titles)) / length(unique(citedJournals$doi))

cjt <- table(citedJournals$doi, citedJournals$titles)
cjt <- as.data.frame(cjt)
cjt <- filter(cjt, Freq > 0)
cjt <- arrange(cjt, Var1)
cjt_1 <- table(cjt$Var2)
cjt_1 <- as.data.frame(cjt_1)
abc <- cbind(journalTitlesSorted$titleCount, journalTitlesSorted$titles)
abc <- as.data.frame(abc)
names(abc) <- c("count", "titles")
names(cjt_1) <- c("titles", "articles")
def <- full_join(abc, cjt_1)
rm(cjt, cjt_1, abc)
def$count <- as.integer(as.character(def$count))
def$prop <- def$articles / def$count
def$index <- 1:length(def$count)

fed <- filter(def, articles > 1)
fed$index <- NULL
fed$index <- 1:length(fed$count)

fedtest <- filter(fed, prop < 1)
fedtest$index <- NULL
fedtest$index <- 1:length(fedtest$count)

# assign true / false for all obs w/ more than one article
def$singletons <- def$articles > 1
# assign true / false for all proportions less than one
def$prop1 <- def$prop < 1
def$prop2 <- def$singletons == TRUE & def$prop1 == TRUE

# the lower the rank (x -> 200), i.e., the less frequently cited the journal title,
# the more concentrated the journal titles
# the higher the rank (x -> 1), i.e., the more frequently cited the journal title,
# the more scattered the journal titles

# scatter means that many authors are relying on the same set of journal titles
# but the long tail here, the concentration metric, indicates that PeerJ authors
# are drawing from an incredible number of sources and only relying on a few major
# sources, e.g., PLOS ONE, Nature, Science, PNAS, relatively infrequently
# this suggests that PeerJ is behaving incohesively -- that it's drawing authors
# who have a diverse set of specialties; and thus it's not acting under a unified
# direction, like a more specialized scientific journal might (e.g., such as one on 
# pediatrics); this further adds evidence of the article-centric nature of
# this megajournal
# this means that peerj won't ever be knowing as a reading journal; instead
# it will be highly dependent on search engines to retrieve articles based on
# user keyword searches

# Figure 1: plots all data points
p <- ggplot(def, aes(x = index,
                     y = prop))

p + geom_point() +
  geom_smooth(method = "loess") +
  theme_light() +
  xlab("Journal Title Rank") +
  ylab("Cited Title Density") +
  theme(axis.text.x = element_text(hjust = 1, size = 13, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, size = 13, colour = "black"))

# # facets two plots: one w/ singletons and one w/o out
# p + geom_point() +
#   geom_smooth(method = "loess") +
#   theme_light() +
#   facet_grid(singletons ~ .) +
#   xlab("Journal Title Rank") +
#   ylab("Cited Title Density") +
#   theme(axis.text.x = element_text(hjust = 1, size = 13, colour = "black")) +
#   theme(axis.text.y = element_text(hjust = 1, size = 13, colour = "black"))
# 
# # facets two plots: one w/ prop = 1 and one w/ no prop
# p + geom_point() +
#   geom_smooth(method = "loess") +
#   theme_light() +
#   facet_grid(prop1 ~ .) +
#   xlab("Journal Title Rank") +
#   ylab("Cited Title Density") +
#   theme(axis.text.x = element_text(hjust = 1, size = 13, colour = "black")) +
#   theme(axis.text.y = element_text(hjust = 1, size = 13, colour = "black"))

# Figure 2: Include all articles that appear more than one time
q <- ggplot(fed, aes(x = index,
                     y = prop))

q + geom_point() +
  geom_smooth(method = "loess") +
  theme_light() +
  xlab("Journal Title Rank") +
  ylab("Cited Title Density") +
  theme(axis.text.x = element_text(hjust = 1, size = 13, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, size = 13, colour = "black"))

# Figure 3: Include all articles that appear more than one time and that have a proportion less than one

r <- ggplot(fedtest, aes(x = index,
                         y = prop))

r + geom_point() +
  geom_smooth(method = "loess") +
  theme_light() +
  xlab("Journal Title Rank") +
  ylab("Cited Title Density") +
  theme(axis.text.x = element_text(hjust = 1, size = 13, colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1, size = 13, colour = "black"))

# Example of fitting loess line with confidence intervals
fit.1 <- loess(prop ~ index, data = fed)
fit.2 <- predict(fit.1, data.frame(index = seq(1, 198, 1)), se = TRUE)
plot(fit.2$fit, type = "l")
lines(fed$index, fit.2$fit - qt(0.975, fit.2$df) * fit.2$se, lty = 2)
lines(fed$index, fit.2$fit + qt(0.975, fit.2$df) * fit.2$se, lty = 2)

---

## CITING SOURCES

# counts of Archive Policy and OA Status for citing journals
length(unique(citingJournals2$title)) # number of unique titles
cj2 <- unique(citingJournals2)

table(cj2$ArchivePolicy)
is.na(cj2$ArchivePolicy)
table(is.na(cj2$ArchivePolicy))

table(cj2$OAStatus)
is.na(cj2$OAStatus)
table(is.na(cj2$OAStatus))

table(cj2$ArchivePolicy, cj2$OAStatus)
apoa <- table(cj2$ArchivePolicy, cj2$OAStatus)

CrossTable(apoa, fisher = TRUE, chisq = TRUE, expected = TRUE,
           prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE,
           sresid = TRUE, format = "SPSS")
rm(apoa)

tmpcj2 <- cj2
tmpcj2$ArchivePolicy <- gsub("Green", "OAA", tmpcj2$ArchivePolicy)
tmpcj2$ArchivePolicy <- gsub("Blue", "OAA", tmpcj2$ArchivePolicy)
tmpcj2$ArchivePolicy <- gsub("Yellow", "OAA", tmpcj2$ArchivePolicy)
tmpcj2$ArchivePolicy <- gsub("White", "NOOAA", tmpcj2$ArchivePolicy)

tmpcj2$OAStatus <- gsub("DOAJ", "OAP", tmpcj2$OAStatus)
tmpcj2$OAStatus <- gsub("Paid OA", "OAP", tmpcj2$OAStatus)
tmpcj2$OAStatus <- gsub("No-paid OA", "NOOAP", tmpcj2$OAStatus)

tmpcj2 <- tmpcj2[!tmpcj2$ArchivePolicy == "ungraded",]

apoa2 <- table(tmpcj2$ArchivePolicy, tmpcj2$OAStatus)

CrossTable(apoa2, fisher = TRUE, chisq = TRUE, expected = TRUE,
           prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE,
           sresid = TRUE, format = "SPSS")
rm(tmpcj2, cj2, apoa2)

## For Table 12, No. Citing PeerJ Articles column
## Search journal titles with 2 or more in the Count of Instances column
a <- sapply(citingJournals1[4:13], match, "PLoS ONE",  nomatch = 0)
rowSums(a)

stat.desc(cbind(citingJournals2$SNIP2012, citingJournals2$SJR2012))

cj2      <- unique(citingJournals2)
snip2012 <- na.omit(cj2$SNIP2012)
sjr2012  <- na.omit(cj2$SJR2012)
stat.desc(cbind(snip2012, sjr2012))
stat.desc(cbind(snip2012, cj2$SNIP2012, sjr2012, cj2$SJR2012))
rm(snip2012, sjr2012)




## Count archival policy per unique journal
aplist <- tapply(citingJournals2$title, citingJournals2$ArchivePolicy, table)
as.matrix(aplist) # does not include NAs

oalist <- tapply(citingJournals2$title, citingJournals2$OAStatus, table)
as.matrix(oalist) # does not include NAs


citingJournals1$doi[citingJournals1$ScopusMay >= 1]
table(citingJournals1$doi[citingJournals1$ScopusMay >= 1])
ct2 <- unique(citingJournals2)
write.table(ct2, file = "data-processed/table12.csv",
            quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)

## Usage: doi2title(citingJournals1$doi, citingJournals1$title1)
## Repeat for each title column: citingJournals$title2, title3, etc.
doi2title <- function(x, y) {
  d2t <- table(x,y)
  d2t <- as.data.frame(d2t)
  d2t[d2t$Freq > 0,]
}

### JOURNAL TITLES
### count of citations
sum(citingJournals1$ScopusMay)
sum(citingJournals1$ScopusAug14)
### count of unique citing journal titles
unique(citingJournals2$title)

ctj1 <- melt(citingJournals1, id = c("doi", "ScopusMay", "ScopusAug14"))
ctj1 <- na.omit(ctj1)
ctj1$doi <- as.character(ctj1$doi)

### BOOK TITLES

## SNIP and SJR

x <- distinct(citingJournals2)
stat.desc(x$SNIP2012, norm = FALSE)
stat.desc(x$SJR2012, norm = FALSE)

stat.desc(table7$SNIP.2012, norm = FALSE)
stat.desc(table7$SJR.2012, norm = FALSE)

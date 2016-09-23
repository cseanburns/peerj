summary(as.numeric((peerjMetadata$DataCol5 - peerjMetadata$PubDate)))

dc4topd <- factor(quantile(as.numeric((peerjMetadata$DataCol5 - peerjMetadata$PubDate))))

xyplot(peerjMetadata$DownloadsMay ~ peerjMetadata$AuthCount | dc4topd,
       xlab = "Author Count",
       ylab = "Fill in the Blank")

xyplot(peerjMetadata$DownloadsMay ~ peerjMetadata$AuthCount | 
         quantile(as.numeric(peerjMetadata$DataCol5 - peerjMetadata$PubDate)),
       xlab = "Author Count",
       ylab = "Fill in the Blank")


qOfDates <- as.factor(quantile(as.numeric((peerjMetadata$DataCol5 - peerjMetadata$PubDate))))
p <- ggplot(peerjMetadata, aes(x = peerjMetadata$AuthCount, y = peerjMetadata$TopRefTotalMay))
p + geom_point(aes(shape = factor(peerjMetadata$MultiNational)))
p + geom_point(aes(shape = factor(peerjMetadata$Gender)))
p + geom_point(aes(shape = factor(peerjMetadata$FemaleFirst)))
p + geom_point(aes(shape = factor(peerjMetadata$PeerAnon)))
p + geom_point(aes(shape = factor(peerjMetadata$NoRevisions)))
p + geom_point(aes(shape = factor(peerjMetadata$PeerRevAvail)))

## Create pandoc table unique social references in Aug to total social 
## references in Aug
panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Referrals to Total Referrals')
pandoc.table(stat.desc(peerjMetadataSubsettedYes[,c(24,28)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Visitors Per Month')
pandoc.table(stat.desc(peerjMetadata[,c(18, 22, 26, 30, 34, 38)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Referrals to Total Referrals')
pandoc.table(stat.desc(peerjMetadata[,c(24,28)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

# Create pandoc table unique social references in Nov to total social references
# in Nov
panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Referrals to Total Referrals')
pandoc.table(stat.desc(peerjMetadataSubsettedYes[,c(25,29, 33, 37, 41, 45)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

# Create pandoc table unique social references in Feb to total social references
# in Feb
panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Referrals to Total Referrals')
pandoc.table(stat.desc(peerjMetadataSubsettedYes[,c(26,30)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

# Create pandoc table unique social references in May to total social references
# in May
panderOptions('table.split.table', Inf)
set.caption('Descriptive Statistics: Unique Referrals to Total Referrals')
pandoc.table(stat.desc(peerjMetadataSubsettedYes[,c(27,31)]),
             basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)

### Relationship between Authors and Impact
## Is there a relationship between author count and unique number of visitors?
cor(peerjMetadata$AuthCount, peerjMetadata$UniqueVisitors.Aug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$UniqueVisitors.Nov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$UniqueVisitors.Feb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$UniqueVisitors.May, method = "kendall")

## Exploratory using lattice
xyplot(log(peerjMetadata$UniqueVisitorsAug + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$UniqueVisitorsAug + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$UniqueVisitorsNov + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$UniqueVisitorsNov + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$UniqueVisitorsFeb + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$UniqueVisitorsFeb + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$UniqueVisitorsMay + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$UniqueVisitorsMay + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)


## Is there a relationship between author count and unique number of pageviews?
cor(peerjMetadata$AuthCount, peerjMetadata$PageviewsAug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$PageviewsNov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$PageviewsFeb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$PageviewsMay, method = "kendall")

xyplot(log(peerjMetadata$PageviewsAug + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$PageviewsAug + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$PageviewsNov + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$PageviewsNov + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$PageviewsFeb + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$PageviewsFeb + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

xyplot(log(peerjMetadata$PageviewsMay + 1) ~ peerjMetadata$AuthCount)
xyplot(log(peerjMetadata$PageviewsMay + 1) ~
         peerjMetadata$AuthCount | peerjMetadata$MultiNational)

## Is there a relationship between author count and unique count of social referrals?
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefUniqAug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefUniqNov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefUniqFeb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefUniqMay, method = "kendall")

## Is there a relationship between author count and unique count of web referrals?
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefUniqAug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefUniqNov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefUniqFeb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefUniqMay, method = "kendall")

## Is there a relationship between author count and total count of social referrals?
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefTotalAug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefTotalNov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefTotalFeb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$SocRefTotalMay, method = "kendall")

## Is there a relationship between author count and total count of web referrals?
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefTotalAug, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefTotalNov, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefTotalFeb, method = "kendall")
cor(peerjMetadata$AuthCount, peerjMetadata$TopRefTotalMay, method = "kendall")

## Are multinational contributions more likely
## to have a higher number of unique visitors?
# Up to August 2013
# calculate n, Mdn, and M
tapply(peerjMetadata$UniqueVisitorsAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$UniqueVisitorsAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$UniqueVisitorsAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$UniqueVisitorsNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$UniqueVisitorsNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$UniqueVisitorsNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$UniqueVisitorsFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$UniqueVisitorsFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$UniqueVisitorsFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$UniqueVisitorsMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$UniqueVisitorsMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$UniqueVisitorsMay, peerjMetadata$MultiNational, FUN = mean)

## Are multinational contributions more likely
## to have a higher number of unique pageviews?
# Up to August 2013
tapply(peerjMetadata$PageviewsAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$PageviewsAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$PageviewsAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$PageviewsNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$PageviewsNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$PageviewsNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$PageviewsFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$PageviewsFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$PageviewsFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$PageviewsMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$PageviewsMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$PageviewsMay, peerjMetadata$MultiNational, FUN = mean)

## Are multinational contributions more likely
## to have higher unique counts of social referrals?
# Up to August 2013
tapply(peerjMetadata$SocRefUniqAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefUniqAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefUniqAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$SocRefUniqNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefUniqNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefUniqNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$SocRefUniqFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefUniqFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefUniqFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$SocRefUniqMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefUniqMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefUniqMay, peerjMetadata$MultiNational, FUN = mean)

## Are multinational contributions more likely
## to have higher unique counts of web referrals?
# Up to August 2013
tapply(peerjMetadata$TopRefUniqAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefUniqAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefUniqAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$TopRefUniqNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefUniqNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefUniqNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$TopRefUniqFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefUniqFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefUniqFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$TopRefUniqMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefUniqMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefUniqMay, peerjMetadata$MultiNational, FUN = mean)

## Are multinational contributions more likely
## to have higher total counts of social referrals?
# Up to August 2013
tapply(peerjMetadata$SocRefTotalAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefTotalAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefTotalAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$SocRefTotalNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefTotalNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefTotalNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$SocRefTotalFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefTotalFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefTotalFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$SocRefTotalMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$SocRefTotalMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$SocRefTotalMay, peerjMetadata$MultiNational, FUN = mean)

## Are multinational contributions more likely
## to have higher total counts of web referrals?
# Up to August 2013
tapply(peerjMetadata$TopRefTotalAug, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefTotalAug, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefTotalAug, peerjMetadata$MultiNational, FUN = mean)

# Up to November 2013
tapply(peerjMetadata$TopRefTotalNov, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefTotalNov, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefTotalNov, peerjMetadata$MultiNational, FUN = mean)

# Up to February 2014
tapply(peerjMetadata$TopRefTotalFeb, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefTotalFeb, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefTotalFeb, peerjMetadata$MultiNational, FUN = mean)

# Up to May 2014
tapply(peerjMetadata$TopRefTotalMay, peerjMetadata$MultiNational, FUN = length)
tapply(peerjMetadata$TopRefTotalMay, peerjMetadata$MultiNational, FUN = median)
tapply(peerjMetadata$TopRefTotalMay, peerjMetadata$MultiNational, FUN = mean)

## Note about above: social referrals drop / stagnate but web
## referrals continue to increase.

### Manuscript characteristics

## If papers needed major revisions after the first round of
## reviews, are they more likely to have less impact than
## papers that needed minor revisions?

# Count of articles with Minor Revisions needed
table(peerjMetadata$RevStanding1 == "Minor")
# Count of articles with Major Revisions needed
table(peerjMetadata$RevStanding1 == "Major")

# E.g.: 

xyplot(log(peerjMetadata$SocRefTotalAug + 1)
       ~ peerjMetadata$AuthCount | peerjMetadata$RevStanding1)

xyplot(log(peerjMetadata$TopRefTotalAug + 1)
       ~ peerjMetadata$AuthCount | peerjMetadata$RevStanding1)

xyplot(log(peerjMetadata$TopRefUniqAug + 1)
       ~ peerjMetadata$AuthCount | peerjMetadata$RevStanding1)

xyplot(log(peerjMetadata$PageviewsAug + 1)
       ~ peerjMetadata$AuthCount | peerjMetadata$RevStanding1)


# With regards to total social referrals?
# Up to August 2013
tapply(peerjMetadata$SocRefTotalAug,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$SocRefTotalAug,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$SocRefTotalAug,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$SocRefTotalAug,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to November 2013
tapply(peerjMetadata$SocRefTotalNov,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$SocRefTotalNov,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$SocRefTotalNov,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$SocRefTotalNov,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to February 2014
tapply(peerjMetadata$SocRefTotalFeb,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$SocRefTotalFeb,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$SocRefTotalFeb,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$SocRefTotalFeb,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to May 2014
tapply(peerjMetadata$SocRefTotalMay,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$SocRefTotalMay,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$SocRefTotalMay,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$SocRefTotalMay,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# With regards to total web referrals?
# Up to August 2013
tapply(peerjMetadata$TopRefTotalAug,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$TopRefTotalAug,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$TopRefTotalAug,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$TopRefTotalAug,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to November 2013
tapply(peerjMetadata$TopRefTotalNov,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$TopRefTotalNov,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$TopRefTotalNov,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$TopRefTotalNov,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to February 2014
tapply(peerjMetadata$TopRefTotalFeb,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$TopRefTotalFeb,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$TopRefTotalFeb,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$TopRefTotalFeb,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

# Up to May 2014
tapply(peerjMetadata$TopRefTotalMay,
       peerjMetadata$RevStanding1 == "Minor", FUN = median)
tapply(peerjMetadata$TopRefTotalMay,
       peerjMetadata$RevStanding1 == "Minor", FUN = mean)
tapply(peerjMetadata$TopRefTotalMay,
       peerjMetadata$RevStanding1 == "Major", FUN = median)
tapply(peerjMetadata$TopRefTotalMay,
       peerjMetadata$RevStanding1 == "Major", FUN = mean)

## Relationship between number of channels of
## dissemination to total dissemination units
# Web referrals
cor(peerjMetadata$TopRefUniqAug, peerjMetadata$TopRefTotalAug)
plot(log(peerjMetadata$TopRefUniqAug),
     log(peerjMetadata$TopRefTotalAug))
cor(peerjMetadata$TopRefUniqNov, peerjMetadata$TopRefTotalNov)
plot(log(peerjMetadata$TopRefUniqNov),
     log(peerjMetadata$TopRefTotalNov))
cor(peerjMetadata$TopRefUniqFeb, peerjMetadata$TopRefTotalFeb)
plot(log(peerjMetadata$TopRefUniqFeb),
     log(peerjMetadata$TopRefTotalFeb))
cor(peerjMetadata$TopRefUniqMay, peerjMetadata$TopRefTotalMay)
plot(log(peerjMetadata$TopRefUniqMay),
     log(peerjMetadata$TopRefTotalMay))

xyplot(log(peerjMetadata$TopRefTotalAug) ~
         log(peerjMetadata$TopRefUniqAug))
panel.abline(reg = (log(peerjMetadata$TopRefTotalAug) ~
                      log(peerjMetadata$TopRefUniqAug)))

# Social referrals
cor(peerjMetadata$SocRefUniqAug, peerjMetadata$SocRefTotalAug, method="spearman")
plot(log(peerjMetadata$SocRefUniqAug),
     log(peerjMetadata$SocRefTotalAug))
cor(peerjMetadata$SocRefUniqNov, peerjMetadata$SocRefTotalNov, method = "spearman")
plot(log(peerjMetadata$SocRefUniqNov),
     log(peerjMetadata$SocRefTotalNov))
cor(peerjMetadata$SocRefUniqFeb, peerjMetadata$SocRefTotalFeb, method = "spearman")
plot(log(peerjMetadata$SocRefUniqFeb),
     log(peerjMetadata$SocRefTotalFeb))
cor(peerjMetadata$SocRefUniqMay, peerjMetadata$SocRefTotalMay, method = "spearman")
plot(log(peerjMetadata$SocRefUniqMay),
     log(peerjMetadata$SocRefTotalMay))

# Date analysis
qplot(peerjMetadata$PubDate,
      log(peerjMetadata$TopRefTotalAug)) + ylim(0,10)
qplot(peerjMetadata$PubDate,
      log(peerjMetadata$TopRefTotalNov)) + ylim(0,10)
qplot(peerjMetadata$PubDate,
      log(peerjMetadata$TopRefTotalFeb)) + ylim(0,10)
qplot(peerjMetadata$PubDate,
      log(peerjMetadata$TopRefTotalMay)) + ylim(0,10)

qplot(peerjMetadata$PubDate, peerjMetadata$ScopusFeb)
qplot(peerjMetadata$PubDate, peerjMetadata$GSFeb)
qplot(peerjMetadata$PubDate, peerjMetadata$ScopusMay)
qplot(peerjMetadata$PubDate, peerjMetadata$GSMay)

# Relationship between Total Web Referrals & Citation Counts
cor(peerjMetadata$TopRefTotalFeb,
    peerjMetadata$GSFeb, method = "spearman")
cor(peerjMetadata$TopRefTotalFeb,
    peerjMetadata$ScopusFeb, method = "spearman")

cor(peerjMetadata$TopRefTotalMay,
    peerjMetadata$GSMay, method = "spearman")
cor(peerjMetadata$TopRefTotalMay,
    peerjMetadata$ScopusMay, method = "spearman")

# 3D Scatterplots
library(scatterplot3d)
accept2pubDays   <- peerjMetadata$PubDate - peerjMetadata$AcceptDate
recvd2pubDays    <- peerjMetadata$PubDate - peerjMetadata$RecdDate
recvd2acceptDays <- peerjMetadata$AcceptDate - peerjMetadata$RecdDate
r2pub <- as.numeric(recvd2pubDays)    # received date to publication date
r2acc <- as.numeric(recvd2acceptDays) # received date to accept date
a2pub <- as.numeric(accept2pubDays)   # accept date to publication date

scatterplot3d(r2pub,
              peerjMetadata$PubDate,
              peerjMetadata$GSFeb)
scatterplot3d(a2pub,
              r2pub,
              peerjMetadata$GSFeb)
scatterplot3d(r2acc,
              peerjMetadata$TopRefTotalFeb,
              peerjMetadata$GSFeb,
              highlight.3d=TRUE, box=FALSE)

scatterplot3d(r2pub,
              peerjMetadata$PubDate,
              peerjMetadata$GSMay)
scatterplot3d(a2pub,
              r2pub,
              peerjMetadata$GSMay)
scatterplot3d(r2acc,
              peerjMetadata$TopRefTotalMay,
              peerjMetadata$GSMay, highlight.3d=TRUE, box=FALSE)

fit.1 <- glm(peerjMetadata$GSFeb ~ r2acc +
               peerjMetadata$TopRefTotalFeb)
summary(fit.1)

scatterplot3d(peerjMetadata$r2acc, peerjMetadata$TopRefTotalFeb,
              peerjMetadata$GSFeb, highlight.3d=TRUE,
              box=FALSE)$plane3d(fit.1)

fit.2 <- glm(peerjMetadata$GSMay ~ peerjMetadata$r2acc +
               peerjMetadata$TopRefTotalMay)
summary(fit.2)

scatterplot3d(peerjMetadata$r2acc, peerjMetadata$TopRefTotalMay,
              peerjMetadata$GSMay, highlight.3d=TRUE,
              box=FALSE)$plane3d(fit.1)

rm(accept2pubDays, recvd2pubDays, recvd2acceptDays, r2pub, r2acc, a2pub)

# Update these to look at the percentage change for each date collected:
# Aug to Nov; Nov to Feb; Feb to May; and total, as below: Aug to May:
perChange(peerjMetadata$TopRefTotalMay,
          peerjMetadata$TopRefTotalAug)
summary(perChange(peerjMetadata$TopRefTotalMay,
                  peerjMetadata$TopRefTotalAug))
pcA2F <- perChange(peerjMetadata$TopRefTotalMay,
                   peerjMetadata$TopRefTotalAug)

# Compare percentage change to publication date. Newer
# publications show great percentage changes simply because 
# they are newer. In short, as you read the plot form RIGHT to
# LEFT, it becomes apparant that the percentage changes approaches
# zero in the total web referrals it receives as the manuscript
# ages.

qplot(peerjMetadata$PubDate, pcA2F)
qplot(peerjMetadata$PubDate, pcA2F, geom = "smooth")
qplot(peerjMetadata$PubDate, pcA2F, geom = "jitter")
qplot(peerjMetadata$PubDate, pcA2F, geom = "quantile")

## Regressions
# Log transformations
peerjMetadata$logTRTAug <- log(peerjMetadata$TopRefTotalAug)
peerjMetadata$logTRTNov <- log(peerjMetadata$TopRefTotalNov)
peerjMetadata$logSRTAug <- log(peerjMetadata$SocRefTotalAug + 1)
peerjMetadata$logSRTNov <- log(peerjMetadata$SocRefTotalNov + 1)

## Table -2- in pj-tables.odt !!!!I REMOVED THIS TABLE!!!! KEEP AS EXAMPLE
ct1 <- xtabs(peerjMetadata$AuthCount ~
                  peerjMetadata$MultiNational + peerjMetadata$PeerRevAvail)
ct1
chisq.test(ct1)

### Tables 6,7,8 in pj-tables.odt !!!!!These all removed from the paper!!!!!
### peerjMetadataSubsettedNo$UniqueVisitorsAug
### peerjMetadataSubsettedYes$UniqueVisitorsAug
### and so on for UniqueVisitors, Pageviews, SocRefUniq
### Usage: socialStats(peerjMetadataSubsettedNo$PageviewsAug)

socialStats <- function(x) {
  round(stat.desc(x), 2)
}


# example plotting correlation matrix
library("ellipse")
a <- data.frame(peerjMetadata$UniqueVisitorsAug, peerjMetadata$PageviewsAug,
         peerjMetadata$SocRefUniqAug, peerjMetadata$SocRefTotalAug,
         peerjMetadata$TopRefUniqAug, peerjMetadata$TopRefTotalAug)
acor <- cor(a)
plotcorr(acor)

library("ggplot2")
library("reshape2")
library("lubridate")
library("extrafont")
library("extrafontdb")

# Figure 1 (15748 = launch date of PeerJ, days since 1970-01-01)
p <- ggplot(peerjMetadata,
            aes(RecdDate, PeerRevAvail))
#postscript("fig1.ps")
p + geom_point() +
  geom_vline(xintercept = 15748) + #theme_classic() +
  xlab("Date of Manuscript Submission") +
  ylab("Publicly Available Peer Review History") +
  ggtitle("Tendency of Authors to Restrict Peer Review History\nVertical Line Indicates Launch Date")
# embed_fonts("fig1.ps", outfile = "fig1.eps", options="-dEPSCrop")
# dev.off()

# Figure 2
apm1 <- cbind(log(peerjMetadata$DownloadsAug14),
              log(peerjMetadata$UniqueVisitorsAug14),
              log(peerjMetadata$PageviewsAug14))
apm1 <- as.data.frame(apm1)
apm1$PubDate <- ymd(peerjMetadata$PubDate)
apm1 <- melt(apm1, id = "PubDate")

p <- ggplot(apm1, aes(x = PubDate, y = value, linetype = variable))
# postscript("fig2.ps")
p + geom_line() + #theme_classic() +
  labs(linetype = "Metric:") +
  theme(legend.position = "top") +
  xlab("Publication Date") +
  ylab("Counts (log)") +
  scale_linetype( breaks = c("V1", "V2", "V3"),
    labels = c("Downloads", "Unique Visitors", "Page Views"))
# embed_fonts("fig2.ps", outfile = "fig2.eps", options="-dEPSCrop")
# dev.off()

# Figure 3
apm2 <- cbind(log(peerjMetadata$SocRefUniqAug14 + 1),
              log(peerjMetadata$SocRefTotalAug14 + 1),
              log(peerjMetadata$TopRefUniqAug14 + 1),
              log(peerjMetadata$TopRefTotalAug14 + 1))
apm2 <- as.data.frame(apm2)
apm2$PubDate <- ymd(peerjMetadata$PubDate)

apm2 <- melt(apm2, id = "PubDate")
p <- ggplot(apm2, aes(x = PubDate, y = value, linetype = variable))
# postscript("fig3.ps")
p + geom_line() + #theme_classic() +
  labs(linetype = "Metric:") +
  theme(legend.position = "top") +
  xlab("Publication Date") +
  ylab("Counts (log + 1)") +
  scale_linetype(
    breaks = c("V1", "V2", "V3", "V4"),
    labels = c("Unique Social Referrals", "Total Social Referrals",
               "Unique Top Referrals", "Total Top Referrals"))
# embed_fonts("fig3.ps", outfile = "fig3.eps", options="-dEPSCrop")
# dev.off()
  
# Figure 4
apm3 <- cbind(peerjMetadata$GSAug14,
              peerjMetadata$ScopusAug14)
apm3 <- as.data.frame(apm3)
apm3$PubDate <- ymd(peerjMetadata$PubDate)

apm3 <- melt(apm3, id = "PubDate")
p <- ggplot(apm3, aes(x = PubDate, y = value, linetype = variable))
# postscript("fig4.ps")
p + geom_line() + #theme_classic() +
  labs(linetype = "Source:") +
  theme(legend.position = "top") +
  xlab("Publication Date") +
  ylab("Citations") +
  #ggtitle("Kinds of Citations, Cumulative Counts as of 2014-08-20") +
  scale_linetype(
    breaks = c("V1", "V2"),
    labels = c("Google Scholar", "Scopus"))
#+ xlim(ymd(c('2013-02-01','2013-08-01')))
# embed_fonts("fig4.ps", outfile = "fig4.eps", options="-dEPSCrop")
# dev.off()

#### BELOW NOT USED: EXPLORATORY ####
rd <- peerjMetadata$RecdDate[!is.na(peerjMetadata$PeerAnon)]
pa <- peerjMetadata$PeerAnon[!is.na(peerjMetadata$PeerAnon)]
rdpa <- cbind(ymd(rd),
              factor(pa,
                     levels = c(0, 1, 2),
                     labels = c("Anonymous", "Public", "Mixed")))
rdpa <- as.data.frame(rdpa)
ggplot(rdpa, aes(rd, pa)) +
  geom_point() +
  geom_vline(xintercept = 15748) +
  xlab("Manuscript Submission / Received Date") +
  ylab("Peer Review Anonymous") +
  ggtitle("Peer Review Anonymous")

# Figure 2a, 2b, 2c
qplot(peerjMetadata$PubDate, r2acc) +
  xlab("Date of Publication") +
  ylab("Days from Submission to Acceptance") + theme_bw()
qplot(peerjMetadata$PubDate, r2pub) +
  xlab("Date of Publication") +
  ylab("Days from Submission to Publication") + theme_bw()
qplot(peerjMetadata$PubDate, a2pub) +
  xlab("Date of Publication") +
  ylab("Days from Acceptance to Publication") + theme_bw()

## Multiline plot
apm <- cbind(peerjMetadata$DownloadsMay, peerjMetadata$DownloadsAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Downloads") +
  ggtitle("Downloads by Pub Month") +
  scale_shape_discrete(name = "Data Collection",
                          breaks = c("V1", "V2"),
                          labels = c("May", "August"))

apm <- cbind(peerjMetadata$UniqueVisitorsAug,
             peerjMetadata$UniqueVisitorsNov,
             peerjMetadata$UniqueVisitorsFeb,
             peerjMetadata$UniqueVisitorsMay,
             peerjMetadata$UniqueVisitorsAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Visitors, Unique") +
  ggtitle("Visitors by Publication Month")

apm <- cbind(peerjMetadata$PageviewsAug,
             peerjMetadata$PageviewsNov,
             peerjMetadata$PageviewsFeb,
             peerjMetadata$PageviewsMay,
             peerjMetadata$PageviewsAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Page Views") +
  ggtitle("Page Views by Publication Month")

apm <- cbind(peerjMetadata$SocRefUniqAug,
             peerjMetadata$SocRefUniqNov,
             peerjMetadata$SocRefUniqFeb,
             peerjMetadata$SocRefUniqMay,
             peerjMetadata$SocRefUniqAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Unique Social Referrals") +
  ggtitle("Social Referrals by Publication Month")

apm <- cbind(peerjMetadata$SocRefTotalAug,
             peerjMetadata$SocRefTotalNov,
             peerjMetadata$SocRefTotalFeb,
             peerjMetadata$SocRefTotalMay,
             peerjMetadata$SocRefTotalAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Total Social Referrals") +
  ggtitle("Total Social Referrals by Publication Month")

apm <- cbind(peerjMetadata$TopRefUniqAug,
             peerjMetadata$TopRefUniqNov,
             peerjMetadata$TopRefUniqFeb,
             peerjMetadata$TopRefUniqMay,
             peerjMetadata$TopRefUniqAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Total Unique Referrals") +
  ggtitle("Total Unique Referrals by Publication Month")

apm <- cbind(peerjMetadata$TopRefTotalAug,
             peerjMetadata$TopRefTotalNov,
             peerjMetadata$TopRefTotalFeb,
             peerjMetadata$TopRefTotalMay,
             peerjMetadata$TopRefTotalAug14)
apm <- as.data.frame(apm)
apm$PubDate <- ymd(peerjMetadata$PubDate)
apm <- melt(apm, id = "PubDate")
p <- ggplot(apm, aes(x = PubDate, y = value, colour = variable))
p + geom_line() +
  xlab("Publication Date") +
  ylab("Total Referrals") +
  ggtitle("Total Referrals by Publication Month")

---
title: "PeerJ Research 9"
date: 2014-05-20 11:56:51 -0400
---

On August 20, 2013 I collected a round of
data based on a sample of random numbers generated on July 20,
2013.

On November 20, 2013 I collected a follow
up round of data plus some additional data points.

On February 20, 2014, I collected an
additional round of data and added some additional data points.

Today, on May 20, 2014, I collected the final round of data. One additional data
point was added.

Currently, the data is stored in a LibreOffice Calc spreadsheet file to be
exported to a CSV file and imported into R Studio.

I have the following variables:

### Document Variables

* **RandomNum**: The random number originally generated. Corresponds to the DOI
  assigned to *PeerJ*.
* **doi**: The DOI assigned to *PeerJ*. Corresponds to the random number.
  *PeerJ*'s DOIs are easily identified as sequential.
* **RecdDate**: Recorded date the manuscript was received by *PeerJ*.
* **AcceptDate**: Recorded date the manuscript was accepted by *PeerJ*.
* **PubDate**: Recorded date of publication.
* **PeerRevAvail**: Dichotomous yes / no variable indicating whether the peer
  review history the article in the sample is publicly available.
* **PubMed**: Dichotomous yes / no variable indicating whether the manuscript is
  indexed in PubMed.
* **AuthCount**: Count of authors of a manuscript.
* **MultiNational**: Dichotomous yes / no variable indicating whether authors
  are from more than one nation, as indicated by their recorded address on the
  published article.

### Review Variables

* **NoReviewers**: Number of reviewers per article. For those articles that have
  a publicly available peer review history, the value for the variable is
  numerical. For those articles that do not have a publicly available peer
  review history, the value is *na*.
* **PeerAnon**: Categorical variable with values from 0 to 2. A value of 0
  indicates all referee identities are anonymous. A value of 1 indicates all
  referee identities are public. A value of 2 indicates a mix of anonymous and
  public referee identities. A value of *na* indicates no information available
  and is assigned to those articles without a publicly available peer review
  history.
* **NoRevisions**: Categorical variable indicating the number of revisions the
  manuscript went through before being accepted. The three levels include No
  Revision, One Revision, and Two Revisions.
* **RevStanding1**: Categorical variable indicating whether the first revision
  was accepted after no revisions, minor revisions, or major revisions. Values
  include: None, Minor, Major, and *na*.
* **RevStanding2**: Same as RevStanding-1 except the values indicate category of
  acceptance after the second revision of the manuscript. The range of data is
  either Minor or *na*.

The following two variables are held in separate CSV files.

* **R1.1**: Word count of the first review by the first referee
* **R2.1**: Word count of the first review by the second referee

### Use Variables

* **UniqueVisitorsAug**: The count of unique visitors to an article's page, as
  provided by *PeerJ*, and as recorded on August 20, 2013.
* **UniqueVisitorsNov**: Same as above and as recorded on November 20, 2013.
* **UniqueVisitorsFeb**: Same as above and as recorded on February 20, 2014.
* **UniqueVisitorsMay**: Same as above and as recorded on May 20, 2014.

---

* **PageviewsAug**: The count of unique page views to an article's page, as
  provided by *PeerJ*, and as recorded on August 20, 2013.
* **PageviewsNov**: Same as above and as recorded on November 20, 2013.
* **PageviewsFeb**: Same as above and as recorded on February 20, 2014.
* **PageviewsMay**: Same as above and as recorded on May 20, 2014.

---

* **SocRefUniqAug**: The unique count of social referrals to an article's page,
  as provided by *PeerJ*, and as recorded on August 20, 2013.
* **SocRefUniqNov**: Same as above and as recorded on November 20, 2013.
* **SocRefUniqFeb**: Same as above and as recorded on February 20, 2014.
* **SocRefUniqMay**: Same as above and as recorded on May 20, 2014.

---

* **SocRefTotalAug**: The total number of referrals by all social referrals to
  an article's page, as provided by *PeerJ*, and as recored on August 20, 2013.
* **SocRefTotalNov**: Same as above and as recorded on November 20, 2013.
* **SocRefTotalFeb**: Same as above and as recorded on February 20, 2014.
* **SocRefTotalMay**: Same as above and as recorded on May 20, 2014.

---


* **TopRefUniqAug**: The unique count of web referrals to an article's page, as
  provided by *PeerJ*, and as recorded on August 20, 2013.
* **TopRefUniqNov**: Same as above and as recorded on November 20, 2013.
* **TopRefUniqFeb**: Same as above and as recorded on February 20, 2014.
* **TopRefUniqMay**: Same as above and as recorded on May 20, 2014.

---

* **TopRefTotalAug**: The total number of referrals by all web referrals to an
  article's page, as provided by *PeerJ*, and as recorded on August 20, 2013.
* **TopRefTotalNov**: Same as above and as recorded on November 20, 2013.
* **TopRefTotalFeb**: Same as above and as recorded on February 20, 2014.
* **TopRefTotalMay**: Same as above and as recorded on May 20, 2014.

---

Data points added on February 20, 2014:

* **Promoted**: Dichotomous variable indicating whether the article was promoted
  as part of the [PeerJ Picks 2014 Collection][1] or, as of May 20, 2013, was
  promoted as part of the [The Line Islands Collection][2].
* **Correction**: Categorical variable indicating whether there is a
  post-publication correction to the article. At this point, the categories are:
  **minor**.
* **ScopusFeb**: Citation counts from Scopus.
* **GSFeb**: Citation counts from Google Scholar. Note that article
  10.7717/peerj.22 was not discoverable from Google Scholar and required a title
  search in Google Web in order to retrieve the citation count.

---

Data points added on May 20, 2014:

* **ScopusMay**: Citation counts from Scopus.
* **GSMay**: Citation counts from Google Scholar.
* **DownloadsMay**: PeerJ now displays on article pages the number of times
  article was downloaded. This new data point was added.

Wayback Machine confirms article download information was added in the last
three months. E.g.: [Article 107 on Wayback Machine][3]

---

Additional four columns marking the date of data collection.

---

On June 17, 2014, gathered information on citing journals. Collected information
for two additional tables of data:

Citing Titles 1:

* **doi**: DOI of original sample.
* **ScopusMay**: Citation count of DOI as of 5/20/2014.
* **title[0-7]**: Name of title citing DOI as of 5/20/2014. Maximum Scopus
  measured citations is 7.

Citing Titles 2:

* **title**: Title information only for journals that cited the PeerJ articles
  as of May 20, 2014. Gathered data from Scopus.
* **SJR2012**: [SJR (SCImago Journal Rank)][5] 2012 for each citing journal,
  collected from Scopus.
* **SNIP2012**: [SNIP (Source Normalized Impact per Paper)][6] for each citing
  journal, collected from Scopus.

**ArchivePolicy**: Archive Policy for each journal, collected from [SHERPA /
RoMEO][4]. Levels include:

* **Green**: "Can archive pre-print and post-print or publisher's version/PDF"
* **Blue**: "Can archive post-print (i.e., final draft post-refereeing) or
  publisher's version/PDF:
* **Yellow**: "Can archive pre-print (i.e., pre-refereeing)"
* **White**: "Archiving not formally supported"
* **ungraded**: SHERPA / RoMEO has not yet determined archive policy of journal.

**OAStatus**: Open Access status of Journal, collected from SHERPA / RoMEO.
Levels include:

* **Paid OA**: "A paid open access option is **available** for this journal."
  I.e., not normally a gold open access journal, but authors have an option to
  make their article OA upon payment.
* **No-paid OA**: "This journal is not in the list for the paid open access
  option." I.e., not a gold open access journal.
* **DOAJ**: "DOAJ as an open access journal."  A gold open access journal.

---

On June 23, 2014, gathered additional author information:

* **Gender**: Three level categorical variable indicating the gender of the
  authors: Mixed (female and male authors), Female (all female authors), Male
  (all male authors).
* **FemaleFirst**: Binary variable (yes/no) indicating whether the first author
  is female.

---

At the end of the first week of August 2014, I [scraped (R scrapeR)][7] the
sampled articles' web pages on *PeerJ* in order to collect the lists of
references for each article. After processing, I have three files:

- journalTitlesCited.csv
- journalTitlesCitedUnique.csv
- bookTitlesCited.csv

The first file contains a list of all 2,253 non-unique journal titles in the
reference lists of the 49 sampled articles. The file contains two columns: the
doi of the *PeerJ* article and a column for the name of journal titles. The
bookTitlesCited.csv contains the same information for the books that are in the
references.

The journalTitlesCitedUnique.csv contains five columns:

1. **titleCount**: lists count of times a journal appears in all the references.
1. **titles**: the names of the journal titles.
1. **cumsum**: the cumulative sum of the titleCount column.
1. **percWhole**: the percentage each title is of the whole.
1. **cumper**: the cumulative percentage of the titleCount.

---

On August 5, 2014, and for the most cited journal titles that appear ten or more
times in the journalTitlesCitedUnique.csv file, I gathered the SNIP 2012 and the
SJR 2012 data from Scopus' [Journal Metrics][8] page. Specifically, the URL to
the file for this data is contained in the following spreadsheet: [September 13,
1999-2012 SNIP and SJR dataset][9].

On August 6, 2014, for the above cited journals, I gathered the archival policy
and OA status information from [SHERPA / RoMEO][4]. This is the same variable
data that I collected on June 17, 2014 for the citing journals (see above). As
before, I manually searched each title using the SHERPA / RoMEO service.

[1]: https://peerj.com/collections/5-peerjpicks/ "PeerJ Picks 2014"
[2]: https://peerj.com/collections/1-line-islands/ "The Line Islands Collection"
[3]: http://web.archive.org/web/20131026113907/https://peerj.com/articles/107/ "PeerJ Wayback Machine"
[4]: http://www.sherpa.ac.uk/romeo/ "SHERPA / RoMEO"
[5]: http://www.journalmetrics.com/sjr.php "SJR"
[6]: http://www.journalmetrics.com/snip.php "SNIP"
[7]: http://cran.r-project.org/web/packages/scrapeR/ "R scrapeR package"
[8]: http://www.journalmetrics.com/values.php "Journal Metrics"
[9]: http://www.journalmetrics.com/documents/SNIP_SJR_complete_1999_2012_v1.xlsx "1999-2012 journal metrics data"

---
title: "PeerJ Research 7"
date: 2013-11-20 12:03
---

On August 20, 2013 I collected a round of data based on a sample of random 
numbers generated on July 20, 2013.

Today, on November 20, 2013, I collected a follow up round of data
plus some additional data points. Currently, the data is stored in
a LibreOffice Calc spreadsheet file to be exported to a CSV file
and imported into R Studio.

I have the following variables:

* RandomNum: The random number originally generated. Corresponds
  to the DOI assigned to *PeerJ*.
* DOI: The DOI assigned to *PeerJ*. Corresponds to the random
  number. *PeerJ*'s DOIs are easily identified as sequential.
* RecdDate: Recorded date the manuscript was received by *PeerJ*.
* AcceptDate: Recorded date the manuscript was accepted by
  *PeerJ*.
* PubDate: Recorded date of publication.
* PeerRevAvail: Dichotomous yes / no variable indicating whether
  the peer review history the article in the sample is publicly
  available.
* PubMed: Dichotomous yes / no variable indicating whether the
  manuscript is indexed in PubMed.
* AuthCount: Count of authors of a manuscript.
* MultiNational: Dichotomous yes / no variable indicating whether
  authors are from more than one nation, as indicated by their
  recorded address on the published article.
* NoReviewers: Number of reviewers per article. For those articles
  that have a publicly available peer review history, the value
  for the variable is numerical. For those articles that do not
  have a publicly available peer review history, the value is
  *na*.
* PeerAnon: Categorical variable with values from 0 to 2. A value
  of 0 indicates all referee identities are anonymous. A value of
  1 indicates all referee identities are public. A value of 2
  indicates a mix of anonymous and public referee identities. A
  value of *na* indicates no information available and is assigned
  to those articles without a publicly available peer review
  history.
* NoRevisions: Numerical variable indicating the number of
  revisions the manuscript went through before being accepted.
* RevStanding-1: Categorical variable indicating whether the first
  revision would be accepted after no revisions, minor revisions,
  or major revisions. Values include: None, Minor, Major, and na.
* RevStanding-2: Same as RevStanding-1 except the values indicate
  category of acceptance after the second revision of the
  manuscript.
* UniqueVisitors-Aug: The count of unique visitors to an article's
  page, as provided by *PeerJ*, and as recorded on August 20,
  2013.
* UniqueVisitors-Nov: Same as above and as recorded on November
  20, 2013.
* Pageviews-Aug: The count of unique pageviews to an article's
  page, as provided by *PeerJ*, and as recorded on August 20,
  2013.
* Pageviews-Nov: Same as above and as recorded on November 20,
  2013.
* SocRefUniq-Aug: The unique count of social referrals to an
  article's page, as provided by *PeerJ*, and as recorded on
  August 20, 2013.
* SocRefUniq-Nov: Same as above and as recorded on November 20,
  2013.
* SocRefTotal-Aug: The total number of referrals by all social
  referrals to an article's page, as provided by *PeerJ*, and as
  recored on August 20, 2013.
* SocRefTotal-Nov: Same as above and as recorded on November 20,
  2013.
* TopRefUniq-Aug: The unique count of weblog referrals to an
  article's page, as provided by *PeerJ*, and as recorded on
  August 20, 2013.
* TopRefUniq-Nov: Same as above and as recorded on November 20,
  2013.
* TopRefTotal-Aug: The total number of referrals by all weblog
  referrals to an article's page, as provided by *PeerJ*, and as
  recorded on August 20, 2013.
* TopRefTotal-Nov: Same as above and as recorded on November 20,
  2013.
* R1.1: Word count of the first review by the first referee
* R2.1: Word count of the first review by the second referee

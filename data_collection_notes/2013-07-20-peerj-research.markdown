---
title: "PeerJ Research 1"
date: 2013-07-20 12:05
---

**Note:** This is the first set of notes on a research project I'm
working on and that will be presented, as part of a panel, at the
next [ASIS&amp;T](http://asis.org/) conference in Montreal. More
details about the project later.

## PeerJ Sampling

**Date of sampling: July 20, 2013**

### Step 1: Determine the population size

The search results for all published articles indicate that 107
articles have been published since PeerJ's launch (107 results are
returned when visiting the article section of the site at
[https://peerj.com/articles/](https://peerj.com/articles/)).
However, the sampling frame includes 108 articles, or articles 1
through 108. This is established by examining the sequence of DOIs
of the published articles, which begin with 1 and end at 108, at
the time of data collection. See details in Step 3 below.

### Step 2: Generate random numbers

Take a random sample of 30 articles.

Total population = 108  
Total sample size = 30

[Random.org](http://www.random.org/) | [Random Integer Set
Generator](http://www.random.org/integer-sets/)

Parameters include:

Generate 1 set with 30 unique random integers in each.
Each integer should have a value between 1 and 108.

Set includes:

2, 6, 9, 12, 14, 19, 21, 22, 24, 25, 28, 29, 32, 33, 49, 55, 56,
65, 69, 70, 74, 75, 76, 80, 85, 93, 94, 99, 102, 106

### Step 3: Identify articles in population

Collect by DOI. DOI is assigned incrementally so:
doi:10.7717/peerj.1 is article 1, or the first article published.

Matching collected random numbers is as simple as matching to DOI,
noted by the number ending the DOI. Example:

    random number 19 == doi:10.7717/peerj.19

Note, there is some skipping in the search results page. For
example, at time of data sampling, the article for
doi:10.7717/peerj.4 is not listed in the search results list for
all published articles, although the DOI and the article both
exist and are published (accessed by visiting its article page).
Best guess at the moment is that the search page does not return
all results when search is refined or limited by *Published
articles*. Will need to confirm.


### Step 4: Check each DOI and collect time-sensitive metadata

Visit each DOI to insure that the article exists and is published.
This is done by (and checked through PubMed) by substituting the
DOI's accession number in the article's URL, where x at the end of
the URL signifies the article number:

    https://peerj.com/articles/x/

Also collect various data points and other metadata that is time
sensitive. This metadata includes various metrics and web
analytics. Units collected and saved as a CSV file include (with
literal variable names in brackets, when necessary):

1. Random Number (RandomNum)
2. DOI (doi)
3. Acceptance Date (AcceptDate)
4. Peer Review History Public (PeerRevAvail)
5. Archived on PubMed (PubMed)
6. Unique visitors (UniqueVisitors)
7. Pageviews
8. Count of social referrals by unique visitors (SocRefUniq)
9. Sum of social referrals by unique visitors (SocRefTotal)
10. Count of top referrals by unique visitors (TopRefUniq)
11. Sum of top referrals by unique visitors (TopRefTotal)

Since the objective of the study is not a bibliometric (or
comparable) analysis, I do not collect the breakdown of social
referrals (e.g., Twitter, Google+, Facebook, etc.) or Top
referrals (e.g., Google, NIH, etc.), but will only collect total
counts and total sums. I collect this primarily for descriptive
statistics, but it may be useful later.

Static metadata (or metadata that should be static), such as
author count, subject areas, and so forth, will be collected
later.

### Step 5: Data collection: First Round

I collected the data and discovered that nearly half (~ 47%) of
the published articles do not have public peer review histories.
Since authors have the option to withhold these from the public,
it seems that many do, although this appears to be more common
during the first half of the sample and not the second half. Since
the sample is ordered by publication date (or DOI accession
number), perhaps this means authors are acquiring more trust in
the publisher and the system as PeerJ gets itself established. The
big caveat to this proposition is that the sample size is small
and it's not possible to generalize with any decent degree of
confidence. The trend can best be summarized with a quick table
function in R:

Where **pj** signifies the data frame in R and where **Yes**
indicates the existence of a peer review history and **No** the
opposite:

For the first half of the sample:

    table(pj$PeerRevAvail[1:15])

    No  Yes
    11  4

For the second half of the sample:

    table(pj$PeerRevAvail[16:30])

    No  Yes
     3  12

Sixteen peer review histories is not sufficient for my analysis.
Since I want to perform an analysis of 30 articles, I will collect
a second round of data by taking another random sample with
replacement, using random.org. I'm sampling with replacement in
order to insure randomness (that is, all still have an equal
chance of being selected). This should also help verify whether
the above trend with peer review histories maintains after
collecting additional data.

### Step 6: Data collection: Second Round

Using the same parameters from Step 2, the sequence I get is:

1, 3, 4, 9, 14, 15, 17, 19, 22, 24, 25, 26, 28, 34, 35, 36, 48,
56, 60, 61, 68, 70, 73, 75, 76, 86, 91, 92, 105, 107

Adding these numbers to the spreadsheet (CSV) file tells me this
new set involves 19 additional articles. Next step is to repeat
the metadata collection step from Step 4.

After I collected the data: Examining peer review history trends
again, now with 49 articles in the sample:

For the first half of the sample:

    table(pj$PeerRevAvail[1:25])

    No  Yes
    13  12

For the second half of the sample:

    table(pj$PeerRevAvail[26:49])

    No  Yes
     3  21

It seems the trend holds in the second round, although something
could be said for the equilibrium in the new first half of the
sample, suggesting an interesting change (or becoming) with the
distribution. Also, the second round of collection lets me meet my
*a priori* criteria of 30 articles -- since I now have 33 peer
review histories for the analysis.

### Next steps

Plan is to resume data collection and analysis in August. Details
later.

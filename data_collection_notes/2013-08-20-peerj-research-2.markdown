---
title: "PeerJ Research 2"
date: 2013-08-20 15:07
---

## PeerJ Data Collection

**Date of 2nd round of data collection: August 20, 2013**

This data is (or should be) static and therefore was not dependent
or time sensitive to the first round of data that was collected.

Reminder: I have 33 articles with peer review histories. However,
because of resampling with replacement (see first post), I've
collected some information on 49 articles.

### Step 1: Collect Peer Review History Documents

Visited each URL for the articles that have a peer review history
from my sample and copied or downloaded the documents and metadata
related to that history. These documents include:

Items that could be copied and pasted into a text editor:

- Peer review comments by each peer reviewer
- Second round of peer review comments by each peer reviewer, if
  applicable
- Comments by the Academic Editor responsible for the peer review
  process of the particular article

File naming convention:

- n-Rn-n is ArticleNo-ReviewerNo-ReviewNo (eg: 92-R2-1 is 92nd
  article, second reviewer's first review).
- n-AE-n is ArticleNo-AcademicEditor-EditorCommentNo (eg: 35-AE-2
  is 35th article, Academic Editor's second comment).

Items that had to be downloaded:

- Rebuttal documents submitted by authors.

File naming convention:

- n-Rebuttal-n.{doc|docx|pdf}. Meaning:
  ArticleNo-Rebuttal-RebuttalNumber.{doc|docx|pdf}. Ex:
  55-Rebuttal-1.doc is the first rebuttal for the 55th article.

**Note:** The ordinal numbers I'm using are not relative to my
sample. So the 55th article means the 55th article in the PeerJ
collection, and not in my sample, and which corresponds to its
DOI: 10.7717/peerj.55. And that corresponds to one of its stable
URLS: http://dx.doi.org/10.7717/peerj.55.

Metadata collected (w/ variable names in brackets):

- Number / count of reviewers (NoReviewers)
- Anonymous State of Peer Reviewers (PeerAnon)

    0 = all peer reviewers' identities are anonymous  
    1 = all peer reviewers' identities are public  
    2 = mixed of anonymous and public reviewers' identities  

- Number of Revisions (NoRevisions)
- Standing after First Round of Review (RevStanding1)

    Minor Revisions needed  
    Major Revisions needed  

- Standing after Second Round of Review (RevStanding2)

    Minor Revisions needed  
    Major Revisions needed  

### Step 3: Data check

Went through the data and revisited each URL to check for mistakes
in data collection.

### Step 4: Quick review of data 

Some quick results (based on grepping in the command line):

- 83 total peer review comments
- 70 comments from the first round of reviews
- 13 comments from the second round of reviews
- 43 total rebuttals
- 77 Academic Editor comments

Grep commands (respective to above list):

    ls | grep -i "R[0-9]" | wc -l  
    ls | grep -i "R[0-9]-1" | wc -l  
    ls | grep -i "R[0-9]-2" | wc -l  
    ls | grep -i "rebuttal" | wc -l  
    ls | grep -i "AE" | wc -l

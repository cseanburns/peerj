---
title: "PeerJ Research 3"
date: 2013-09-16 15:03
---

## File Handling and Conversion

The downloaded and retrieved PeerJ files exist in four formats:
txt, doc, docx, and pdf. I have archived this collection of
sampled files so that I will have access to the originals in case
needed. For analysis, I copied the files to a new directory and
converted the pdfs, docs, and docx files to txt files.

I will import these files into R separately (probably with
[RQDA](http://rqda.r-forge.r-project.org/)). But in order to take
a quick look at the whole thing, I output all the files to a
single file (out of a total of 203 files) in a separate temporary
directory: 

    for i in *txt ; do cat $i >> $HOME/tmp/oneFile.txt ; done

Since the files are numbered 01 through 107, this saves the files
out of order (e.g., 99 comes after 107 because of the file
system). But since this is just a quick peek, it is not a problem.

## Importing and Peeking

There are a lot of text mining and R examples on the web. This is
not the point of my project, but I use one nice
[example](http://www.exegetic.biz/blog/2013/09/text-mining-the-complete-works-of-william-shakespeare/)
to take a quick look at what I have. The code follows:

    # Import file into R
    pj <- readLines("$HOME/tmp/oneFile.txt")

    # Take a quick peak at the text
    head(pj)
    tail(pj)

    # Load text mining and stemming libraries
    library(tm)
    library(SnowballC)

    # Create vector out of document
    doc.vec <- VectorSource(pj)
    doc.corpus <- Corpus(doc.vec)
    summary(doc.corpus)

    # Convert text to lower case, remove punctuation,
    # numbers, and stopwords. Stem document and 
    # strip white space
    doc.corpus <- tm_map(doc.corpus, tolower)
    doc.corpus <- tm_map(doc.corpus, removePunctuation)
    doc.corpus <- tm_map(doc.corpus, removeNumbers)
    doc.corpus <- tm_map(doc.corpus, removeWords,
                        stopwords("english"))
    doc.corpus <- tm_map(doc.corpus, stemDocument)
    doc.corpus <- tm_map(doc.corpus, stripWhitespace)

    # Take a peak
    inspect(doc.corpus[1])
    inspect(doc.corpus[2])

    # Create TD Matrix
    TDM <- TermDocumentMatrix(doc.corpus)
    inspect(TDM[1:10,1:10])

    # Create DT Matrix
    DTM <- DocumentTermMatrix(doc.corpus)
    inspect(DTM[1:10,1:10])

    # Quick exploration
    findFreqTerms(TDM, 100)
    findAssocs(TDM, "discuss", 0.2)
    findAssocs(TDM, "cite", 0.2)

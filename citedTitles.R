# load libraries
library(scrapeR)

## Commands here used to generate tables 10 and 11

## The process is broken down into five parsings because PeerJ returns errors if
## I attempt to scrape too many articles at once.

---

## Create paths
paths1 <- peerjMetadata$RandomNum[1:9]
paths2 <- peerjMetadata$RandomNum[10:19]
paths3 <- peerjMetadata$RandomNum[20:29]
paths4 <- peerjMetadata$RandomNum[30:39]
paths5 <- peerjMetadata$RandomNum[40:49]

# Use as.matrix() because otherwise the vector of relative URLs is coerced into
# the wrong characters

urls <- function(paths) {
        base_url <- "https://peerj.com/articles/"
        all_urls <- paste(base_url, as.matrix(paths), sep = "")
        paste(all_urls, "/", sep = "")
}

## Retrieve reference lists from PeerJ articles and save reference lists into R
## lists; doing this in five batches to retrieve the sampled articles

all_urls <- urls(paths1)
parsed1  <- scrape(url = all_urls, headers = TRUE,
                  parse = FALSE, follow = FALSE,
                  maxSleep = 10,
                  userAgent = unlist(options("my email: sean.burns@uky.edu")))
parsed1  <- scrape(object = "parsed1") # parse object


all_urls <- urls(paths2)
parsed2  <- scrape(url = all_urls, headers = TRUE,
                  parse = FALSE, follow = FALSE,
                  maxSleep = 10,
                  userAgent = unlist(options("my email: sean.burns@uky.edu")))
parsed2  <- scrape(object = "parsed2") # parse object


all_urls <- urls(paths3)
parsed3  <- scrape(url = all_urls, headers = TRUE,
                   parse = FALSE, follow = FALSE,
                   maxSleep = 10,
                   userAgent = unlist(options("my email: sean.burns@uky.edu")))
parsed3  <- scrape(object = "parsed3") # parse object


all_urls <- urls(paths4)
parsed4  <- scrape(url = all_urls, headers = TRUE,
                   parse = FALSE, follow = FALSE,
                   maxSleep = 10,
                   userAgent = unlist(options("my email: sean.burns@uky.edu")))
parsed4  <- scrape(object = "parsed4") # parse object


all_urls <- urls(paths5)
parsed5  <- scrape(url = all_urls, headers = TRUE,
                   parse = FALSE, follow = FALSE,
                   maxSleep = 10,
                   userAgent = unlist(options("my email: sean.burns@uky.edu")))

parsed5  <- scrape(object = "parsed5") # parse object

rm(all_urls)

---

# Get all journal titles.
# Usage: jt(parsed1, 1)
# Retreive journal titles in first element of file

jt <- function(file, n) {
        journalTitles <- xpathSApply(file[[n]],
                                     "//span[@class='source']", xmlValue)
        print(journalTitles)
}

# parse = parse file, m = article in parse file, x = matching doi
# Usage: alljournalTitles(parse1, 1, 1)
# Meaning: first parse file, first article in parse file, matching doi in peerj
# dataframe

alljournalTitles <- function(parse, m, x) {
        titles <- jt(parse, m)
        n <- 1:length(titles)
        doi <- as.character(rep(peerjMetadata$doi[x], length(n)))
        jTitles <- cbind(doi, titles)
        jTitles <- as.data.frame(jTitles)
}

# SHOULD CONVERT THESE TO A FUNCTION
a <- alljournalTitles(parsed1, 1, 1)
b <- alljournalTitles(parsed1, 2, 2)
c <- alljournalTitles(parsed1, 3, 3)
d <- alljournalTitles(parsed1, 4, 4)
e <- alljournalTitles(parsed1, 5, 5)
f <- alljournalTitles(parsed1, 6, 6)
g <- alljournalTitles(parsed1, 7, 7)
h <- alljournalTitles(parsed1, 8, 8)
i <- alljournalTitles(parsed1, 9, 9)

abcd <- rbind(a,b,c,d,e,f,g,h,i)
rm(a,b,c,d,e,f,g,h,i)

a <- alljournalTitles(parsed2, 1, 10)
b <- alljournalTitles(parsed2, 2, 11)
c <- alljournalTitles(parsed2, 3, 12)
d <- alljournalTitles(parsed2, 4, 13)
e <- alljournalTitles(parsed2, 5, 14)
f <- alljournalTitles(parsed2, 6, 15)
g <- alljournalTitles(parsed2, 7, 16)
h <- alljournalTitles(parsed2, 8, 17)
i <- alljournalTitles(parsed2, 9, 18)
j <- alljournalTitles(parsed2, 10, 19)

bbb <- rbind(a,b,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)

a <- alljournalTitles(parsed3, 1, 20)
b <- alljournalTitles(parsed3, 2, 21)
c <- alljournalTitles(parsed3, 3, 22)
d <- alljournalTitles(parsed3, 4, 23)
e <- alljournalTitles(parsed3, 5, 24)
f <- alljournalTitles(parsed3, 6, 25)
g <- alljournalTitles(parsed3, 7, 26)
h <- alljournalTitles(parsed3, 8, 27)
i <- alljournalTitles(parsed3, 9, 28)
j <- alljournalTitles(parsed3, 10, 29)

ccc <- rbind(a,b,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)

a <- alljournalTitles(parsed4, 1, 30)
b <- alljournalTitles(parsed4, 2, 31)
c <- alljournalTitles(parsed4, 3, 32)
d <- alljournalTitles(parsed4, 4, 33)
e <- alljournalTitles(parsed4, 5, 34)
f <- alljournalTitles(parsed4, 6, 35)
g <- alljournalTitles(parsed4, 7, 36)
h <- alljournalTitles(parsed4, 8, 37)
i <- alljournalTitles(parsed4, 9, 38)
j <- alljournalTitles(parsed4, 10, 39)

ddd <- rbind(a,b,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)

a <- alljournalTitles(parsed5, 1, 40)
b <- alljournalTitles(parsed5, 2, 41)
c <- alljournalTitles(parsed5, 3, 42)
d <- alljournalTitles(parsed5, 4, 43)
e <- alljournalTitles(parsed5, 5, 44)
f <- alljournalTitles(parsed5, 6, 45)
g <- alljournalTitles(parsed5, 7, 46)
h <- alljournalTitles(parsed5, 8, 47)
i <- alljournalTitles(parsed5, 9, 48)
j <- alljournalTitles(parsed5, 10, 49)

eee <- rbind(a,b,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)

alltitlesjournals <- rbind(abcd, bbb, ccc, ddd, eee)
rm(abcd, bbb, ccc, ddd, eee)
write.table(alltitlesjournals,
            file = "citedJournals.csv") # I lowered the case in libreoffice calc and resorted

---
  
### BOOKS

# Changed from alltitlesbooks to bookTitlesCited
bookTitlesCited <- read.csv("~/Dropbox/workspace/peerj/data-processed/bookTitlesCited.csv")
atb <- bookTitlesCited
rm(bookTitlesCited)
atb$titles <- as.character(atb$titles)
atb$titles[129] <- "The PyMOL user's manual"
atb$titles[141] <- "The PyMOL user's manual"
atb1 <- atb
atb1$titles <- as.factor(atb1$titles)

# look at data w/o NAs
atb2 <- atb[1:189,]
unique(atb2$doi)
unique(atb2$titles)
sort(table(atb2$titles))

## Examine how many titles are in each of the articles that cite more than two books
titleCount <- function(x, file) {
  a <- grep(x, file$titles)
  print(table(as.character(file$doi[a])))
}

length(titleCount("developmental genetics and plant evolution", atb2))
length(titleCount("coral reefs: an ecosystem in transition", atb2))
length(titleCount("the pymol user's manual", atb2))
length(titleCount("the natural history of madagascar", atb2))
length(titleCount("the dinosauria", atb2))
length(titleCount("risikovurdering av norsk fiskeoppdrett", atb2))
length(titleCount("on the various contrivances by which british and foreign orchids are fertilised by insects, and on the good effects of intercrossing", atb2))
length(titleCount("minerals in soil environments", atb2))
length(titleCount("encyclopedia of marine mammals", atb2))


---

## BOOKS CITED

### Get all book titles
bt <- function(file, x) {
  bookTitles <- xpathSApply(file[[x]],
                            "//a[@class='source']", xmlValue)
  print(bookTitles)
}

### parse = parse file, m = book in parse file, x = matching doi
### Usage: alljournalTitles(parse1, 1, 1)
#### Meaning: first parse file, first article in parse file, matching doi in peerj dataframe
allbookTitles <- function(parse, m, x) {
  titles <- bt(parse, m)
  n <- 1:length(titles)
  doi <- as.character(rep(peerjMetadata$doi[x], length(n)))
  bTitles <- cbind(doi, titles)
  bTitles <- as.data.frame(bTitles)
}

a <- allbookTitles(parsed1, 1, 1)
b <- allbookTitles(parsed1, 2, 2)
c <- allbookTitles(parsed1, 3, 3)
d <- allbookTitles(parsed1, 4, 4)
e <- allbookTitles(parsed1, 5, 5)
f <- allbookTitles(parsed1, 6, 6)
g <- allbookTitles(parsed1, 7, 7)
h <- allbookTitles(parsed1, 8, 8)
i <- allbookTitles(parsed1, 9, 9)

aaa <- rbind(a,b,c,d,e,f,g,h)
rm(a,b,c,d,e,f,g,h,i)
#i had no books

a <- allbookTitles(parsed2, 1, 10)
b <- allbookTitles(parsed2, 2, 11)
c <- allbookTitles(parsed2, 3, 12)
d <- allbookTitles(parsed2, 4, 13)
e <- allbookTitles(parsed2, 5, 14)
f <- allbookTitles(parsed2, 6, 15)
g <- allbookTitles(parsed2, 7, 16)
h <- allbookTitles(parsed2, 8, 17)
i <- allbookTitles(parsed2, 9, 18)
j <- allbookTitles(parsed2, 10, 19)

bbb <- rbind(a,b,c,d,e,g,i,j)
rm(a,b,c,d,e,f,g,h,i,j)
#f, h had no books

a <- allbookTitles(parsed3, 1, 20)
b <- allbookTitles(parsed3, 2, 21)
c <- allbookTitles(parsed3, 3, 22)
d <- allbookTitles(parsed3, 4, 23)
e <- allbookTitles(parsed3, 5, 24)
f <- allbookTitles(parsed3, 6, 25)
g <- allbookTitles(parsed3, 7, 26)
h <- allbookTitles(parsed3, 8, 27)
i <- allbookTitles(parsed3, 9, 28)
j <- allbookTitles(parsed3, 10, 29)

ccc <- rbind(a,d,e,g,h,i)
rm(a,b,c,d,e,f,g,h,i,j)
#b,c,f,j had no books

a <- allbookTitles(parsed4, 1, 30)
b <- allbookTitles(parsed4, 2, 31)
c <- allbookTitles(parsed4, 3, 32)
d <- allbookTitles(parsed4, 4, 33)
e <- allbookTitles(parsed4, 5, 34)
f <- allbookTitles(parsed4, 6, 35)
g <- allbookTitles(parsed4, 7, 36)
h <- allbookTitles(parsed4, 8, 37)
i <- allbookTitles(parsed4, 9, 38)
j <- allbookTitles(parsed4, 10, 39)

ddd <- rbind(d,f,g,i,j)
rm(a,b,c,d,e,f,g,h,i,j)
#a,b,c,e,h had no books

a <- allbookTitles(parsed5, 1, 40)
b <- allbookTitles(parsed5, 2, 41)
c <- allbookTitles(parsed5, 3, 42)
d <- allbookTitles(parsed5, 4, 43)
e <- allbookTitles(parsed5, 5, 44)
f <- allbookTitles(parsed5, 6, 45)
g <- allbookTitles(parsed5, 7, 46)
h <- allbookTitles(parsed5, 8, 47)
i <- allbookTitles(parsed5, 9, 48)
j <- allbookTitles(parsed5, 10, 49)

eee <- rbind(a,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)
#b had no books

alltitlesbooks <- rbind(aaa, bbb, ccc, ddd, eee)
rm(aaa, bbb, ccc, ddd, eee)

## articles with no books
a <- allbookTitles(parsed1, 9, 9)
b <- allbookTitles(parsed2, 6, 15)
c <- allbookTitles(parsed2, 8, 17)
d <- allbookTitles(parsed3, 2, 21)
e <- allbookTitles(parsed3, 3, 22)
f <- allbookTitles(parsed3, 6, 25)
g <- allbookTitles(parsed3, 10, 29)
h <- allbookTitles(parsed4, 1, 30)
i <- allbookTitles(parsed4, 2, 31)
j <- allbookTitles(parsed4, 3, 32)
k <- allbookTitles(parsed4, 5, 34)
l <- allbookTitles(parsed4, 8, 37)
m <- allbookTitles(parsed5, 2, 41)

a <- a$doi[1]
b <- b$doi[1]
c <- c$doi[1]
d <- d$doi[1]
e <- e$doi[1]
f <- f$doi[1]
g <- g$doi[1]
h <- h$doi[1]
i <- i$doi[1]
j <- j$doi[1]
k <- k$doi[1]
l <- l$doi[1]
m <- m$doi[1]

# Fix rownames and add NAs to book title data frame
rownames(lll) <- seq(190, 202)
alltitlesbooks <- rbind(alltitlesbooks, lll)
rm(lll)

---

##### REMOVED CODE ##### 
# Test results (not necessary; fixed by parsing multiple times in smaller
# chunks
# Find all errors from too many requests
# checkResults <- function(list) {
#         for(i in 1:length(list))
#                 print(xpathSApply(list[[i]], "//title", xmlValue))
# }
# Test results: Get all references for first article
# xpathSApply(parsed1[[1]], "//li[@class='ref']", xmlValue)

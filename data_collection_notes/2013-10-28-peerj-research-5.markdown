---
title: "PeerJ Research 5"
date: 2013-10-28 16:33
---

## Begin word count analysis

Use BASH scripting to assemble word counts.

All files adhere to the following format:

articleNo-Role-ResponseNo.txt

Example: the first article, the first reviewer, the first
reviewer's response:

**01-R1-1.txt**  

And the first reviewer's second response:

**01-R1-2.txt**  

Most articles have two reviewers. A few have three reviewers.

To assemble word counts for all the reviewers:

    for i in *R1-1* ; do
        wc -w $i >> ../wordcounts/R1-1.csv
    done

    for i in *R1-2* ; do
        wc -w $i >> ../wordcounts/R1-2.csv
    done

    for i in *R2-1* ; do
        wc -w $i >> ../wordcounts/R2-1.csv
    done

    for i in *R2-2* ; do
        wc -w $i >> ../wordcounts/R2-2.csv
    done

    for i in *R3-1* ; do
        wc -w $i >> ../wordcounts/R3-1.csv
    done

To assemble word counts for all academic editors:

    for i in *AE-1* ; do
        wc -w $i >> ../wordcounts/AE-1.csv
    done

    for i in *AE-2* ; do
        wc -w $i >> ../wordcounts/AE-1.csv
    done

    for i in *AE-3* ; do
        wc -w $i >> ../wordcounts/AE-1.csv
    done

To assemble word counts for all author rebuttals:

    for i in *Rebuttal-1* ; do
        wc -w $i >> ../wordcounts/Rebuttal-1.csv
    done

    for i in *Rebuttal-2* ; do
        wc -w $i >> ../wordcounts/Rebuttal-2.csv
    done

Then use awk to print only the word counts for each file and
direct output to a new file:

    awk '{ print $1}' R1-1.csv >> R1-1-count.csv
    awk '{ print $1}' R2-1.csv >> R2-1-count.csv
    awk '{ print $1}' R3-1.csv >> R3-1-count.csv
    awk '{ print $1}' AE-1.csv >> AE-1-count.csv
    awk '{ print $1}' Rebuttal-1.csv >> Rebuttal-1-count.csv
    awk '{ print $1}' R1-2.csv >> R1-2-count.csv
    awk '{ print $1}' R2-2.csv >> R2-2-count.csv
    awk '{ print $1}' AE-2.csv >> AE-2-count.csv
    awk '{ print $1}' Rebuttal-2.csv >> Rebuttal-2-count.csv
    awk '{ print $1}' AE-3.csv >> AE-3-count.csv

Then uses the BASH *paste* command to create one CSV file from all
these.

    paste R1-1-count.csv R2-1-count.csv R3-1-count.csv
    AE-1-count.csv Rebuttal-1-count.csv R1-2-count.csv
    R2-2-count.csv AE-2-count.csv Rebuttal-2-count.csv
    AE-3-count.csv >> total-counts.csv

Then opened up *total-counts.csv* to check numbers and confirm
with files. Also checked several to the review history on *PeerJ*
to see if accurate.

Next uploaded *total-counts.csv* into RStudio and retrieved
summary statistics on word counts.

I'll add these individually now, and a more detailed, prettier
report later:

    summary(totalCounts$R1.1)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    94.0   211.0   401.0   461.9   542.0  1443.0 

    summary(totalCounts$R2.1)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    30.0   270.0   322.0   497.2   535.0  1812.0       1 

    summary(totalCounts$R3.1)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    87.0   157.0   271.0   605.2   682.0  1829.0      28 

    summary(totalCounts$AE.1)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    32.5    51.0   141.2    95.0  1235.0        1 

    summary(totalCounts$Rebuttal.1)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    204    1305    1765    2080    2319    6547        4 

    summary(totalCounts$R1.2)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    33.0    95.0   186.0   504.2   604.0  2362.0      24 

    summary(totalCounts$R2.2)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    27.00   48.75  513.00  680.20 1144.00 1668.00     29 

    summary(totalCounts$AE.2)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.00   14.75   27.50   97.47   76.00  700.00       1 

    summary(totalCounts$Rebuttal.2)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    41.0   223.5  1389.0  1949.0  2742.0  7709.0      19

    summary(totalCounts$AE.3)
    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.00   12.00   17.00   24.23   34.00   70.00      20

Much of this could be automated.

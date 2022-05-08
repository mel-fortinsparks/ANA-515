#This is the ANA515 Practicum Assignment
#Using R, I must clean the data so it's ready for analysis. 
#Find errors and decide how to deal with them, include notes to justify (30pts)

#The first thing I need to do is consider the various libraries I want to use for this project
#The two main ones that come to mind are tidyverse and ggplot2
library(tidyverse)
library(ggplot2)
library(readxl)

#The next thing I need to do is save the data into my work space; this is not a graded thing this time
scores_ay1718 <- read_excel("C:/Users/melin/OneDrive/Desktop/Data Analytics M.S/ANA515/ANA515 R Working Directory/Copy of G&T Results 2017-18.xlsx")
scores_ay1819 <- read_excel("C:/Users/melin/OneDrive/Desktop/Data Analytics M.S/ANA515/ANA515 R Working Directory/Copy of G&T Results 2018-19.xlsx")
#The second df has two extra variables with the names "...13" and "...14"; 
#it isn't entirely clear what these columns are talking about, possibly test prep
#I'm going to drop these columns because this data was not collected for AY17-18, so I can't definitely say if test prep happened or didn't for the test takers that year
scores_ay1819 <- subset(scores_ay1819, select = -c(`...13`,`...14`))

#Cleaning: First column -- "Timestamp"
#Looking at the first column of both dfs, I can see that the DTG formatting doesn't match
#I'm not overly interested in what time they took the test that day, so I'm going to change the scores_ay1718 to just YYYY-MM-DD
class(scores_ay1718$Timestamp)
scores_ay1718$Timestamp <- as.Date.POSIXct(scores_ay1718$Timestamp)
#Four of the dates in scores_ay1718$Timestamp still don't look right; there's one from 2019 and three from 1899
hist(scores_ay1718$Timestamp, breaks=150)
#All of the other test scores came from 2017, mostly APR-JUN, one in SEP and one in NOV
#There's no telling when exactly these tests took place, but the *date* isn't that important to me
#I'll make them all 2017-12-13 so I can keep track of exactly which 4 observations had the error, but keep the rest of the data
scores_ay1718$Timestamp <- as.character(scores_ay1718$Timestamp)
scores_ay1718[scores_ay1718$Timestamp=="1899-12-31", "Timestamp"] <- "2017-12-31"
scores_ay1718[scores_ay1718$Timestamp=="2019-01-11", "Timestamp"] <- "2017-12-31"
scores_ay1718$Timestamp <- as.Date(scores_ay1718$Timestamp)
#This went horribly wrong, don't do it again: scores_ay1718$Timestamp <- as.Date.POSIXct(scores_ay1718$Timestamp)
#Now looking at the first column in scores_ay1819, I can see there are three dates that don't work there either
#I will make these 2018-12-31 for the same reason as above
#Two of the dates look like they were proper testing dates in MAR, but I can't be totally certain
class(scores_ay1819$Timestamp)
scores_ay1819$Timestamp <- as.Date.POSIXct(scores_ay1819$Timestamp)
scores_ay1819$Timestamp <- as.Date(scores_ay1819$Timestamp)
scores_ay1819$Timestamp <- as.character(scores_ay1819$Timestamp)
#I don't know why, but the method I used last time didn't work this time
#throes an error: scores_ay1819[scores_ay1819$Timestamp=="2021-01-19", "Timestamp"] <- "2018-12-31"
#nope scores_ay1819$Timestamp["2021-01-19"]="2018-12-31"
scores_ay1819[scores_ay1819=="2021-01-19"] <- "2018-12-31"
scores_ay1819[scores_ay1819=="2017-03-27"] <- "2018-12-31"
scores_ay1819[scores_ay1819=="2718-03-01"] <- "2018-12-31"
scores_ay1819$Timestamp <- as.Date(scores_ay1819$Timestamp)

#Cleaning: Second column -- "Entering Grade Level"
class(scores_ay1718$`Entering Grade Level`)
class(scores_ay1819$`Entering Grade Level`)
#I would rather these be numeric and the kids entering kindergarten be recorded as entering grade "0"
#There are also some errors where someone wrote out "first" "second" etc
counts <- scores_ay1718$`Entering Grade Level`
barplot(prop.table(table(counts)))
counts[counts=="K"] <- "0"
counts[counts=="first"] <- "1"
counts[counts=="kindergarten"] <- "0"
counts[counts=="kinder"] <- "0"
counts[counts=="second"] <- "2"
counts<-as.numeric(counts)
scores_ay1718$`Entering Grade Level`<-counts
counts2 <- scores_ay1819$`Entering Grade Level`
barplot(prop.table(table(counts2)))
counts2[counts2=="K"] <- "0"
counts2[counts2=="k"] <- "0"
counts2[counts2=="Kindergarten"] <- "0"
counts2[counts2=="first"] <- "1"
counts2[counts2=="Kinder"] <- "0"
counts2<-as.numeric(counts2)
scores_ay1819$`Entering Grade Level`<-counts2

#Cleaning: Third column -- "District"
class(scores_ay1718$District)
class(scores_ay1819$District)
barplot(prop.table(table(scores_ay1819$District)))
#The district column in scores_ay1819 looks like it's probably good to go
#The district column in scores_ay1718, on the other hand... someone put "Anderson" down for one of the tests
#There are also a dozen or so "NA"s in this one; I'm going to see if they stay NA after I change it to numeric
#First, I need to figure out if I can make Anderson NA
district <- scores_ay1718$District
district[district=="Anderson"] <- ""
district <- as.numeric(district)
scores_ay1718$District<-district

#Cleaning: Fourth column -- "Birth Month"
class(scores_ay1718$`Birth Month`)
class(scores_ay1819$`Birth Month`)
#I'm mostly just looking for spelling errors here; I'm pretty sure I saw a couple during my initial skim
barplot(prop.table(table(scores_ay1718$`Birth Month`)))
scores_ay1718$`Birth Month`[scores_ay1718$`Birth Month`=="11"] <- "November"
scores_ay1718$`Birth Month`[scores_ay1718$`Birth Month`=="2"] <- "February"
scores_ay1718$`Birth Month`[scores_ay1718$`Birth Month`=="8"] <- "August"
barplot(prop.table(table(scores_ay1819$`Birth Month`)))
scores_ay1819$`Birth Month`[scores_ay1819$`Birth Month`=="11"] <- "November"
scores_ay1819$`Birth Month`[scores_ay1819$`Birth Month`=="2"] <- "February"
scores_ay1819$`Birth Month`[scores_ay1819$`Birth Month`=="12"] <- "December"
scores_ay1819$`Birth Month`[scores_ay1819$`Birth Month`=="Feb"] <- "February"
scores_ay1819$`Birth Month`[scores_ay1819$`Birth Month`=="september"] <- "September"
#There's a single "NA" value that I can't really do anything about

#Cleaning: Columns 5-8
#Okay, there's a lot of weird stuff going on here
#Ultimately, the raw score and the percentile should be equivalent to each other
#I'm going to check the columns and see if I can get away with dropping the raw scores
#Instead of cleaning all four columns (8 if you count them separately) and then circling back to deal with the errors
#Okay, so none of the math that I can see is making any sense here (13/30 != 55%)
#I have to trust that the data is mostly correct, so I'm going to drop the columns with the raw scores and see what I'm left with
scores_ay1718 <- subset(scores_ay1718, select = -c(`OLSAT Verbal Score`,`NNAT Non Verbal Raw Score`))
scores_ay1819 <- subset(scores_ay1819, select = -c(`OLSAT Verbal Score`,`NNAT Non Verbal Raw Score`))
#Now I just need to sort out the formatting issues in the percentile columns
barplot(prop.table(table(scores_ay1718$`OLSAT Verbal Percentile`)))
class(scores_ay1718$`OLSAT Verbal Percentile`)
scores_ay1718$`OLSAT Verbal Percentile`[scores_ay1718$`OLSAT Verbal Percentile`=="~70"] <- "70"
#I still can't really figure out the math that combines the verbal and nonverbal scores into the overall score
#Pretty much all of the "NA"s here result in an overall score of 99
#All of the overall scores with 99 that have scores listed in these columns are equal to 99
#So I'm going to change all of these values to 99 (I guess someone got lazy and figured they didn't need to put the individual scores in if they maxed it??)
scores_ay1718$`OLSAT Verbal Percentile`[scores_ay1718$`OLSAT Verbal Percentile`=="NA"] <- "99"
scores_ay1718$`NNAT Non Verbal Percentile`[scores_ay1718$`NNAT Non Verbal Percentile`=="NA"] <- "99"
#There are three scores in the scores_ay1718$`NNAT Non Verbal Percentile` column that look like they're supposed to be 99
#The reason I think this is because their overall scores were listed as 99
scores_ay1718$`NNAT Non Verbal Percentile`[scores_ay1718$`NNAT Non Verbal Percentile`=="9"] <- "99"
scores_ay1718$`NNAT Non Verbal Percentile`[scores_ay1718$`NNAT Non Verbal Percentile`=="0.99"] <- "99"
#Finally, I'll convert both of these into numeric
scores_ay1718$`OLSAT Verbal Percentile` <- as.numeric(scores_ay1718$`OLSAT Verbal Percentile`)
scores_ay1718$`NNAT Non Verbal Percentile` <- as.numeric(scores_ay1718$`NNAT Non Verbal Percentile`)
#Now to do all the same stuff to scores_ay1819
class(scores_ay1819$`OLSAT Verbal Percentile`)
class(scores_ay1819$`NNAT Non Verbal Percentile`)
#So there's a single "NA" that looks like it's probably supposed to be a 99, and an entry of "0.91" that should be 91
scores_ay1819$`OLSAT Verbal Percentile`[scores_ay1819$`OLSAT Verbal Percentile`=="0.91"] <- "91"
scores_ay1819$`OLSAT Verbal Percentile`[scores_ay1819$`OLSAT Verbal Percentile`==NA] <- "99"
#This NA is giving me some trouble
#I did an oops, and had to re-run all my code so I could get back to where I was
#I'm going to convert all the decimals into whole numbers in the NNAT column
scores_ay1819$`NNAT Non Verbal Percentile`[scores_ay1819$`NNAT Non Verbal Percentile`==0.71] <- 71
scores_ay1819$`NNAT Non Verbal Percentile`[scores_ay1819$`NNAT Non Verbal Percentile`==0.98] <- 98
scores_ay1819$`NNAT Non Verbal Percentile`[scores_ay1819$`NNAT Non Verbal Percentile`==0.99] <- 99
#Mathematically, these 0 are most likely 99s
scores_ay1819$`NNAT Non Verbal Percentile`[scores_ay1819$`NNAT Non Verbal Percentile`==0] <- 99
#Okay, now let's try this NA thing again
#Here are the notes from the last time that botched it(?)
#NOPE scores_ay1819$`NNAT Non Verbal Percentile`[is.na(scores_ay1819$`NNAT Non Verbal Percentile`)] = 0
#That is not what I wanted to do... where's the undo button?
#And now let's deal with it
#But now I can't find the NA
barplot(prop.table(table(scores_ay1819$`NNAT Non Verbal Percentile`)))
#Nevermind??? T_T
scores_ay1819$`OLSAT Verbal Percentile` <- as.numeric(scores_ay1819$`OLSAT Verbal Percentile`)

#Clean: Overall Scores
#Three of the overall scores came out to zero
#One has a verbal score of 97 and a non-verbal score of 99; other students with these scores came out to an overall score of 98
#One has a verbal score of 96 and a non-verbal score of 98; also computes to 98
#The last one has a verbal score of 86 and a non-verbal score of 99; this computes to 96 T_T
#So I need to isolate them this time, I can't just blanket change them
#I'm going to make a df with just those three lines
slice = scores_ay1718[scores_ay1718$`Overall Score` == 0,]
#I'm going to drop those lines from scores_1718
scores_ay1718 <- filter(scores_ay1718, `Overall Score`!=0)
#Now I'll just replace the Overall Score with the correct vector c(98,96,98)
slice$`Overall Score` <- c(98,96,98)
#And now I'll just join the two datasets again
scores_ay1718 <- rbind(scores_ay1718,slice)

#I'm not super interested in where the student/parents want the student to attend, but I'll leave them for now

#If there is more than one datafile, combine the two files into one datafile (5pts)
#Okay, now it's time to combine the two dfs
#Since I reformatted the columns to the type I wanted them to be as I went, this should just work the first time
#Wait, I got to the visuals piece below and decided to change something
scores_ay1718$Year <- 2017
scores_ay1819$Year <- 2018
class(scores_ay1718$Year)
scores <- rbind(scores_ay1718,scores_ay1819)


#Include visuals of at least two variables (10pts)
#I've already done a lot of bar plots, but I'll crank out a few more to be thorough
#Let's look at a comparison of overall scores between the two years
plot1 <- table(scores$Year, scores$`Overall Score`)
barplot(plot1, main="Overall Score Distribution by Year",
        xlab="Percentile", col=c("darkblue","red"),
        ylab="Frequency",
        legend.text = rownames(plot2),
        args.legend = list(x = "topleft",
                           inset = c(0.1, 0)))
#Or perhaps a side-by-side would do better
plot2 <- table(scores$Year, scores$`Overall Score`)
barplot(plot2, main="Overall Score Distribution by Year",
        xlab="Percentile", col=c("darkblue","red"),
        ylab="Frequency",
        legend.text = rownames(plot2), beside=TRUE,
        args.legend = list(x = "topleft",
                           inset = c(0.1, 0)))
#I've never done a pie chart before, let's do one of those
scores99 <- filter(scores, `Overall Score`==99)
scores98 <- filter(scores, `Overall Score`==98)
scores97 <- filter(scores, `Overall Score`==97)
scores96 <- filter(scores, `Overall Score`==96)
scores95 <- filter(scores, `Overall Score`==95)
plot3 <- data.frame(
  group = c(99, 98, 97, 96, 95),
  value = c(nrow(scores99),nrow(scores98),nrow(scores97),nrow(scores96),nrow(scores95)))
ggplot(plot3, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
#Okay, the labels aren't quite right, but it's pretty close


#Include comments to describe what is being done (15pts)
#I think I did a lot of this above

#At the end:
#Upload script file and cleaned dataset to GitHub
#I need to export my cleaned dataset to do this
write.csv(scores,"C:/Users/melin/OneDrive/Desktop/Data Analytics M.S/ANA515/ANA515 R Working Directory/clean G&T scores.csv", row.names = FALSE)

#Include a descriptive readme file in the repository (5pts)
#Upload the original datasets, include a comment when you commit
#In the comment box with your submission, include a screenshot of your github contribution activity (5pts)
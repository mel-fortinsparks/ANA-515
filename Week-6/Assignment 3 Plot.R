#1. Read and save the dataset
#The first thing I need to do is load in the data
#The file I have is a csv.gz, so I need to use readr from tidyverse
library(tidyverse)
stormevents <- read.csv("C:/Users/melin/OneDrive/Desktop/Data Analytics M.S/ANA515/ANA515 R Working Directory/StormEvents_details-ftp_v1.0_d1990_c20210803.csv.gz")
#I have read the file into R and saved it as "stormevents"

#2. Limit the dataset
#Limit the df to the "following columns"
colnames(stormevents)
head(stormevents)
storm_simp <- subset(stormevents, select = c(BEGIN_YEARMONTH,END_YEARMONTH,BEGIN_DATE_TIME,END_DATE_TIME,EPISODE_ID,EVENT_ID,STATE,STATE_FIPS,CZ_NAME,CZ_TYPE,CZ_FIPS,EVENT_TYPE,SOURCE,BEGIN_LAT,BEGIN_LON,END_LAT,END_LON))

#3. Format the date/time
#Arrange the data beginning year and month(BEGIN_YEARMONTH)
#I added the BEGIN_DATE_TIME for sorting because I wanted them in proper chronological order.
storm_simp<-arrange(storm_simp,BEGIN_YEARMONTH,BEGIN_DATE_TIME)

#4. Modify name case
#I need to make all the states in the STATE column so that only the first letter of each word is capital
storm_simp$STATE <- str_to_title(storm_simp$STATE)
storm_simp$CZ_NAME <- str_to_title(storm_simp$CZ_NAME)

#5. Limit events
#I need to limit the events to C type and then drop the CZ_TYPE column
storm_simp_c <- filter(storm_simp, CZ_TYPE=="C")
storm_simp_c <- subset(storm_simp_c, select = -c(CZ_TYPE))

#6. Pad the FIPs
#First add the 0s to the left for STATE_FIPS and CZ_FIPS
storm_simp_c$STATE_FIPS <- str_pad(storm_simp_c$STATE_FIPS, width=3, side="left", pad="0")
storm_simp_c$CZ_FIPS <- str_pad(storm_simp_c$CZ_FIPS, width=3, side="left", pad="0")
#Then combine STATE_FIPS and CZ_FIPS columns
storm_simp_c$FIPS <- str_c(storm_simp_c$STATE_FIPS,"",storm_simp_c$CZ_FIPS)

#7. Modify all column names
#Rename all the column names to lowercase
storm_simp_c <- rename_all(storm_simp_c, tolower)

#8. Create DF
#Create a dataframe with state
data("state")
state_df <- data.frame(state=state.name, area=state.area, region=state.region)

#9. Merge DF
Newset <- data.frame(table(storm_simp_c$state))
Newset <- rename(Newset, c("state"="Var1"))
merged <- merge(x=Newset,y=state_df,by.x="state", by.y="state")
#It seems there were no storms in two states during 1990 and the state df doesn't include DC, so we end with 48 observations
#I did not have the title case issue :/

#10. Plot
library(ggplot2)
storm_plot <- ggplot(merged, aes(x=area, y = Freq)) +
  geom_point(aes(color=region)) +
  labs(x="Land area (square miles)",
       y="# of storm events in 1990")
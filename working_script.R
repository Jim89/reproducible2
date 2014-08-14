library(dplyr,quietly=T)
library(ggplot2,quietly=T)
library(R.utils,quietly=T)
library(reshape2,quietly=T)
library(magrittr,quietly=T)
library(sqldf,quietly=T)


################################################################################
# Step 1 - Getting and loading the data
################################################################################
# 1a. Set the URL for the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# 1b. Conditionally create a directory in the working directory to store the data
if(!file.exists("./data")){dir.create("./data",showWarnings = FALSE)}

# 1c. Download and unzip the data - download method conditional on operating system
if(Sys.info()["sysname"]=="Windows"){setInternet2(use = TRUE)}
if(!file.exists("./data/repdata_data_StormData.csv.bz2"))
{
  ifelse(Sys.info()["sysname"]=="Windows",
         download.file(url=url,"./data/repdata_data_StormData.csv.bz2",quiet=TRUE),
         download.file(url=url,"./data/repdata_data_StormData.csv.bz2",method="curl",quiet=TRUE)
  )
bunzip2("./data/repdata_data_StormData.csv.bz2",
        "./data/repdata-data-StormData.csv", overwrite = TRUE, remove = FALSE)
}
# 1d. Read in the data
data<-read.csv("./data/repdata-data-StormData.csv",
               header=T,
               stringsAsFactors=F
               )

################################################################################
# Step 2 - Process the data
################################################################################
# 2a. make data field names lower case
names(data) <- tolower(names(data))

# 2b. make evtype values all lower
data$evtype <- tolower(data$evtype)


# look at the top 20 types
ev_sum <- data %.%
  select(evtype,fatalities,injuries,propdmg,cropdmg) %.%
  group_by(evtype) %.%
  summarise(affected=sum(fatalities,injuries)) %.%
  arrange(-affected)
head(ev_sum,20)
# looks like within the top 20 there are several duplicates  - have to clean up


# property
unique(data$propdmgexp)
sqldf("select distinct propdmgexp, count(*) from data group by propdmgexp order by 2 desc")

# 2b. replace some of the event types to clean up data
data$evtype_clean <-  ifelse(data$evtype=="tstm wind","high wind",
                      ifelse(data$evtype=="heat","excessive heat",
                      ifelse(data$evtype=="flood","floods",       
                      ifelse(data$evtype=="flash flood","floods",
                      ifelse(data$evtype=="ice storm","winter storm",
                      ifelse(data$evtype=="thunderstorm wind","high wind",
                      ifelse(data$evtype=="hail","winter storm",
                      ifelse(data$evtype=="heavy snow","winter storm",
                      ifelse(data$evtype=="thunderstorm winds","high wind",
                      ifelse(data$evtype=="blizzard","winter storms",
                      ifelse(data$evtype=="wild/forest fire","wildfire",
                      ifelse(data$evtype=="strong wind","high wind",
                             data$evtype))))))))))))
# 2d. make all uppercase
data$evtype_clean <- toupper(data$evtype_clean)


ev_sum <- data %.%
          select(evtype_clean,fatalities,injuries) %.%
          group_by(evtype_clean) %.%
          summarise(deaths=sum(fatalities),
                    injuries=sum(injuries),
                    affected=sum(deaths,injuries)) %.%
          arrange(-affected) %.%
          transform(evtype_clean = reorder(evtype_clean,order(affected,decreasing=T))) %.%
          select(-affected)

ev_sum <- melt(ev_sum[1:15,],
               id.vars="evtype_clean",
               variable.name="efftype",
               value.name="people")
ev_sum$efftype <- factor(ev_sum$efftype,levels=c("deaths","injuries"))


options(scipen=100)
ggplot(data=ev_sum,aes(x=evtype_clean,y=people,fill=efftype))+
geom_bar(stat="identity")+
theme_minimal()+
scale_fill_manual(values=c("firebrick","steelblue"),
                  name = element_blank(),
                  breaks = c("deaths","injuries"),
                  labels=c("Deaths","Injuries"))+
ylab("Individuals Affected")+
scale_y_continuous(breaks=seq(
                  from=0,to=(max(ev_sum$people)+0.1*max(ev_sum$people)),by=10000)
                  )+
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x=element_blank(),
      legend.position="bottom",
      axis.title.x=element_blank()
      )






 
# blob <- sqldf("select distinct evtype_clean, count(*) from data group by evtype order by 2 desc")
# head(blob,20)









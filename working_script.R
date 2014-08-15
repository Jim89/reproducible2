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

# ################################################################################
# # Step 1b - initial explorations
# ################################################################################
# nrow(data)
#
# # events - find all the values of event type
# data %.%
#   group_by(evtype) %.%
#   summarise(count = n(),
#   prop = 100*count/nrow(data)) %.%
#   arrange(-count) %.%
#   head(25)
#
# # property - find all the values of property exponents
# data %.%
#   group_by(propdmgexp) %.%
#   summarise(count = n()) %.%
#   arrange(-count)
# # looks like I need to account for values K, M, B, m
#
# # crops - find all the values of property exponents
# data %.%
#   group_by(cropdmgexp) %.%
#   summarise(count = n()) %.%
#   arrange(-count)
# # looks like I need to account for values K, M, k, B, m


################################################################################
# Step 2 - Process the data
################################################################################
# 2a. make data field names lower case
names(data) <- tolower(names(data))

# 2b. make evtype values all lower
data$evtype <- tolower(data$evtype)

# 2c. replace some of the event types to clean up data
data$evtype_clean <- ifelse(data$evtype=="tstm wind","high wind",
                     ifelse(data$evtype=="heat","excessive heat",
                     ifelse(data$evtype=="flood","floods",
                     ifelse(data$evtype=="flash flood","floods",
                     ifelse(data$evtype=="ice storm","winter storm",
                     ifelse(data$evtype=="thunderstorm wind","high wind",
                     ifelse(data$evtype=="hail","winter storm",
                     ifelse(data$evtype=="heavy snow","winter storm",
                     ifelse(data$evtype=="thunderstorm winds","high wind",
                     ifelse(data$evtype=="blizzard","winter storm",
                     ifelse(data$evtype=="wild/forest fire","wildfire",
                     ifelse(data$evtype=="strong wind","high wind",
                     ifelse(data$evtype=="winter storms","winter storm",
                     ifelse(data$evtype=="storm surge","floods",
                     ifelse(grep(data$evtype=="hurricane","hurricane/typhoon",
                     ifelse(data$evtype=="river flood","floods",
                     ifelse(data$evtype=="tropical storm","hurricane/typhoon",
                     ifelse(data$evtype=="heavy rains/severe weather","winter storm",
                     ifelse(data$evtype=="heavy rain/severe weather","winter storm",
                     ifelse(data$evtype=="storm surge/tide","floods",
                     ifelse(data$evtype=="hurricane opal","hurricane/typhoon",
                     data$evtype)))))))))))))))))))))

# 2d. make all uppercase
data$evtype_clean <- toupper(data$evtype_clean)

# 2e. recode property damage exponenets
data$propexp_clean <- ifelse(data$propdmgexp=="K",1E3,
                      ifelse(data$propdmgexp=="M",1E6,
                      ifelse(data$propdmgexp=="B",1E9,
                      ifelse(data$propdmgexp=="m",1E6,
                      ifelse(data$propdmgexp=="5",1E5,
                      ifelse(data$propdmgexp=="2",1E2,
                      ifelse(data$propdmgexp=="7",1E7,
                      ifelse(data$propdmgexp=="3",1E3,
                      ifelse(data$propdmgexp=="4",1E4,
                      ifelse(data$propdmgexp=="6",1E6,
                      ifelse(data$propdmgexp=="8",1E8,1)))))))))))

data$cropexp_clean <- ifelse(data$cropdmgexp=="K",1E3,
                      ifelse(data$cropdmgexp=="k",1E3,
                      ifelse(data$cropdmgexp=="M",1E6,
                      ifelse(data$cropdmgexp=="m",1E6,
                      ifelse(data$cropdmgexp=="B",1e9,
                      ifelse(data$cropdmgexp=="2",1E2,1))))))

# 2f. esitmation of human value (source: en.wikipedia.org/wiki/Value_of_life)
data$human_cost <- rep(7E6,nrow(data))


################################################################################
# Step 2 - Summarise by Injury/Death
################################################################################
ev_sum <- data %.%
  select(evtype_clean,fatalities,injuries) %.%
  group_by(evtype_clean) %.%
  summarise(deaths=sum(fatalities),
            injuries=sum(injuries),
            affected=sum(deaths,injuries)) %.%
  arrange(-affected) %.%
  transform(evtype_clean = reorder(evtype_clean,order(affected,decreasing=T))) %.%
  select(-affected)

ev_sum <- melt(ev_sum[1:10,],
               id.vars="evtype_clean",
               variable.name="efftype",
               value.name="people")
ev_sum$efftype <- factor(ev_sum$efftype,levels=c("deaths","injuries"))

options(scipen=100)
ggplot(data=ev_sum,aes(x=evtype_clean,y=people,fill=efftype))+
geom_bar(stat="identity")+
theme_minimal()+
scale_fill_manual(values=c("firebrick","chocolate"),
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


################################################################################
# Step 2 - Summarise by property/crop damage
################################################################################
ev_sum_eco <- data %.%
              select(evtype_clean,propdmg,propexp_clean,cropdmg,cropexp_clean) %.%
              group_by(evtype_clean) %.%
              summarise(property=sum(propdmg*propexp_clean),
                        crop=sum(cropdmg*cropexp_clean),
                        total_cost=sum(property,crop)) %.%
              arrange(-total_cost) %.%
              transform(evtype_clean = reorder(evtype_clean,order(total_cost,decreasing=T))) %.%
              select(-total_cost)

ev_sum_eco <- melt(ev_sum_eco[1:10,],
               id.vars="evtype_clean",
               variable.name="dmgtype",
               value.name="cost")
ev_sum$efftype <- factor(ev_sum$efftype,levels=c("crop","property"))

options(scipen=100)
ggplot(data=ev_sum_eco,aes(x=evtype_clean,y=(cost/1E6),fill=dmgtype))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(values=c("yellow3","olivedrab4"),
                    name = element_blank(),
                    breaks = c("crop","property"),
                    labels=c("Crop Damage","Property Damage"))+
  ylab("Total Cost m$")+
  scale_y_continuous(breaks=seq(
    from=0,to=(max(ev_sum_eco$cost)+0.1*max(ev_sum_eco$cost)),by=10000)
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x=element_blank(),
        legend.position="bottom",
        axis.title.x=element_blank()
  )









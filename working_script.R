################################################################################
# Step 0 - Admin - load packages and create directories
################################################################################
# 0a - conditionally install and then load all packages
# 0ai. dplyr
if(require("dplyr",quietly=T)){
  print("loading dplyr")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  library("dplyr",quietly=T)
  print("dplyr installed and loaded")
  if(require("dplyr",quietly=T)){
  } else {
    stop("could not install dplyr")
  }
}
# 0aii. ggplot2
if(require("ggplot2",quietly=T)){
  print("loading ggplot2")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  library("ggplot2",quietly=T)
  print("ggplot2 installed and loaded")
  if(require("ggplot2",quietly=T)){
  } else {
    stop("could not install ggplot2")
  }
}
# 0aiii. R.utils
if(require("R.utils",quietly=T)){
  print("loading R.utils")
} else {
  print("trying to install R.utils")
  install.packages("R.utils")
  library("R.utils",quietly=T)
  print("R.utils installed and loaded")
  if(require("R.utils",quietly=T)){
  } else {
    stop("could not install R.utils")
  }
}
# 0aiv. reshape2
if(require("reshape2",quietly=T)){
  print("loading reshape2")
} else {
  print("trying to install reshape2")
  install.packages("reshape2")
  library("reshape2",quietly=T)
  print("reshape2 installed and loaded")
  if(require("reshape2",quietly=T)){
  } else {
    stop("could not install reshape2")
  }
}

# 0b. Conditionally create a directory in the working directory to store data
if(!file.exists("./data")){dir.create("./data",showWarnings = FALSE)}

# 0c. Conditionally create a directory to store plots
if(!file.exists("./plots")){dir.create("./plots",showWarnings = FALSE)}

################################################################################
# Step 1 - Getting and loading the data
################################################################################
# 1a. Set the URL for the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# 1b. Download and unzip the data - conditional on operating system
if(Sys.info()["sysname"]=="Windows"){setInternet2(use = TRUE)}
if(!file.exists("./data/repdata_data_StormData.csv.bz2"))
{
  ifelse(Sys.info()["sysname"]=="Windows",
         download.file(url,"./data/repdata_data_StormData.csv.bz2",quiet=TRUE),
         download.file(url,"./data/repdata_data_StormData.csv.bz2",method="curl"
                       ,quiet=TRUE)
  )
  bunzip2("./data/repdata_data_StormData.csv.bz2",
          "./data/repdata-data-StormData.csv", overwrite = TRUE, remove = FALSE)
}
# 1c. Read in the data
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
                     ifelse(data$evtype=="hurricane","hurricane/typhoon",
                     ifelse(data$evtype=="river flood","floods",
                     ifelse(data$evtype=="tropical storm","hurricane/typhoon",
                     ifelse(data$evtype=="heavy rains/severe weather",
                                          "winter storm",
                     ifelse(data$evtype=="heavy rain/severe weather",
                                          "winter storm",
                     ifelse(data$evtype=="storm surge/tide","floods",
                     ifelse(data$evtype=="hurricane opal","hurricane/typhoon",
                     ifelse(data$evtype=="tornadoes, tstm wind, hail",
                                          "winter storm",
                     ifelse(data$evtype=="severe thunderstorm","winter storm",
                     ifelse(data$evtype=="frost/freeze","extreme cold",
                     ifelse(data$evtype=="marine tstm wind","high wind",
                     ifelse(data$evtype=="winter weather","winter storm",
                     ifelse(data$evtype=="marine thunderstorm wind","high wind",
                     data$evtype)))))))))))))))))))))))))))

# 2d. make all uppercase
data$evtype_clean <- toupper(data$evtype_clean)

# # # events - find all the values of event type
# data %.%
#   group_by(evtype_clean) %.%
#   summarise(count = n(),
#   prop = 100*count/nrow(data)) %.%
#   arrange(-count) %.%
#   head(15)

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
# Step 3 - Summarise by Injury/Death
################################################################################
ev_sum <- data %.%
          select(evtype_clean,fatalities,injuries) %.%
          group_by(evtype_clean) %.%
          summarise(deaths=sum(fatalities),
                    injuries=sum(injuries),
                    affected=sum(deaths,injuries)) %.%
          arrange(-affected) %.%
          transform(evtype_clean =
                      reorder(evtype_clean,order(affected,decreasing=T))) %.%
          select(-affected)

ev_sum <- melt(ev_sum[1:10,],
               id.vars="evtype_clean",
               variable.name="efftype",
               value.name="people")

ev_sum$efftype <- factor(ev_sum$efftype,levels=c("deaths","injuries"))

options(scipen=100)
png("./plots/human_cost.png",height=960,width=960)
ggplot(data=ev_sum,aes(x=evtype_clean,y=people,fill=efftype))+
geom_bar(stat="identity")+
theme_minimal()+
scale_fill_manual(values=c("firebrick2","firebrick"), name = element_blank(),
                 breaks = c("deaths","injuries"),labels=c("Deaths","Injuries"))+
ylab("Individuals Affected")+
ggtitle("Total human cost per event type")+
scale_y_continuous(breaks=seq(from=0,
                              to=(max(ev_sum$people)+0.1*max(ev_sum$people)),
                              by=10000))+
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position="bottom",
      axis.title.x=element_blank())
dev.off()

################################################################################
# Step 4 - Summarise by property/crop damage
################################################################################
ev_sum_eco <- data %.%
              select(evtype_clean,
                     propdmg,
                     propexp_clean,
                     cropdmg,
                     cropexp_clean) %.%
              group_by(evtype_clean) %.%
              summarise(property=sum(propdmg*propexp_clean),
                        crop=sum(cropdmg*cropexp_clean),
                        total_cost=sum(property,crop)) %.%
              arrange(-total_cost) %.%
              transform(evtype_clean =
                          reorder(evtype_clean,order(total_cost,decreasing=T))) %.%
              select(-total_cost)

ev_sum_eco <- melt(ev_sum_eco[1:10,],
               id.vars="evtype_clean",
               variable.name="dmgtype",
               value.name="cost")

ev_sum$efftype <- factor(ev_sum$efftype,levels=c("crop","property"))

ev_sum_eco$cost_clean <- ev_sum_eco$cost/1E6

options(scipen=100)
png("./plots/property_cost.png",height=960,width=960)
ggplot(data=ev_sum_eco,aes(x=evtype_clean,y=cost_clean,fill=dmgtype))+
geom_bar(stat="identity")+
theme_minimal()+
scale_fill_manual(values=c("goldenrod","gold"),name = element_blank(),
                  breaks = c("crop","property"),
                  labels=c("Crop Damage","Property Damage"))+
ylab("m$")+
ggtitle("Total property cost per event type")+
scale_y_continuous(breaks=seq(from=0,
                              to=(max(ev_sum_eco$cost)+0.1*max(ev_sum_eco$cost)),
                              by=25000))+
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x=element_blank(),
      legend.position="bottom",
      axis.title.x=element_blank())
dev.off()

################################################################################
# Step 5 - Summarise by both
################################################################################
ev_sum_both <- data %.%
               select(evtype_clean,
                      injuries,
                      fatalities,
                      human_cost,
                      propdmg,
                      propexp_clean,
                      cropdmg,
                      cropexp_clean) %.%
                group_by(evtype_clean) %.%
                summarise(
                          human_cost = sum((fatalities*human_cost)+(0.5*(injuries*human_cost))),
                          eco_cost = sum((propdmg*propexp_clean)+(cropdmg*cropexp_clean)),
                          total_cost = sum(human_cost,eco_cost)
                          ) %.%
                arrange(-total_cost) %.%
                transform(evtype_clean =
                            reorder(evtype_clean,order(total_cost,decreasing=T))) %.%
                select(-total_cost)

ev_sum_both <- melt(ev_sum_both[1:10,],
                   id.vars="evtype_clean",
                   variable.name="efftype",
                   value.name="cost")

ev_sum_both$efftype <- factor(ev_sum_both$efftype,levels=c("human_cost","eco_cost"))

ev_sum_both$cost_clean <- ev_sum_both$cost/1E6

options(scipen=100)
png("./plots/cost_combined.png",height=960,width=960)
ggplot(data=ev_sum_both,aes(x=evtype_clean,y=cost_clean,fill=efftype))+
geom_bar(stat="identity")+
theme_minimal()+
scale_fill_manual(values=c("firebrick","goldenrod"),
                  name = element_blank(),
                  breaks = c("human_cost","eco_cost"),
                  labels=c("Human Cost","Property Cost"))+
ylab("m$")+
ggtitle("Total combined human and property cost per event type")+
scale_y_continuous(breaks=seq(
  from=0,to=(max(ev_sum_both$cost)+0.1*max(ev_sum_both$cost)),by=25000)
)+
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x=element_blank(),
      legend.position="bottom",
      axis.title.x=element_blank()
)
dev.off()





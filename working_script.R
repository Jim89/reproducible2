library(dplyr,quietly=T)
library(ggplot2,quietly=T)
library(R.utils,quietly=T)
library(reshape2,quietly=T)
library(magrittr,quietly=T)
library(sqldf,quietly=T)

dir<-"C:/Users/Jleach1/Documents/R/reproducible2"
setwd(dir)

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

################################################################################
# Initial explorations
################################################################################
ev_sum <- data %.%
          select(evtype,fatalities,injuries) %.%
          group_by(evtype) %.%
          summarise(deaths=sum(fatalities),
                    injuries=sum(injuries),
                    affected=sum(deaths,injuries)) %.%
          arrange(-affected) %.%
          transform(evtype = reorder(evtype,order(affected,decreasing=T))) %.%
          select(-affected)

ev_sum <- melt(ev_sum[1:20,],
               id.vars="evtype",
               variable.name="efftype",
               value.name="people")
ev_sum$efftype <- factor(ev_sum$efftype,levels=c("deaths","injuries"))
# summary(ev_sum)

ev_sum_tot <- ev_sum %.%
              select(evtype,people) %.%
              group_by(evtype) %.%
              summarise(affected=sum(people))



ggplot(data=ev_sum,aes(x=evtype,y=people,fill=efftype))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(values=c("firebrick","steelblue"),
                    name = element_blank(),
                    breaks = c("deaths","injuries"),
                    labels=c("Deaths","Injuries"))+
  ylab("Individuals Affected")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x=element_blank(),
        legend.position="bottom",
        axis.title.x=element_blank()
        )

















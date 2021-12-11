#Reset data
rm(list =ls())

#Load Libraries
library(readr)
library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tabulizer)
#Load member data
members <- read.csv("Members.csv")
members
#Remove columns
members$Location <- NULL
members$'Looking.or.Hiring.' <- NULL
members$'Notes' <- NULL
members$'Left.group.on' <- NULL                      
members
#Histogram of member/attendance
qplot(TotalAttendance,
      data = members,
      geom = "histogram",
      binwidth = 1) 

#Members by company
Company_order <-members %>%
  count(Company) %>% 
  arrange(desc(n))
Company_order <- subset(Company_order, Company != "")
top10Company_order <-Company_order[1:10,]

#Member by company (visualization)
par(las=1)
par(mar=c(5,14,4,2))
p<-barplot(top10Company_order$n,names.arg=top10Company_order$Company,xlab="",ylab="",col="gold",
           main="Top 10 companies in DSM Group",horiz=TRUE,border="red")
p

#Member by categorization (job title)
Categorization_order <-members %>%
  count(Categorization) %>% 
  arrange(desc(n))
Categorization_order <- subset(Categorization_order, Categorization != "")
top10Categorization_order <-Categorization_order[1:10,]

#Member by linkedin title (job title)
Linkedin_order <-members %>%
  count(LinkedIn.Title) %>% 
  arrange(desc(n))
Linkedin_order <- subset(Linkedin_order, LinkedIn.Title != "")
top10Linkedin_order <-Linkedin_order[1:10,]

#Member by linkedin title (visualization)
par(las=1)
par(mar=c(5,10,4,2))
p<-barplot(top10Linkedin_order$n,names.arg=top10Linkedin_order$LinkedIn.Title,xlab="",ylab="",col="gold",
           main="Top 10 Job title",horiz=TRUE,border="red")
p

#When did member join the DSM Group?
#Clean data
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

joindate<-members$Joined.Group.on
joindate <- group_by(members, Joined.Group.on)
joindate <- subset(joindate, Joined.Group.on != "")
joindate <-count(joindate)
joindate$Joined.Group.on <- as.Date(joindate$Joined.Group.on)
str(joindate)

p <- joindate %>%
  ggplot(aes(x=Joined.Group.on, y=n)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Member count")+
  xlab("Date")
p <- ggplotly(p)
p


#joindate<-substr(joindate,1,7)
#head(joindate)


############################################################################################################
#Load attendance data
attendance <- read.csv("Attendance.csv")
attendance

#Review most active members
attendance_order <-attendance %>%
  count(MemberName) %>% 
  arrange(desc(n))
attendance_order <- subset(attendance_order, MemberName != "")

############################################################################################################
#Load events of data
events <- read.csv("Events.csv")
#Remove columns
events$EventDescription <- NULL
events$EventURL <- NULL
events$EventLocation <- NULL
events$Event.type <- NULL
events$EventAddress <- NULL
events$Event_type <- events$Allison.s.Event.Type
events$Allison.s.Event.Type <- NULL
events
#Create bar graph of popular events 




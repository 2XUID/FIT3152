library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(gridExtra)
library(grid)
library(lattice)
library(reshape2)
library(corrplot)
library(RColorBrewer)
library(igraph)
library(igraphdata)
library(data.table)
#set up path
#setwd("/Users/mac/My Drive/Documents/Assignment/2-SEM_1/FIT3152/A1 (20%)")
setwd("C:/Users/aud/My Drive/Documents/Assignment/2-SEM_1/FIT3152/A1 (20%)")
#read data
rm(list = ls())
set.seed(30874157)
webforum <- read.csv("webforum.csv")
webforum <- webforum [sample(nrow(webforum), 20000), ] # 20000 rows
webforum$Year_and_month <- format(as.Date(webforum$Date), "%y-%m")
str(webforum)
#data tidy
#check if NA value and datatype
webforum<-na.omit(webforum)
webforum<-webforum%>%distinct()
str(webforum)
#turn date from char to date
webforum$Date <- as.Date(webforum$Date)
#make new columns with year, month and day
webforum_with_date<-webforum%>%
  mutate(
    year = year(Date),
    month = month(Date),
    day = day(Date)
  )
# clean Author ID is -1
webforum_with_date <- webforum_with_date[!(webforum_with_date$AuthorID == -1),]
# Clean post which word count is 0
webforum_with_date <- webforum_with_date[!(webforum_with_date$WC == 0),]
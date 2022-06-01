
#AQ1
#make a data frame with year column and month column
AQ1_dataframe<-webforum_with_date%>%group_by(year,month)%>%summarise(count =n())
#make a data frame about the year and month
long_term_dataframe<-AQ1_dataframe%>%mutate(date = make_date(year, month))
#graph create
ggplot(long_term_dataframe,
       aes(x <- date, 
           y <- count)) +  
  labs(
    title = "Post over the longer term",
    subtitle = "(2002-2011)",
    x = "Year",
    y = "Post Number"
  )+
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x)

#Time
AQ1_time<-webforum_with_date%>%group_by(Time)%>%summarise(count =n())
ggplot(AQ1_time,
       aes(x <- Time, 
           y <- count)) +  
  labs(
    title = "Active participants over the time",
    x = "Time",
    y = "Post Count"
  )+
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x)


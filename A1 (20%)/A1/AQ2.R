#AQ2
AQ2_dataframe = webforum_with_date%>%group_by(year,month)%>% 
  summarise(count=n(),
            WC,Analytic,Clout,Authentic,Tone,ppron,i,we,you,shehe,they,posemo,negemo,anx,
            anger,sad,focuspast,focuspresent,focusfuture 
  )
AQ2_dataframe[,5:19] <- AQ2_dataframe[,5:19]*(AQ2_dataframe$WC/100)
AQ2_dataframe$Date= as.yearmon(paste(AQ2_dataframe$year, AQ2_dataframe$month), "%Y %m")
AQ2_dataframe<-AQ2_dataframe%>%group_by(Date)%>%summarise(
  WC = mean(WC,na.rm = TRUE),
  Analytic = mean(Analytic,na.rm = TRUE),
  Clout = mean(Clout,na.rm = TRUE),
  Authentic = mean(Authentic,na.rm = TRUE),  
  Tone = mean(Tone, na.rm = TRUE),
  ppron = mean(ppron, na.rm = TRUE),
  i = mean(i,na.rm = TRUE),
  we = mean(we,na.rm = TRUE),
  you = mean(you,na.rm = TRUE),
  shehe = mean(shehe,na.rm = TRUE),
  they = mean(they,na.rm = TRUE),
  posemo = mean(posemo,na.rm = TRUE),
  negemo = mean(negemo,na.rm = TRUE),
  anx = mean(anx,na.rm = TRUE),
  anger = mean(anger,na.rm = TRUE),
  sad = mean(sad,na.rm = TRUE),
  focuspast = mean(focuspast,na.rm = TRUE),
  focuspresent = mean(focuspresent,na.rm = TRUE),
  focusfuture = mean(focusfuture,na.rm = TRUE),
)


#Analytic 
# Calculate the Graph
Analytic_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=Analytic)) + 
  labs(
    title = "Trend of Analytic thinking over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "Analytic")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))


#Clout 
# Calculate the Graph
Clout_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=Clout)) + 
  labs(
    title = "Trend of Clout over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "Clout")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

#Authentic 
# Calculate the Graph
Authentic_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=Authentic)) + 
  labs(
    title = "Trend of Authentic over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "Authentic")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

#Tone 
# Calculate the Graph
Tone_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=Tone)) + 
  labs(
    title = "Trend of Tone over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "Tone")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))
grid.arrange(Analytic_plot, Clout_plot, Authentic_plot, Tone_plot)

#ppron 
# Calculate the Graph
ppron_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=ppron)) + 
  labs(
    title = "Trend of ppron over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "ppron")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

i_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=i)) + 
  labs(
    title = "Trend of i over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "i")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

we_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=we)) + 
  labs(
    title = "Trend of we over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "we")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

you_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=you)) + 
  labs(
    title = "Trend of you over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "you")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

shehe_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=shehe)) + 
  labs(
    title = "Trend of shehe over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "shehe")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

they_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=they)) + 
  labs(
    title = "Trend of they over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "they")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))
grid.arrange(ppron_plot,i_plot,we_plot, you_plot, shehe_plot, they_plot)
#emotion
posemo_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=posemo)) + 
  labs(
    title = "Trend of posemo over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "posemo")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

negemo_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=negemo)) + 
  labs(
    title = "Trend of negemo over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "negemo")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

anx_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=anx)) + 
  labs(
    title = "Trend of anx over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "anx")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

anger_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=anger)) + 
  labs(
    title = "Trend of anger over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "anger")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

sad_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=sad)) + 
  labs(
    title = "Trend of sad over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "sad")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))
grid.arrange(posemo_plot,negemo_plot, anx_plot, anger_plot, sad_plot)
#focus
focuspast_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=focuspast)) + 
  labs(
    title = "Trend of focuspast over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "focuspast")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

focuspresent_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=focuspresent)) + 
  labs(
    title = "Trend of focuspresent over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "focuspresent")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))

focusfuture_plot=ggplot(
  AQ2_dataframe, 
  aes(x=Date, 
      y=focusfuture)) + 
  labs(
    title = "Trend of focusfuture over the time",
    subtitle = "(2002-2011)",
    x = "Date",
    y = "focusfuture")+
  geom_line(color = 'black') + 
  geom_smooth(method = "loess", formula = y ~ x)+
  theme(axis.text.x = element_text(angle = 270))
grid.arrange(focuspast_plot, focuspresent_plot, focusfuture_plot)

#correlation
AQ2_Correlation <- webforum_with_date[c(3,6:23)]
AQ2_Correlation$Date <- as.numeric(format(AQ2_Correlation$Date, "%Y"))
webforum_with_date_with_month<-webforum_with_date
webforum_with_date_with_month$Date<-as.yearmon(webforum_with_date$Date)
webforum_with_date_with_month <- webforum_with_date_with_month[c(3,6:23)]
Average_correlation <- aggregate(AQ2_Correlation, 
                                 by = list(webforum_with_date_with_month$Date), 
                                 mean)
by(Average_correlation[3:20], 
   factor(Average_correlation$Date), 
   cor)
str(Average_correlation)
corrplot(cor(Average_correlation[3:20]), 
         method = 'square', 
         order = 'alphabet', 
         type = 'lower',
         tl.col= "black", 
         tl.srt= 45, 
         diag = FALSE)+
  mtext("Total correlation", at=9, line=2, cex=1)
par(mfrow=c(2,3))
#2002
Average_correlation_2002 = Average_correlation %>% group_by(Date) %>% filter(Date == 2002)
plot_2002=corrplot(cor(Average_correlation_2002[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2002", at=9, line=0.2, cex=1)

#2004
Average_correlation_2004 = Average_correlation %>% group_by(Date) %>% filter(Date == 2004)
plot_2004=corrplot(cor(Average_correlation_2004[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2004", at=9, line=0.2, cex=1)

#2006
Average_correlation_2006 = Average_correlation %>% group_by(Date) %>% filter(Date == 2006)
plot_2006=corrplot(cor(Average_correlation_2006[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2006", at=9, line=0.2, cex=1)
#2008
Average_correlation_2008 = Average_correlation %>% group_by(Date) %>% filter(Date == 2008)
plot_2008=corrplot(cor(Average_correlation_2008[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2008", at=9, line=0.2, cex=1)
#2010
Average_correlation_2010 = Average_correlation %>% group_by(Date) %>% filter(Date == 2010)
plot_2010=corrplot(cor(Average_correlation_2010[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2010", at=9, line=0.2, cex=1)
#2011
Average_correlation_2011 = Average_correlation %>% group_by(Date) %>% filter(Date == 2011)
plot_2011=corrplot(cor(Average_correlation_2011[3:20]), 
                   method = 'square', 
                   order = 'alphabet', 
                   type = 'lower',
                   tl.col= "black", 
                   tl.srt= 45, 
                   diag = FALSE)+
                   mtext("2011", at=9, line=0.2, cex=1)
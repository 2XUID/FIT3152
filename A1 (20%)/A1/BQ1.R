BQ1_df <- webforum_with_date[!(webforum_with_date$posemo == 0),]
BQ1_df <- BQ1_df%>%group_by(ThreadID,year,month)%>%summarise(posemo = mean(posemo,na.rm = TRUE))

BQ1_df <- subset(BQ1_df, posemo >= mean(BQ1_df$posemo)) 
BQ1_df_without_repeat = BQ1_df %>% group_by(ThreadID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
BQ1_df <- setdiff(BQ1_df,BQ1_df_without_repeat)
average = mean(BQ1_df$posemo)

BQ1_df <- subset(BQ1_df, posemo >= mean(BQ1_df$posemo)) 
BQ1_df_without_repeat = BQ1_df %>% group_by(ThreadID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
BQ1_df <- setdiff(BQ1_df,BQ1_df_without_repeat)
average = mean(BQ1_df$posemo)


BQ1_df <- subset(BQ1_df, posemo >= mean(BQ1_df$posemo)) 
BQ1_df_without_repeat = BQ1_df %>% group_by(ThreadID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
BQ1_df <- setdiff(BQ1_df,BQ1_df_without_repeat)
average = mean(BQ1_df$posemo)

remove(BQ1_df_without_repeat)
ggplot(BQ1_df, aes(x=year, y=posemo)) + 
  geom_point(aes(size = posemo, colour = factor(ThreadID)))

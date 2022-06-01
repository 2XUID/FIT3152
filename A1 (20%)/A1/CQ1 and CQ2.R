#find out the frequancy of post
frequancy_post = as.table(by(webforum_with_date,webforum_with_date$Year,nrow))
frequancy_post = as.data.frame(frequancy_post)
#choose the highest frequancy
webforum_05_12 = webforum_with_date[webforum_with_date$Year_and_month=="05-12",]

#delete author only post once time
webforum_05_12_without_repeat = webforum_05_12 %>% group_by(AuthorID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
webforum_05_12 = setdiff(webforum_05_12,webforum_05_12_without_repeat)
remove(webforum_05_12_without_repeat)

#delete thread only show once time
webforum_05_12_without_repeat = webforum_05_12 %>% group_by(ThreadID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
webforum_05_12 = setdiff(webforum_05_12,webforum_05_12_without_repeat)
remove(webforum_05_12_without_repeat)

#find top 30 most posts author
top_authors = count(webforum_05_12)
top_authors = setDT(top_authors)[order(-n), .SD[1:30]]
sum(top_authors$n)

#merge
webforum_05_12 = merge(top_authors, webforum_05_12, by = "AuthorID")
webforum_05_12 = unique(webforum_05_12)
webforum_05_12 = select(webforum_05_12, AuthorID, ThreadID)

#delete threadID and unique graphdata
graphdata = dplyr::inner_join(webforum_05_12, webforum_05_12, by = "ThreadID")
graphdata = graphdata[graphdata$AuthorID.x!=graphdata$AuthorID.y]
graphdata$ThreadID=NULL
graphdata = unique(graphdata)

#draw graph
g = graph.data.frame(graphdata, directed=F)
#duplicate: 586
E(g)
g = simplify(g, remove.multiple = T, remove.loops = T)
#drop to 293
E(g)
plot(g, vertex.color = "red")




#CQ2
# Compare clustering coefficient of graphs
transitivity(g)
# Compare individual vertex based on closeness centrality
closeness(g)
# Compare individual vertex based on betweenness centrality
betweenness(g)
#eigen_centality
eigen_centrality(g)

#find the reason
#choose the highest frequancy
webforum_41237 = webforum_with_date[webforum_with_date$Year_and_month=="05-12",]

#delete author only post once time
webforum_41237_without_repeat = webforum_41237 %>% group_by(AuthorID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
webforum_41237 = setdiff(webforum_41237,webforum_41237_without_repeat)
remove(webforum_41237_without_repeat)

#delete thread only show once time
webforum_41237_without_repeat = webforum_41237 %>% group_by(ThreadID) %>% mutate(n=n()) %>% filter(n==1) %>% select(-n)
webforum_41237 = setdiff(webforum_41237,webforum_41237_without_repeat)
remove(webforum_41237_without_repeat)
#make table 41237 2005-12 posting only
webforum_41237 = webforum_41237[webforum_41237$AuthorID=="41237",]
webforum_41237[ ,c(1,2,3,4,24,25,26,27)] <- list(NULL)
webforum_41237 = data.frame(t(webforum_41237[-1]))
#using AQ2 data frame get the general data
webform_average_05_12 = AQ2_dataframe[AQ2_dataframe$Date=="Dec 2005",]
webform_average_05_12$Date = NULL
webform_average_05_12 = data.frame(t(webform_average_05_12[-1]))
colnames(webform_average_05_12) <- "attribute"
#merge
test <- merge(webform_average_05_12, webforum_41237, by=0, all=TRUE)
rownames(test) <- test$Row.names
test$Row.names<-NULL
#draw
ggplot(test, aes(x = row.names(test))) + 
  geom_line(aes(y = X1,group = 1)) + 
  geom_line(aes(y = X2,group = 1)) +
  geom_line(aes(y = X3,group = 1)) +
  geom_line(aes(y = X4,group = 1)) +
  geom_line(aes(y = X5,group = 1)) +
  geom_line(aes(y = X6,group = 1)) +
  geom_line(aes(y = X7,group = 1)) +
  geom_line(aes(y = X8,group = 1)) +
  geom_line(aes(y = attribute,group = 1,color="red"))+
  labs(
    title = "features of posts of 41237 vs average features rank",
    x = "Features",
    y = "rank"
  )



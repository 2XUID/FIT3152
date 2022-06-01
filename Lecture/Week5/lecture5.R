library(igraph)
# make a data frame of people and club membership
Person = as.data.frame((c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "A", "B")))
Club = as.data.frame((c("X", "X", "X", "X", "X", "X", "X", "Y", "Y", "Y", "Y", "Y")))
ClubData = cbind(Person, Club)
colnames(ClubData) = c("Person", "Club")
UniquePerson = unique(Person)
colnames(UniquePerson) = "Person"

g <- make_empty_graph(directed = FALSE)
# add vertices using “for loop”
for (i in 1 : nrow(UniquePerson)) {
  g <- add_vertices(g, 1, name = as.character(UniquePerson$Person[i]))
}

# loop through each group
for (k in unique(ClubData$Club)){
  temp = ClubData[(ClubData$Club == k),]
  # combine each pair of agents to make an edge list
  Edgelist = as.data.frame(t(combn(temp$Person,2)))
  colnames(Edgelist) = c("P1", "P2")
  # loop through pairs of edges and add
  for (i in 1 : nrow(Edgelist)) {
    g <- add_edges(g, c(as.character(Edgelist$P1[i]),as.character(Edgelist$P2[i])))
  }}
plot(g)
library(twitteR)
setup_twitter_oauth("authent","authent", “authent”, "authent")
##note that the seed user is one chosen at random because they have a small number following and followers
seed <- getUser("Michael57526911")
seed.n <- seed$screenName
seed.n
following <- seed$getFriends()
following.n <- as.character(lapply(following, function(x) x$getScreenName()))
follow.list <- c()
follow.list[[seed.n]] <- following.n
descriptions <- as.character(lapply(following, function(x) x$getDescription()))
extract.irishpolitics <- function(descriptions) {
irish <- grep("poli(tic|tics|tical)|policy|Dáil|Dail|Oireachtas|TD|T.D.|Oireachtas|government|minister|Taoiseach|Tánaiste|tanaiste|senator|journalist|news|current affairs|broadcaster", descriptions, ignore.case = T)
return(irish)
}
ir_pol<- extract.irishpolitics(descriptions)
ir_pol.users <- c(seed$screenName, following.n[ir_pol])
ir_pol.users
while (length(ir_pol.users) > length(follow.list)) {
user <- ir_pol.users[ir_pol.users %in% names(follow.list) == FALSE], 1)
user <- getUser(user)
user.n <- user$screenName
following <- user$getFriends()
friends <- as.character(lapply(following, function(x) x$getScreenName()))
follow.list[[user.n]] <- friends
descriptions <- as.character(lapply(following, function(x) x$getDescription()))
ir_pol <- extract.irishpolitics(descriptions)
new.users <- lapply(following[ir_pol], function(x) x$getScreenName())
new.users <- as.character(new.users)
ir_pol.users  <- unique(c(ir_pol.users, new.users))
limit <- getCurRateLimitInfo()[47, 3]
while (limit == "0") {
Sys.sleep(901)
limit <- getCurRateLimitInfo()[47, 3]
}
print(ir_pol.users)
}

ir_pol.users <- names(follow.list)
adjMatrix <- lapply(follow.list, function(x) (ir_pol.users %in% x) * 1)
adjMatrix <- matrix(unlist(adjMatrix), nrow = length(ir_pol.users3), byrow = TRUE,
dimnames = list(ir_pol.users, ir_pol.users))

library(igraph)
network <- graph.adjacency(adjMatrix)
plot(network)

V(network)$size <- degree(network, mode="in")
V(network)$label.cex <- (degree(network, mode="in")/max(degree(network, mode="in"))*1.25)+0.5
set.seed(777)
l <- layout.fruchterman.reingold.grid(network, niter=500)
pdf("network_nyu.pdf", width=7, height=7)
plot(network, layout=l, edge.width=1, edge.arrow.size=.25, vertex.label.color="black", vertex.shape="none", margin=-.15)
dev.off()



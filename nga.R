#set input and output folder
inputfolder = "C:/Users/gramirez/Desktop/#yali"
outputfolder = "C:/Users/gramirez/Desktop/#yali/output"

#choose to analyze net
analyzenet = TRUE

#choose how many comm you want to analyze
numberofcommunities = 5


setwd(inputfolder)

ibraries
library(igraph) #install.packages("igraph")
library(tm) #install.packages("tm")
library(ggplot2) #install.packages("ggplot2")
library(stringr) #install.packages("stringr")
library(wordcloud) #install.packages("wordcloud")

files <- list.files()
files <- files[grep("graph_for_r",files)]

data.frame <- data.frame()

for(file in files){
  print(file)
  data.frame.temp <- read.csv(file,fill=T)
  data.frame <- rbind(data.frame,data.frame.temp)
}
rm(data.frame.temp)

#fix dates
data.frame$created_time <- as.character(as.numeric(strptime(as.character(data.frame$created_time), format="%Y-%m-%d %H:%M:%S")))
data.frame$created_time <- substr(data.frame$created_time,4,10)

#fix text
#data.frame$text <- enc2utf8(as.character(data.frame$text))

#generate the initial graph
data.frame.temp <- data.frame


####select only metions and rts
#create edge list
edge.list <- data.frame.temp[,c("from_user","connection", "text","source", "created_at","created_time","url","lang","source")]
#make minor graph
g <- graph.data.frame(edge.list ,directed=TRUE)
#get the node props
node.props <- unique(data.frame.temp[,c("from_user", "followers","user_created_at","time_zone",'user_statuses','user_lang')])
names(node.props)[1] <- "name"
#create data.frame of nodes and nodeprops
test <- as.data.frame(V(g)$name)
#rename to name
names(test) <- "name"
#merge the node props with nodes
actors <- merge( test, node.props, by = "name", all.x = T)
#delete dups
actors <- actors[!duplicated(actors$name),]
#create final graph
G <- graph.data.frame(edge.list, vertices=actors, directed=FALSE)

#### delete extra####
rm(actors); rm(edge.list) ; rm(node.props) ;rm(test) ;rm(g) ; rm(file) ; rm(files)

####community finding algorighms####
multilevel <- multilevel.community(G)

#### create df####
communities <- data.frame(multilevel =  multilevel$membership, name = multilevel$names)

#### merge in the new communities
#get edge list
edge.list <- data.frame.temp[,c("from_user","connection", "text","source", "created_at","created_time","url","lang","source")]
#create gdf
g <- graph.data.frame(edge.list ,directed=TRUE)
#set node names
node.props <- unique(data.frame.temp[,c("from_user", "followers","user_created_at","time_zone",'user_statuses','user_lang')])
names(node.props)[1] <- "name"
#import communities by geting a list of names
namefile <- as.data.frame(V(g)$name)
names(namefile) <- "name"
actors <- merge( namefile, node.props, by = "name", all.x = T)
actors <- actors[!duplicated(actors$name),]
actors <- merge(actors, communities, by= "name")
G <- graph.data.frame(edge.list, vertices=actors, directed=T)

V(G)$indegrees <- degree(G,mode="in")
V(G)$outdegrees <- degree(G,mode="out")
summary(G)

#remove extra stuff
rm(g);rm(node.props);rm(data.frame.temp);rm(communities);rm(actors);rm(edge.list);rm(namefile)

#change dir
write.graph(G,"final_graph.graphml", format = "graphml")


setwd(outputfolder)


if (analyzenet == TRUE){
  


#using the wt community find the top 5 classes
top.communities <- na.omit(names(sort(table(V(G)$multilevel),decreasing = T)[1:numberofcommunities]))


for (community in top.communities){
  
  temp.graph <-  induced.subgraph(graph=G, v = which(V(G)$multilevel==community))  
  
  
  #words per tweet
  # split words 
  words_list = strsplit(E(temp.graph)$text, " ")
  # words per tweet
  words_per_tweet = as.data.frame(sapply(words_list, length))
  names(words_per_tweet) <- "dist"
  #plot
  wpt <- ggplot(words_per_tweet, aes(dist)) + 
    stat_density() + #geom_histogram
    ggtitle("Distribution of Words Per Tweet") +
    xlab("Number of Words")
  ggsave(paste("wpt_","community_",community,".jpeg",sep=""), wpt, 
         width = 10, height = 10)
  
  
  # how many hashtags per tweet
  hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
  #anything over 4 rename with 4+
  hash_per_tweet[hash_per_tweet>3] <- 4
  pietable <- table(hash_per_tweet)
  names(pietable)[names(pietable)==4] <- "4+"
  lbs <- paste(names(pietable)," hashtag: ",round(prop.table(table(hash_per_tweet)) * 100),"%",sep="")
  
  jpeg(paste("hpt_","community_",community,".jpeg",sep=""),width = 750, height = 750)
  pie(pietable,labels = lbs, col=rainbow(length(lbs)),
      main="Hashtags per Tweet")
  dev.off()
  
  
  # how many @mentions per tweet
  ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
  #anything over 4 rename with 4+
  ats_per_tweet[ats_per_tweet>3] <- 4
  pietable <- table(ats_per_tweet)
  names(pietable)[names(pietable)==4] <- "4+"
  lbs <- paste(names(pietable)," mention: ",round(prop.table(pietable) * 100),"%",sep="")
  
  jpeg(paste("mpt_","community_",community,".jpeg",sep=""),width = 750, height = 750)
  pie(pietable,labels = lbs, col=rainbow(length(lbs)),
      main="Mentions per Tweet")
  dev.off()
  
  words <- unlist(words_list)
  words <- words[nchar(words) > 3]
  
  #hashtags
  hashtags <- words[grepl("#",words)]
  hashtags <- gsub('[[:punct:]]',"",hashtags)
  hashtags <- table(hashtags)
  hashtags <- hashtags[order(hashtags,decreasing = T)]
  write.csv(hashtags,paste(community,"_top_hashtags.csv"))
  
  
  #words
  words <- gsub('[[:punct:]]','',words)
  words <- table(words)
  words <- words[order(words,decreasing = T)]
  write.csv(words,paste(community,"_top_words.csv"))
  
  
  #urls
  urls <- E(temp.graph)$url
  urls <- table(urls[urls != ""])
  if(length(urls) > 0)
  {
    write.csv(urls,paste(community,"_top_urls.csv"))
  }
  
  
  #language
  language <- E(temp.graph)$lang
  language <- table(language[language != ""])
  if(length(language) > 0)
  {
    write.csv(language,paste(community,"_top_language.csv"))
  }
  
  
  #timezones
  timezones <- table(V(temp.graph)$time_zone)
  if(length(timezones) > 0)
  {
    timezones <- timezones[order(timezones,decreasing=T)]
    write.csv(timezones,paste(community,"_top_timezones.csv"))
  }
  
  #followers
  followers <- V(temp.graph)$followers
  if(length(followers) > 0)
  {
    write.csv(followers,paste(community,"_follower_counts.csv"))
  }
  
  write.table(E(temp.graph)$text,sep = "",file=paste(community,"tweets.txt"))
  
  times <- strftime(strptime(E(temp.graph)$created_at,format="%a %b %e %H:%M:%S %z %Y",),format="%I %p",tz = "America/New_York")
  times <- data.frame(table(times))
  
  oneday <- as.character(1:24)
  oneday <- strftime(strptime(oneday,format="%H"),format="%I %p")
  
  timechart <- data.frame(row.names = oneday, order = 1:24)
  timechart <- merge(timechart,times, by.x = "row.names", by.y = "times", all.x = TRUE)
  timechart <- timechart[order(timechart$order),]
  
  if(length(timechart[is.na(timechart$Freq),]$Freq) > 0)
  {
    timechart[is.na(timechart$Freq),]$Freq <- 0
  }
  
  write.csv(timechart,file=paste(community,"besttimes.csv",sep=""))
  
}

}

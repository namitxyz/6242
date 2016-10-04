setwd('~/Documents')
load('movies_merged')
library("ggplot2")
library('stringi')
library(plyr)
library("splitstackshape")
library("scales")

# Question 1
movies_df = subset(movies_merged, Type == "movie")
cat("Number of rows which were removed :", nrow(movies_merged) - nrow(movies_df))

# Question 2
movies_df$Runtime <- stri_sub(movies_df$Runtime, 0, -5)
movies_df$Runtime <-as.numeric(as.character(movies_df$Runtime))

cat("Average Runtime :", mean(movies_df$Runtime, na.rm=TRUE))
cat("Standard Deviation Runtime :", sd(movies_df$Runtime, na.rm=TRUE))

g_1<-ggplot(movies_df, aes(Runtime)) + geom_histogram(col="red", 
                                                   fill="green", 
                                                   alpha = .5,
                                                   binwidth =4)  + ggtitle("Histogram of Runtime of Movies Dataset")
print(g_1)

g_1<-ggplot(na.omit(movies_df[,c("Year","Runtime")]), aes(Year, Runtime)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Two way relationship: Year/Runtime")
print(g_1)

g_1<-ggplot(na.omit(movies_df[,c("Budget","Runtime")]), aes(Budget, Runtime)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Two way relationship: Budget/Runtime")
print(g_1)

#Question 3

genre_dict = list()
genre_list=sort(unique(unlist(strsplit(as.character(movies_df$Genre),", "))))
movies_df<-dcast.data.table(cSplit(movies_df, "Genre", ", ", "long"), 
                       ... ~ Genre, value.var = "Genre", 
                       fun.aggregate = length)

for (column in genre_list)
  genre_dict[[column]] <- sum(movies_df[,get(column)])

genre_dict<-(genre_dict[order(-unlist(genre_dict))])
melt_genre = melt(genre_dict[1:10])
g_1 <- ggplot(melt_genre,aes(L1,value)) + geom_bar(fill="darkseagreen",stat="identity") + xlab("") + labs(title = "Number of titles for the top 10 Genres")
print(g_1)

melt_genre$fraction = melt_genre$value / sum(melt_genre$value)
g_2<- ggplot(melt_genre, aes(x="", y=value, fill=L1)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), label=paste(format(round(fraction*100, 2)),"%"), size=1)) + ggtitle("Pie chart showing the proportion of each genre")
print(g_2)

g_1<-ggplot(movies_df, aes(Action, Gross)) + geom_point() + ggtitle("Box plot of Genre and Gross")
print(g_1)
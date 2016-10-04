setwd('\\\\corp.bloomberg.com/ny-dfs/users/ngupta93/Desktop/6242')
load('movies_merged')
library("ggplot2")
library('stringi')
library('plyr')
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

movies_df_gross = subset(movies_df, !is.na(Gross))
print(nrow(movies_df_gross))
movies_df_gross_2 = list()

for (column in names(genre_dict[1:10]))
  movies_df_gross_2[[column]] <- movies_df_gross$Gross[movies_df_gross[,get(column)]==1]

g_1<-ggplot(melt(movies_df_gross_2), aes(L1, value)) + geom_boxplot() + ggtitle("Box plot of Genre and Gross")
print(g_1)

#Question 4

movies_df_year = subset(movies_df, !is.na(Released))
movies_df_year$Released <- stri_sub(movies_df_year$Released, 0, -7)
movies_df_year$Released <-as.numeric(as.character(movies_df_year$Released))
orig_movies = nrow(movies_df_year)
movies_df_year<-subset(movies_df_year, Released == Year | Released == Year+1)
cat("Number of movies removed based on Year/Released mismatch :", orig_movies, "-", nrow(movies_df_year), " = ", orig_movies-nrow(movies_df_year))

#Question 5

movies_df_year = subset(movies_df, !is.na(Released) & !is.na(Gross))
movies_df_year$Released <- stri_sub(movies_df_year$Released, 6, 7)
movies_df_year$Released <-as.numeric(as.character(movies_df_year$Released))

df <- data.frame(month = movies_df_year$Released,
                 GrossRevenue = movies_df_year$Gross)

df <- melt(df ,  id.vars = 'month')

g_1<-ggplot(df, aes(month,value)) +  geom_point() + geom_smooth(span=.2)  +  ggtitle("Month of the year VS Total Gross Revenue")
print(g_1)

genre_list=sort(unique(unlist(strsplit(as.character(movies_df_year$Genre),", "))))

df<-data.frame(Genre=character(), Month=numeric(), Gross=numeric(), stringsAsFactors = FALSE)

for (column in names(genre_dict[1:10]))
  df <- rbind(df, data.frame(Genre = column, Month= movies_df_year$Released[movies_df_year[,get(column)] == 1], 
                             Gross= movies_df_year$Gross[movies_df_year[,get(column)] == 1]))

g_1<-ggplot(df, aes(Month, Gross)) + geom_boxplot(aes(colour = Genre)) + geom_smooth(span=0.2) + ggtitle("Box plot of Gross/Month/Genre")
print(g_1)

df<-(ddply(df, .(Month, Genre), summarize,  Average_Gross=mean(Gross)))
g_1<-ggplot(df, aes(Month, Average_Gross)) + geom_point(aes(colour = Genre)) + geom_smooth(span=0.2) + ggtitle("Three way relationship: Average_Gross/Month/Genre")
print(g_1)

#Question 6

movies_df = subset(movies_merged, Type == "movie")

df<-data.frame(Imdb_Rating=numeric(), Tomato_Critic_Rating=numeric(), Tomato_User_Rating = numeric(), Gross=numeric(), stringsAsFactors = FALSE)

df <- rbind(df, data.frame(Gross = movies_df$Gross, Tomato_User_Rating = movies_df$tomatoUserRating,
                           Tomato_Critic_Rating = movies_df$tomatoRating, Imdb_Rating = movies_df$imdbRating))

df<-subset(df, !is.na(Gross) & !is.na(Tomato_User_Rating) & !is.na(Tomato_Critic_Rating) & !is.na(Imdb_Rating))
print(cor(df))

g_1<-ggplot(df, aes(Tomato_Critic_Rating, Tomato_User_Rating)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Tomato Critic Rating and Tomato User Rating")
print(g_1)

g_1<-ggplot(df, aes(Tomato_Critic_Rating, Imdb_Rating)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Tomato Critic Rating and IMBDB User Rating")
print(g_1)

g_1<-ggplot(df, aes(Tomato_User_Rating, Imdb_Rating)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Tomato User Rating and IMBDB User Rating")
print(g_1)

g_1<-ggplot(df, aes(Imdb_Rating, Gross)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Gross Revenue and IMBDB User Rating")
print(g_1)

g_1<-ggplot(df, aes(Tomato_Critic_Rating, Gross)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Gross Revenue and Tomato Critic Rating")
print(g_1)

g_1<-ggplot(df, aes(Tomato_User_Rating, Gross)) + geom_point() + geom_smooth(span=0.2) + ggtitle("Relationship between Gross Revenue and Tomato User Rating")
print(g_1)

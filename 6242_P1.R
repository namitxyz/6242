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

#Question 7

movies_df$Awards[movies_df$Awards == 'N/A'] = 0

l<-numeric()
for (single_string in as.character(gsub(("[^0-9]"), ",", movies_df$Awards)))
{
  y<-unlist(strsplit(single_string, ","))
  y<-y[y!=""]
  y<-(sum(as.numeric(y)))
  l<-c(l, y)
}

df<-data.frame(award_string=character(), award_integer=numeric(), Gross = numeric(), no_awards = numeric(), some_awards = numeric(), many_awards = numeric(), stringsAsFactors = FALSE)
df <- rbind(df, data.frame(award_string = movies_df$Awards, award_integer = l, Gross = movies_df$Gross))

df<-subset(df, !is.na(Gross))
g_1<-ggplot(df, aes(award_integer)) + geom_histogram(col="red", 
                                                     fill="green", 
                                                     alpha = .5,
                                                     binwidth =30)  + ggtitle("Histogram of Number of awards/nominations etc for a movie")
print(g_1)

Threshold = 50
df$no_awards[df$award_integer == 0] = 1
df$no_awards[df$award_integer != 0] = 0

df$some_awards[df$award_integer > 0 & df$award_integer <= Threshold] = 1
df$some_awards[df$award_integer == 0 | df$award_integer > Threshold] = 0

df$many_awards[df$award_integer > Threshold] = 1
df$many_awards[df$award_integer <= Threshold] = 0

df_2<-data.frame(Awards=character(), Gross=numeric(), stringsAsFactors = FALSE)
df_2<-rbind(df_2, data.frame(Awards = "no_awards", Gross = df$Gross[df$no_awards == 1]))
df_2<-rbind(df_2, data.frame(Awards = "some_awards", Gross = df$Gross[df$some_awards == 1]))
df_2<-rbind(df_2, data.frame(Awards = "many_awards", Gross = df$Gross[df$many_awards == 1]))

g_1<-ggplot(df_2, aes(Awards, Gross)) + geom_boxplot(aes(colour = Awards)) + geom_smooth(span=0.2) + ggtitle("Box plot of awards and nominations with respect to Gross Revenue")
print(g_1)

#Question 8

g_1<-ggplot(na.omit(movies_df[,c("Budget","Gross")]), aes(Budget, Gross)) + geom_line() + geom_smooth(span=0.2) + ggtitle("Two way relationship: Budget/Gross")
print(g_1)

g_1<-ggplot(na.omit(movies_df[,c("Year", "Budget")]), aes(Year, Budget)) + geom_line() + geom_smooth(span=0.2) + ggtitle("Two way relationship: Budget/Year")
print(g_1)

language_dict = list()
language_list=sort(unique(unlist(strsplit(as.character(movies_df$Language),", "))))
movies_df<-dcast.data.table(cSplit(movies_df, "Language", ", ", "long"), 
                            ... ~ Language, value.var = "Language", 
                            fun.aggregate = length)

language_list = language_list[language_list!= " Ancient (to 1453)"]
language_list = language_list[language_list!= " Old"]
language_list = language_list[language_list!= "N/A"]
for (column in language_list)
  language_dict[[column]] <- sum(movies_df[,get(column)])

language_dict<-(language_dict[order(-unlist(language_dict))])
melt_language = melt(language_dict[2:6])
g_1 <- ggplot(melt_language,aes(L1,value)) + geom_bar(fill="darkseagreen",stat="identity") + xlab("") + labs(title = "Histogram of Languages for the top 5 Languages excluding english")
print(g_1)

df_2<-data.frame(Language=character(), Budget=numeric(), Year=numeric())
movies_df= subset(movies_df, !is.na(Budget) & Budget != 0)

df_2<-rbind(df_2, data.frame(Language = "French", Budget = movies_df$Budget[movies_df$French == 1 & movies_df$English != 1]))
df_2<-rbind(df_2, data.frame(Language = "German", Budget = movies_df$Budget[movies_df$German == 1 & movies_df$English != 1]))
df_2<-rbind(df_2, data.frame(Language = "Italian", Budget = movies_df$Budget[movies_df$Italian == 1 & movies_df$English != 1]))
df_2<-rbind(df_2, data.frame(Language = "Russian", Budget = movies_df$Budget[movies_df$Russian == 1 & movies_df$English != 1]))
df_2<-rbind(df_2, data.frame(Language = "Spanish", Budget = movies_df$Budget[movies_df$Spanish == 1 & movies_df$English != 1]))

g_1<-ggplot(df_2, aes(Language, Budget)) + geom_boxplot(aes(colour = Language)) + geom_smooth(span=0.2) + ggtitle("Box plot of non-english movie languages with respect to Budget")
print(g_1)

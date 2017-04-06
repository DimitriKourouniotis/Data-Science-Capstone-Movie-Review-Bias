######################
# Capstone project 
# Rotten Tomatoes
# exploratory analysis
######################

############################
# creating dataset for movie
# critic magaret pomeranz
############################

margaret_pomeranz <- subset(newdata, critic_name_1 == "margaret-pomeranz")
View(margaret_pomeranz)
str(margaret_pomeranz)

##########################################
# movie reviews from this 
# critic are x out of 5 (formatted as x/5)
# convert to numeric out of 10...
# split variable by "/" sign and x2 to get 
# numeric score out of 10
##########################################

margaret_pomeranz$score <- as.numeric(sapply(strsplit(margaret_pomeranz$critic_score_and_image_1, split='/',fixed=TRUE), function(x) (x[1]))) *2
margaret_pomeranz$score_denominator <- sapply(strsplit(margaret_pomeranz$critic_score_and_image_1, split='/',fixed=TRUE), function(x) (x[2]))  

##########################################
# read in IMDB 5000 movie database
# clean up -convert movie_title to chr and 
# trim white space
##########################################

library("stringr")


IMDB_5000  = read.csv("IMDB meta data 5000 movies.csv", header = TRUE, sep =",")
IMDB_5000$movie_title <- as.character(IMDB_5000$movie_title)
IMDB_5000$movie_title <- str_trim(IMDB_5000$movie_title)
str(IMDB_5000)

##########################################
# merge the two datasets into one
# provides 309 reviews for movies 2006-2015
# convert movie_year from char to int
##########################################

head(exploratory_dataset)

exploratory_dataset <- merge(x=margaret_pomeranz, y=IMDB_5000, by = "movie_title")
exploratory_dataset$movie_year <- as.integer(exploratory_dataset$movie_year)
hist(exploratory_dataset$critic_score, col="grey")
hist(exploratory_dataset$movie_year, col="grey")

##########################################
# create vector of variable names/binary for
# each genre: 19 in total
##########################################

genre <- c("Action","Adventure","Animation","Biography","Comedy",
           "Crime","Documentary","Drama","Family","Fantasy",
           "History","Horror","Romance","Sci-Fi","Sport","Short",
           "Thriller","Musical","Mystery", "War","Western")

genre_column_names <- c(paste("genre_",genre, sep = ""))

library("dplyr")
exploratory_dataset <- mutate(exploratory_dataset, genre_Action = grepl("Action", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Adventure = grepl("Adventure", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Animation = grepl("Animation", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Biography = grepl("Biography", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Comedy = grepl("Comedy", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Crime = grepl("Crime", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Drama = grepl("Drama", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Documentary = grepl("Documentary", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Family = grepl("Family", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Fantasy = grepl("Fantasy", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_History = grepl("History", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Horror = grepl("Horror", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Romance = grepl("Romance", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Sci_Fi = grepl("Sci-Fi", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Sport = grepl("Sport", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Short = grepl("Short", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Thriller = grepl("Thriller", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Musical = grepl("Musical", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Mystery = grepl("Mystery", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_War = grepl("War", genres))
exploratory_dataset <- mutate(exploratory_dataset, genre_Western = grepl("Western", genres))


#######################################
# for CRITIC reviews
# create buckets for scores and reviews
# for tMeter % in groups of 10
#######################################

exploratory_dataset$tMeter_numeric <- as.integer(sapply(strsplit(exploratory_dataset$tMeter, split='%',fixed=TRUE), function(x) (x[1])))
exploratory_dataset$tMeter_bins <- ceiling(exploratory_dataset$tMeter_numeric/10)

critic_sample_movies <- exploratory_dataset[,c(1,6,7,36:54,57:58)]
for (j in 4:24) {
  critic_sample_movies[,j] <- ifelse(critic_sample_movies[,j]==FALSE, NA, critic_sample_movies$critic_score * critic_sample_movies[,j])
}
critic_sample_movies <- unique(critic_sample_movies)

#########################################
# tidy the data since each genre_column 
# is a value  - remove NA cases and then 
# split out to retain the genre 
#########################################

library("tidyr")
critic_sample_movies.tidy <- gather(critic_sample_movies, genre, score, -movie_title, -movie_year, -critic_score)
critic_sample_movies.tidy <- critic_sample_movies.tidy [complete.cases(critic_sample_movies.tidy),]
critic_sample_movies.tidy$genre <- sapply(strsplit(critic_sample_movies.tidy$genre, split='_',fixed=TRUE), function(x) (x[2]))  
View(critic_sample_movies.tidy)
saveRDS(critic_sample_movies.tidy,file="critic_sample_movies_tidy.rds")

#########################################
# for ROTTEN TOMATOES tMeter score (RT)
# create dataset of tMeter reviews 
# by genre for exploratory dataset movies
#########################################


RT_sample_movies <- exploratory_dataset[,c(1,6,56,36:54,57:58)]
for (j in 4:24) {
  RT_sample_movies[,j] <- ifelse(RT_sample_movies[,j]==FALSE, NA, RT_sample_movies$tMeter_bins * RT_sample_movies[,j])
}
RT_sample_movies <- unique(RT_sample_movies)
saveRDS(RT_sample_movies,file="RT_sample_movies.rds")

#########################################
# tidy the data since each genre_column 
# is a value  - remove NA cases and then 
# split out to retain the genre 
#########################################

RT_sample_movies.tidy <- gather(RT_sample_movies, genre, score, -movie_title, -movie_year, -tMeter_bins)
RT_sample_movies.tidy <- RT_sample_movies.tidy[complete.cases(RT_sample_movies.tidy),]
RT_sample_movies.tidy$genre <- sapply(strsplit(RT_sample_movies.tidy$genre, split='_',fixed=TRUE), function(x) (x[2]))  
View(RT_sample_movies.tidy)
saveRDS(RT_sample_movies.tidy,file="RT_sample_movies_tidy.rds")
#########################################
# for whole IMDB movie database
# create separate dataset of movie_title,
# genre and bucket score
#########################################

IMDB_5000_genres <- subset(IMDB_5000_genres, title_year > 2005)
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Action = grepl("Action", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Adventure = grepl("Adventure", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Animation = grepl("Animation", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Biography = grepl("Biography", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Comedy = grepl("Comedy", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Crime = grepl("Crime", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Drama = grepl("Drama", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Documentary = grepl("Documentary", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Family = grepl("Family", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Fantasy = grepl("Fantasy", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_History = grepl("History", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Horror = grepl("Horror", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Romance = grepl("Romance", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Sci_Fi = grepl("Sci-Fi", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Sport = grepl("Sport", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Short = grepl("Short", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Thriller = grepl("Thriller", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Musical = grepl("Musical", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Mystery = grepl("Mystery", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_War = grepl("War", genres))
IMDB_5000_genres <- mutate(IMDB_5000_genres, genre_Western = grepl("Western", genres))

for (j in 5:25) {
  IMDB_5000_genres[,j] <- ifelse(IMDB_5000_genres[,j]==FALSE, NA, IMDB_5000_genres$imdb_score * test_data[,j])
}
saveRDS(IMDB_5000_genres, file="IMDB_5000_genres.rds")

#########################################
# create dataset of IMDB reviews
# by genre for exploratory dataset movies
#########################################

IMDB_sample_movies <- exploratory_dataset[,c(1,6,33,36:54,57:58)]
for (j in 4:24) {
  IMDB_sample_movies[,j] <- ifelse(IMDB_sample_movies[,j]==FALSE, NA, IMDB_sample_movies$imdb_score * IMDB_sample_movies[,j])
}
IMDB_sample_movies <- unique(IMDB_sample_movies)

IMDB_sample_movies.tidy <- gather(IMDB_sample_movies, genre, score, -movie_title, -movie_year, -imdb_score)
IMDB_sample_movies.tidy <- IMDB_sample_movies.tidy[complete.cases(IMDB_sample_movies.tidy),]
IMDB_sample_movies.tidy$genre <- sapply(strsplit(IMDB_sample_movies.tidy$genre, split='_',fixed=TRUE), function(x) (x[2]))  
View(IMDB_sample_movies.tidy)

saveRDS(IMDB_sample_movies.tidy, file= "IMDB_sample_movies_tidy.rds")

##############################################
# scatter/jitter plots for all three datasets
# 1. critic reviews 
# 2. IMDB reviews of sample
# 3. RT reviews of sample
##############################################

ggplot(critic_sample_movies.tidy, aes(x=factor(genre), y=critic_score, col = genre))+geom_jitter()
ggplot(IMDB_sample_movies.tidy, aes(x=factor(genre), y=imdb_score, col = genre))+geom_jitter()
ggplot(RT_sample_movies.tidy, aes(x=factor(genre), y=tMeter_bins, col = genre))+geom_jitter()



#############################################
# comparing distributions of genre= drama
#
#############################################


selection <- "Drama"
IMDB_drama <- subset(IMDB_sample_movies.tidy, genre == selection, imdb_score)
RT_drama <- subset(RT_sample_movies.tidy, genre == selection, tMeter_bins)
critic_drama <- subset(critic_sample_movies.tidy, genre == selection, critic_score)

genre_drama <- data.frame (critic_drama,
                          round(IMDB_drama), 
                          RT_drama )
names(genre_drama) <- c("Critic Review", "IMDB score", "RT Score" )
genre_drama.tidy <- gather(genre_drama, key = review_source, value = score )
head(genre_drama)
summary(genre_drama)
head(genre_drama.tidy)


# Density plots with semi-transparent fill
ggplot(genre_drama.tidy, aes(x= score, fill=review_source)) + geom_density(alpha=.3) +
  ggtitle("Ratings for 175 movies with Genre = Drama")

# box plots ??????
ggplot(genre_drama.tidy, aes(y= score, x=review_source, fill=review_source)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip() +
  ggtitle("Ratings for 175 movies with Genre = Drama")

saveRDS(genre_drama.tidy,file = "genre_drama_tidy.rds")

#############################################
# comparing distributions of genre= comedy
#
#############################################
selection <- "Comedy"
IMDB_comedy <- subset(IMDB_sample_movies.tidy, genre == selection, imdb_score)
RT_comedy <- subset(RT_sample_movies.tidy, genre == selection, tMeter_bins)
critic_comedy <- subset(critic_sample_movies.tidy, genre == selection, critic_score)

genre_comedy <- data.frame (critic_comedy,
                            IMDB_comedy, 
                            RT_comedy )
names(genre_comedy) <- c("Critic Review", "IMDB score", "RT Score" )
head(genre_comedy)
genre_comedy <- gather(genre_comedy, key= review_source, value = score )
head(genre_comedy)
genre_comedy

# Density plots with semi-transparent fill
ggplot(genre_comedy, aes(x= score, fill=review_source)) + geom_density(alpha=.3) +
  ggtitle("Ratings for 104 movies with Genre = Comedy")

saveRDS(genre_comedy,file = "genre_comedy_tidy.rds")
#############################################
# create tidy dataset of directors 
# and a dataset of actors
#############################################

director_sample <- subset(exploratory_dataset[,c( 10, 7, 33,56)] )
director_sample <- director_sample [complete.cases(director_sample),]

actor1_sample <- subset(exploratory_dataset[,c( 19, 7, 33,56)] )
actor2_sample <- subset(exploratory_dataset[,c( 15, 7, 33,56)] )
actor3_sample <- subset(exploratory_dataset[,c( 22, 7, 33,56)] )

names(actor1_sample) <- c("Actor","critic_review", "IMDB_score", "RT_score" )
names(actor2_sample) <- c("Actor","critic_review", "IMDB_score", "RT_score" )
names(actor3_sample) <- c("Actor","critic_review", "IMDB_score", "RT_score" )

library("plyr")
actor_sample <- rbind.fill(actor1_sample,actor2_sample,actor3_sample)
View(actor_sample)


# actor_sample.tidy <- gather(actor_sample, key= actor, value = score, -Critic_Review, - IMDB_score, -RT_score )
head(actor_sample)
saveRDS(actor_sample,file = "actor_sample.rds")

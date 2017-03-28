##combining large data sets from batch runs of script to read critics pages:
##total <- rbind(list_of_review5,  list_of_review7)
##View(total2)
##total2 <- rbind(total, list_of_review4)
##sum(duplicated(total4))
##total3 <-(rbind(total2,list_of_review3))
##total4 <- rbind(total3,list_of_review2)





################### Reading TMeter Scores

critics_urls_pages <-c()
critics_urls_pages<- read_csv("critics_urls_ds_corrected_english.csv")
View(critics_urls_pages)

list_of_tMeterScore <- c()
movie_title_1 <-c()
tMeterScore_1 <-c()

movie_title <-c()
tMeterScore <-c()

for (j in 1:nrow(critics_urls_pages)) {
  ##for t-meter score

##for (j in 1:2 ) {  
  movie_review <- read_html(as.character(critics_urls_pages[j,2]))
  print(j)
  
  ##example movie_review <- read_html("https://www.rottentomatoes.com/critic/sarah-winshall/movies?page=1")
  tMeterScore <- movie_review %>%
    html_nodes(".tMeterScore") %>%
    html_text()
  
  movie_title <- movie_review %>%
    html_nodes(".movie-link") %>%
    html_text()
## create 24 NA entries for each page to compensate for spurious data from promotions on page
  movie_title_1 <-c(movie_title_1, rep(NA,24))
  movie_title_1 <-c(movie_title_1, movie_title)
  tMeterScore_1 <-c(tMeterScore_1, tMeterScore)
}

list_of_tMeterScore<- data.frame(
  movie_title_1, 
  tMeterScore_1)
View(list_of_tMeterScore)
View(tMeterScore_1)
write.csv(tMeterScore_1, file = "tMeterScore_1.csv")



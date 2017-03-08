###### - - - - accented letters


####----- convert accented characters
#### "\xf1 -> "n" https://www.rottentomatoes.com/critic/michael-ordo\xf1a/?page=1
#### "\xed -> "i" https://www.rottentomatoes.com/critic/marcos-gand\xeda/?page=1
#### "\xe1 -> "a" https://www.rottentomatoes.com/critic/mariana-fern\xe1ndez/?page=1
#### "\xf3 -> "o" https://www.rottentomatoes.com/critic/carlos-mara\xf1\xf3n/?page=1
#### "\xe9 -> "e" https://www.rottentomatoes.com/critic/virginie-s\xe9lavy/?page=1
#### "\xfa -> "u" https://www.rottentomatoes.com/critic/jes\xfas-chavarria/?page=1
#### "\xe7 -> "c" https://www.rottentomatoes.com/critic/jc-ma\xe7ek-iii/?page=2

install.packages("stringr")
library(stringr)

critic_full_accented_names_list <- critics_list_dataset$critics_review_page
critic_full_accented_names_list
corrected_list <- c()

for (i in critic_full_accented_names_list) {
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xf1", repl="n")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xed", repl="i")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xe1", repl="a")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xf3", repl="o")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xe9", repl="e")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xfa", repl="u")
critic_full_accented_names_list <- str_replace_all(critic_full_accented_names_list, pattern="\xe7", repl="c")

corrected_list <- c(corrected_list, critic_full_accented_names_list)
}

critic_full_accented_names_list
corrected_list

variable <- str_replace_all("https://www.rottentomatoes.com/critic/michael-ordo\xf1a/?page=1" , pattern="\xf1", repl="n")[]
variable

for (j in 1: dim(critics_list_dataset)[1]) {
  print(critics_list_dataset[j,1])
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xf1", repl="n")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xed", repl="i")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xe1", repl="a")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xf3", repl="o")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xe9", repl="e")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xfa", repl="u")
  critics_list_dataset[j,1] <- str_replace_all(critics_list_dataset[j,1] , pattern="\xe7", repl="c")
  print(critics_list_dataset[j,1])
  ## print(paste(critics_list_dataset[j,1],"?page=",k, sep=""))
  ##  critic_review_urls <- c( critic_review_urls, paste(critics_list_dataset[j,1],"?page=",k, sep=""))
}


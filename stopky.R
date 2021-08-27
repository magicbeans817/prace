install.packages("stopwords")
library(stopwords)

install.packages("utils")
library(utils)

library(dplyr)

library(stringr)

stopky <- stopwords::stopwords("cs", source = "stopwords-iso")
stopky %>% head()
stopky <- stopky %>% tolower()

stringy <- c("Tohle je cvičný string", "Český Krumlov",
             "Aby qwerty", "Babiše, toho ani náhodou")
stringy <- matrix(stringy, ncol = 2, nrow = 2, byrow = FALSE)
colnames(stringy) <- c("stringy1", "stringy2")

stringy <- stringy %>% as.data.frame()
stringy$stopky <- 0
stringy

pocet_stopek <- function(data){
for (s in 1:(ncol(stringy)-1)){
  for (r in 1:nrow(stringy)){
    rozdeleno <- strsplit(tolower(stringy[r,s]), " ") %>% unlist()
    for (q in 1:length(rozdeleno)){
      for (u in 1:length(stopky)) {
        if (rozdeleno[q] == stopky[u]) {
          stringy[r,"stopky"] <- stringy[r, "stopky"] + 1
          break
        }
      }
    }
  }
}
  return(stringy)
}

pocet_stopek(stringy)

stringy


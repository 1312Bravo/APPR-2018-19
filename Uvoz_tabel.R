
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
source("lib/libraries.r", encoding="UTF-8")
library(tidyr)
library(readxl)
library(data.table)
library(dplyr)

Statistika <- read_csv("podatki/statistika.txt")
Statistika <- Statistika[,-1]
Statistika$Player = gsub("^(.*)\\\\.*", "\\1", Statistika$Player)


uvozi.evropejce <- function() {
  link <- "http://pr.nba.com/nba-international-players-2017-18/"
  stran <- html_session(link) %>% read_html()
  evropejci <- stran %>% html_nodes(xpath="//table[@width='691']") %>%
    .[[1]] %>% html_table()
  colnames(evropejci) <- evropejci[1, ]
  evropejci <- evropejci[-1, ]
  evropejci <- evropejci[,-4]
  for (i in 1:ncol(evropejci)) {
    if (is.character(evropejci[[i]])) {
      Encoding(evropejci[[i]]) <- "UTF-8"
    }
  }
}

evropejci <- unite(evropejci, "Name", c("First Name", "Last Name"), remove=FALSE)
evropejci <- evropejci[ ,-c(3,4)]
evropejci <- evropejci %>% mutate(Name = gsub("_", " ", Name))
evropejci <- evropejci[, c(2,1)]

evropske_drzave <- prebivalstvo$Country
evropejci <- filter(evropejci, Country %in% evropske_drzave)





prebivalstvo <- read_excel("podatki/prebivalstvo.xlsx")
prebivalstvo <- prebivalstvo[,c(2,3)]
prebivalstvo <- prebivalstvo[-1,]
colnames(prebivalstvo)[1] <- "Country"


Place <- read_excel("podatki/place.xlsx")
Place <- place[,c(2,3)]
setnames(Place, old = c('2017/18','X__1'), new = c('Player','Salary'))


fiba_ranking <- read_excel("podatki/fiba_ranking.xlsx")
fiba_ranking <- fiba_ranking[,c(-4,-6)]
fiba_ranking <- fiba_ranking[-1,]
fiba_ranking <- fiba_ranking[,c(2,4,1,3)]
fiba_ranking$COUNTRY = gsub("^([A-Z][A-Z][A-Z])(.*)$", "\\2", fiba_ranking$COUNTRY)
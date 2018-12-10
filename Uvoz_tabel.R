
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
source("lib/libraries.r", encoding="UTF-8")
library(tidyr)

Statistika <- read_csv("Statistika.txt")
Statistika$Player
View(Statistika)


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

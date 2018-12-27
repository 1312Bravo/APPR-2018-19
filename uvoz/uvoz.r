# 2. faza: Uvoz podatkov

# separate, fill, melt (>?separate)

source("lib/libraries.r", encoding="UTF-8")
library(tidyr)
library(readxl)
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# UVOZ STATISTIKE

# Funkcija, ki uvozi podatke iz tabele statistika.txt (basketball-reference.com)
uvozi.statistiko <- function() {
  data <- read_csv("podatki/statistika.txt", locale=locale(encoding="Windows-1250"))
}

# Zapis podatkov v razpredelnico statistika
statistika <- uvozi.statistiko()
# Pobrisan prvi stolpec z imenom Rk (ranking)
statistika <- statistika[,-1]
# Izbrisem zadnjo vrstico
statistika <- statistika[-nrow(statistika),]
# Urejen stolpec Player, ki sedaj vsebuje samo imena
statistika$Player = gsub("^(.*)\\\\.*", "\\1", statistika$Player)
# Pri nekaterih je v stolpcu FTA in FT% 'na', to zamenjam z 0.
statistika$FTA[is.na(statistika$FTA)]<- 0

# UVOZ PLAC

# Funkcija, ki uvozi place iz strani hoopshype.com
uvozi.place <- function(){
  link <- "https://hoopshype.com/salaries/players/2017-2018/"
  stran <- html_session(link) %>% read_html()
  place <- stran %>% html_nodes(xpath="//table[@class='hh-salaries-ranking-table hh-salaries-table-sortable responsive']") %>%
    .[[1]] %>% html_table()
}

# Zapis podatkov v razpredelnico place
place <- uvozi.place()
# Poimenovanje stolpcov z imeni v prvi vrstici
colnames(place) <- place[1,]
# Odstranjena prva vrstica in prvi stolpec
place <- place[-1, -1 ]
# Preimenovanje drugega stolpca
names(place)[2] <- "Salary"
# Pretvorba v stevila v stolpcu Salary
place <- place %>% mutate(Salary=parse_number(Salary,
                                              locale=locale(grouping_mark=",")))
place <- place[,-3]
# Zadnji dve stevilki pri placi sta enaki 0, za vecjo preglednost
place$Salary <- signif(place$Salary, digits=5)

# UVOZ EVROPEJCEV

# Funkcija, ki uvozi evropejce iz strani nba.com
uvozi.tujce <- function() {
  link <- "http://pr.nba.com/nba-international-players-2017-18/"
  stran <- html_session(link) %>% read_html()
  tujci <- stran %>% html_nodes(xpath="//table[@width='691']") %>%
    .[[1]] %>% html_table()
}

# Zapis podatkov v razpredelnico evropejci
tujci <- uvozi.tujce()
# Poimenovanje stolpcov z imeni v prvi vrstici
colnames(tujci) <- tujci[1, ]
# Odstranjena prva vrstica
tujci <- tujci[-1, ]
# Odstranjen 4. stolpec z imeni ekip
tujci <- tujci[,-4]
# Zdruzitev stolpcev First in Last Name v dodan stolpec Player
tujci <- unite(tujci, "Player", c("First Name", "Last Name"), remove=FALSE)
# Izbris 3. in 4. stolpca
tujci <- tujci[ ,-c(3,4)]
# Zamenjava znaka _ s " " v stolpcu Player
tujci <- tujci %>% mutate(Player = gsub("_", " ", Player))
# Zamenjava vrstnega reda stolpcev
tujci <- tujci[, c(2,1)]
# Nova tabela samo z evropejci
evropske_drzave <- populacija$Country
evropejci <- filter(tujci, Country %in% evropske_drzave)


# UVOZ  FIBA LESTVICE ZA EVROPO

# # Funkcija, ki uvozi fiba lestvico evropskih drzav iz strani fiba.basketball.com
uvozi.fibalestvico <- function() {
  link <- "http://www.fiba.basketball/rankingmen#%7Ctab=fiba_europe"
  stran <- html_session(link) %>% read_html()
  fiba.lestvica <- stran %>% html_nodes(xpath="//table[@class='fiba_ranking_table columnhover default_style responsive']") %>%
    .[[5]] %>% html_table()
}

# Zapis podatkov v razpredelnico fiba.lestvica
fiba.lestvica <- uvozi.fibalestvico()
# Izbris nepotrebnih stolpcev 
fiba.lestvica <- fiba.lestvica[,c(-4,-6)]
# Zamenjava vrstnega reda stolpcev
fiba.lestvica <- fiba.lestvica[,c(2,4,1,3)]
# Preimenovanje drugega stolpca
names(fiba.lestvica)[2] <- "Points"


# UVOZ POPULACIJE EVROPSKIH DRZAV

# Funkcija za uvoz podatkov o populaciji evropskih drzav iz datotetke prebivalstvo.csv (worldpopulationreview.com)
uvozi.populacijo <- function(populacija) {
  tabela <- read_delim("podatki/prebivalstvo.csv", 
                       ";", escape_double = FALSE, col_types = cols(Population = col_number()), 
                       locale = locale(encoding = "WINDOWS-1250"), 
                       trim_ws = TRUE)
}

# Zapis podatkov v razpredelnico populacija
populacija <- uvozi.populacijo()
# Izbris nepotrebnih stolpcev, ostaneta samo še država in prebivalstvo ter prve vrstice
populacija <- populacija[-1,c(2,3)]
# Poimenovanje stolpcev
names(populacija) <- c("Country", "Population")


# Sedaj imam v tabeli evropejci res samo evropejce
evropske_drzave <- populacija$Country
evropejci <- filter(tujci, Country %in% evropske_drzave)

# OD TU NOVO POROČILO


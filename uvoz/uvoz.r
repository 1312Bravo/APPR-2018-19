# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")
source("lib/libraries.r", encoding="UTF-8")
library(tidyr)
library(readxl)
library(data.table)
library(dplyr)


# UVOZ STATISTIKE

# Funkcija, ki uvozi podatke iz tabele statistika.txt
uvozi.statistiko <- function(statistika) {
  data <- read_csv2("podatki/statistika.txt")
}

#Tu se pojavljajo neke napake

# Zapis podatkov v razpredelnico statistika
statistika <- uvozi.statistiko()
# Pobrisan prvi stolpec z imenom Rk (ranking)
statistika <- statistika[,-1]
# Urejen stolpec Player, ki sedaj vsebuje samo imena
statistika$Player = gsub("^(.*)\\\\.*", "\\1", statistika$Player)

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

# UVOZ EVROPEJCEV

# Funkcija, ki uvozi evropejce iz strani nba.com
uvozi.evropejce <- function() {
  link <- "http://pr.nba.com/nba-international-players-2017-18/"
  stran <- html_session(link) %>% read_html()
  evropejci <- stran %>% html_nodes(xpath="//table[@width='691']") %>%
    .[[1]] %>% html_table()
}

# Zapis podatkov v razpredelnico evropejci
evropejci <- uvozi.evropejce()
# Poimenovanje stolpcov z imeni v prvi vrstici
colnames(evropejci) <- evropejci[1, ]
# Odstranjena prva vrstica
evropejci <- evropejci[-1, ]
# Odstranjen 4. stolpec z imeni ekip
evropejci <- evropejci[,-4]
# Zdruzitev stolpcev First in Last Name v dodan stolpec Name
evropejci <- unite(evropejci, "Name", c("First Name", "Last Name"), remove=FALSE)
# Izbris 3. in 4. stolpca
evropejci <- evropejci[ ,-c(3,4)]
# Zamenjava znaka _ s " " v stolpcu Name
evropejci <- evropejci %>% mutate(Name = gsub("_", " ", Name))
# Zamenjava vrstnega reda stolpcev
evropejci <- evropejci[, c(2,1)]
#Sedaj so v tabeli evropejci res samo evropejci
evropske_drzave <- populacija$Country
evropejci <- filter(evropejci, Country %in% evropske_drzave)



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


# UVOZ POPULACIJE EVROPSKIH DRZAV

# Funkcija za uvoz podatkov o populaciji evropskih drzav iz datotetke populacija.csv
uvozi.populacijo <- function(populacija) {
  tabela <- read_delim("podatki/populacija.csv", 
                           ";", col_names = FALSE, 
                           col_types = cols(X2 = col_number()), 
                           locale = locale(encoding = "WINDOWS-1250"))
}

# Zapis podatkov v razpredelnico populacija
populacija <- uvozi.populacijo()
# Poimenovanje stolpcev
names(populacija) <- c("Country", "Population")

#Sedaj imam v tabeli evropejci res samo evropejce
evropske_drzave <- populacija$Country
evropejci <- filter(evropejci, Country %in% evropske_drzave)




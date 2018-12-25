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

# Nova tabela, ki nam pove število igralcev iz posamezne države
st.igralcev <- table(evropejci$Country)
# Spremenim v data.frame tabelo (prej mi nekaj ni delovalo)
st.igralcev <- as.data.frame(st.igralcev)
# Preimenujem stolpca v novi tabeli
names(st.igralcev)[2] <- "Players"
names(st.igralcev)[1] <- "Country"

# Spremenim tabelo placo, tako da dodam se narodnost igralcev
# (Američani imajo NA)
place <- merge(place,tujci,by="Player",all=TRUE)
# Znak NA v stolpcu Country zamenjam z USA
place$Country[is.na(place$Country)]<- "USA" 
# Nekateri (6 igralcev) imajo 'na' v stolpcu Salary, v prejsnjih placah, nihče ni imel 'na'. 

# Npr. Dennis Schroder (place1 -> 132, place -> 71) mu kar napiše zraven še 'na' in posledično USA, čeprav je Germany.
# Vidim: JJ Barea ima drugačno ime v obeh tabelah..


# Uredil bom ročno (majhno število) in nato vprašal, če je to ok
# Ker bom brisal vrstice bom dodal števila vrstic, ker jih tam na levi ne posodablja
# Kasneje bom to izbrisal
place$vrstica <- seq.int(nrow(place)) 
# Vidim da moram to sprodi posodabljati..

# Dennis Schroeder je Nemec
place[132,2] <- place[133,2]
# Sedaj izbrišem ameriškega Dennisa
place <- place[-133,]

# Jose Juan Barea je iz Portorika
place[219,2] <- place[294,2]
# Izbrišem ameriškega Bareo
place <- place[-294,]

# Juancho Hernangomez je Španec
place[306,2] <- place[305,2]
# Odstranim ameriškega Juanchota
place <- place[-305,]

# Nene je brazilec
place[428,2] <- place[427,2]
# Odstranim ameriškega Neneja
place <- place[-427,]

# Patty Mills je Avstralec
place[453,2] <- place[451,2]
# Odstranim ameriškega Pattya
place <- place[-451,]

# Cabarrot je Francoz
place[528,2] <- place[527,2]
# Odstranim ameriškega Cabarrota
place <- place[-527,]

# Sedaj nihče več nima 'na' pri plači :)

# Zamenjam vrstni red stolpcev, ker mi je bolj všeč, pobrišem tudi stolpec z vrsticami
place <- place[c(1,3,2)]
# Igralci z višjo plačo bodo na vrhu in obratno, uredim padajoče glede na plačo
place <- place[order(place$Salary, decreasing = TRUE),]

# Zdruzim tabeli fiba lestvice in populacije držav (V novo tabelo, da lazje ugotovim kje so tezave)
fiba.lestvica1 <- merge(fiba.lestvica, populacija, by="Country",all=TRUE)
# Ponovno je v nekaterih vrsticah v različnih stolpcih 'na'
# Popravim ročno, morda si pomagam s spletom

# Population of Armenia -> 3.031.669
fiba.lestvica1[3,5] <- 3031669
# Bosnia and Hercegovina :)
fiba.lestvica1[9,5] <- fiba.lestvica1[8,5]
# Population of Cyprus -> 1.170.125
fiba.lestvica1[13,5] <- 1170125
# Population of Georgia <- 3.718.200
fiba.lestvica1[20,5] <- 3718200
# Great Britain :)
fiba.lestvica1[23,5] <- fiba.lestvica1[57,5]
# Population of Azebraijan -> 9.827.589
fiba.lestvica1[5,5] <- 9827589
# Population of Israel -> 8.855.000 
fiba.lestvica1[30,5] <- 8855000
# Population of Kosovo -> 1.870.981.
fiba.lestvica1[32,5] <- 1870981
# MKD :)
fiba.lestvica1[38,5] <- fiba.lestvica1[55,5]
# Population of Turkey -> 80.810.525
fiba.lestvica1[56,5] <- 1870981
# Zbrišem Channel Bosnia & Hercegovina, Channel Islands, Fareoe Islands, Isle of man,
# Liechtenstein, Monaco, MKD, U.K.
# Morda se, da tudi lažje
fiba.lestvica1 <- fiba.lestvica1[-c(8,11,17,25,29,34,40,55,57),]
# Izbrišem vrstice, ki imajo v stolpcu Points vrednost 'na'
# fiba.lestvica1 <- fiba.lestvica1[-c(fiba.lestvica1$Points[is.na(fiba.lestvica1$Points)]),]
# Ni okej

# Sedaj znova shranim tabelo pod fiba.lestvica :)
fiba.lestvica <- fiba.lestvica1

# lestvico združim s številom igralcev v posamezni državi 
fiba.lestvica <- merge(fiba.lestvica, st.igralcev, by="Country",all=TRUE)
# Kjer so 'na' vstavim 0
fiba.lestvica$Players[is.na(fiba.lestvica$Players)]<- 0


# Statistika igralcev glede na: stevila odigranih minut, stevilo tekem, stevilo metov in stevilo zacetih tekem.
# Ta statistika najbolj pove pomembnost igralca pri ekipi in njihovo zaupanje vanj.
# naredim novo tabelo, dobljeno iz tabele statistika, v kateri vzamem potrebne stolpce

# V stolpcu Player se nekateri igralci pojavijo večkrat
# vrstica z največjo številko v stolpcu G je vrstica, ki jo je treba upoštevati, ostale izbrišemo 
statistika <- statistika %>% distinct(Player, .keep_all = TRUE)

#Ustvarim novo tabelo statistike zaupanja, s podatki, ki o zaupanju ekipe igralcu povejo največ
statistika.zaupanja <- statistika[,c(1,3,4,5,6)]

# Dodal ji bom stolpec z narodnostjo, doda se tudi stolpec s placo, ki je tukaj zelo pomemben
# Z inner_join ohranim samo tiste igralce, ki se ujemajo v obeh tabelah :
statistika.zaupanja <- inner_join(place, statistika.zaupanja, by="Player")



# V tabelo statistike zaupanja dodam rank igralcev glede na posamezno meritev
statistika.zaupanja  <- statistika.zaupanja %>% 
  arrange(desc(Salary)) %>% mutate(Salary.rank = 1:nrow(.)) %>%
  arrange(desc(G)) %>% mutate(GamesPlayed.rank = 1:nrow(.)) %>% 
  arrange(desc(GS)) %>% mutate(GamesStarted.rank = 1:nrow(.)) %>%
  arrange(desc(MP)) %>% mutate(MinutesPlayed.rank = 1:nrow(.)) %>% 
  arrange(desc(FGA)) %>% mutate(FieldGoalAttempts.rank = 1:nrow(.))

# Sedaj imam tabelo zaupanja v kateri so samo evropejci in njihov ranking glede na določene meritve
zaupanje.evropejcem <- filter(statistika.zaupanja, Country %in% fiba.lestvica$Country)
zaupanje.evropejcem <- zaupanje.evropejcem[,-c(4:7)]

# Ustvarim novo tabelo statistika.ucinkovitosti, s podatki o doprinosu igralca ekipi
statistika.ucinkovitosti <- statistika[,c(1,5,7,9:14,16)]
# Dodal ji bom stolpec z narodnostjo
statistika.ucinkovitosti <-  statistika.ucinkovitosti %>%
  inner_join(place, statistika.ucinkovitosti, by="Player") %>%
  select(-one_of("Salary")) 
# Prerazporedim stolpce
statistika.ucinkovitosti <- statistika.ucinkovitosti[c(1,11,2:4,10,5:9)]
# Preimenujem stolpca s procenti v imenu saj imam s tem v dplyr težave
names(statistika.ucinkovitosti)[4] <- "eFG"
names(statistika.ucinkovitosti)[5] <- "FT"
# Sedaj posodobim stolpce da bodo kazali statistiko glede na odigrane minute
statistika.ucinkovitosti <- statistika.ucinkovitosti %>%
  mutate_at(vars(PTS: TOV), funs(./statistika.ucinkovitosti$MP )) %>%
  mutate_at(6:11, funs(round(., 4))) %>%
  select(-MP)

statistika.ucinkovitosti$FT[is.na(statistika.ucinkovitosti$FT)]<- 0
statistika.ucinkovitosti$eFG[is.na(statistika.ucinkovitosti$eFG)]<- 0

# V tabelo statistike ucinkovitosti dodam rank igralcev glede na posamezno meritev
statistika.ucinkovitosti  <- statistika.ucinkovitosti %>% 
  arrange(desc(eFG)) %>% mutate(EffectiveFieldGoal.rank = 1:nrow(.)) %>%
  arrange(desc(FT)) %>% mutate(FreeThrows.rank = 1:nrow(.)) %>% 
  arrange(desc(PTS)) %>% mutate(Points.rank = 1:nrow(.)) %>%
  arrange(desc(TRB)) %>% mutate(Rebounds.rank = 1:nrow(.)) %>% 
  arrange(desc(AST)) %>% mutate(Assists.rank = 1:nrow(.)) %>% 
  arrange(desc(STL)) %>% mutate(Steals.rank = 1:nrow(.)) %>%
  arrange(desc(BLK)) %>% mutate(Blocks.rank = 1:nrow(.)) %>% 
  arrange(TOV) %>% mutate(Turnovers.rank = 1:nrow(.))

# Sedaj imam tabelo ucinkovitosti v kateri so samo evropejci in njihov ranking glede na določene meritve
ucinkovitost.evropejcev <- filter(statistika.ucinkovitosti, Country %in% fiba.lestvica$Country)
ucinkovitost.evropejcev <- ucinkovitost.evropejcev[,-c(3:10)]

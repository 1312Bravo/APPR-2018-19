# 4. faza: Analiza podatkov

#source("uvoz/uvoz.r") # Če kličem tukaj v poročilu ne, nekej tazga..

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
# Zbrišem Channel Bosnia & Hercegovina, Channel Islands, Fareoe Islands, Isle of man,
# Liechtenstein, Monaco, MKD, U.K.
# Morda se, da tudi lažje
fiba.lestvica1 <- fiba.lestvica1[-c(8,11,17,25,29,34,40,55,57),]
# Izbrišem vrstice, ki imajo v stolpcu Points vrednost 'na'
# fiba.lestvica1 <- fiba.lestvica1[-c(fiba.lestvica1$Points[is.na(fiba.lestvica1$Points)]),]
# Ni okej
# Sedaj znova shranim tabelo pod fiba.lestvica :)
fiba.lestvica <- fiba.lestvica1

# Nova tabela, ki pove število igralcev iz posamezne države
st.igralcev <- table(evropejci$Country)
st.igralcev <- as.data.frame(st.igralcev)
colnames(st.igralcev) <- c("Country", "Players")

# lestvico združim s številom igralcev v posamezni državi
fiba.lestvica <- merge(fiba.lestvica, st.igralcev, by="Country",all=TRUE)
# Kjer so 'na' vstavim 0
fiba.lestvica$Players[is.na(fiba.lestvica$Players)]<- 0
fiba.lestvica <- fiba.lestvica %>% arrange(Worldrank)
# Naredim nov stolpec v katerem je število igralcev glede na 10 milijonov prebivalcev
fiba.lestvica$PlayersPer10Million <- (fiba.lestvica$Players / fiba.lestvica$Population) * 10000000 %>%
  as.numeric(fiba.lestvica$PlayersPer10Million)
# fiba lestvica v kateri so samo države, ki imajo NBA igralce
fiba.lestvicaNBA <- filter(fiba.lestvica, fiba.lestvica$Players > 0)

# Statistika igralcev glede na: stevila odigranih minut, stevilo tekem, stevilo metov in stevilo zacetih tekem.
# Ta statistika najbolj pove pomembnost igralca pri ekipi in njihovo zaupanje vanj.
# naredim novo tabelo, dobljeno iz tabele statistika, v kateri vzamem potrebne stolpce

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
zaupanje.evropejcem <- zaupanje.evropejcem[,-c(3:7)]

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





#######  NAPREDNA ANALIZA ######
ucinkovitost.overall <- statistika.ucinkovitosti
ucinkovitost.overall$Sum.rank <- rowSums(ucinkovitost.overall[,11:18])
ucinkovitost.overall <- ucinkovitost.overall %>%
  arrange(Sum.rank) %>%  
  mutate(Sum.rank = 1:nrow(.))

ucinkovitost.evropejcev.overall <- filter(ucinkovitost.overall, Country %in% fiba.lestvica$Country)
ucinkovitost.evropejcev.overall <- ucinkovitost.evropejcev.overall[,-c(3:10)]

# Povezava med placo in ucinkovitostjo Overall rank
ucinkovitostvsplaca <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
ucinkovitostvsplaca <- ucinkovitostvsplaca[, c(1,3,17)]

#Izračun modela
fitoverall <- lm(Salary.rank ~ Sum.rank, data=ucinkovitostvsplaca)
#Izris modela
ggplot(data=ucinkovitostvsplaca, aes(x=Sum.rank, y=Salary.rank)) + geom_point() + geom_smooth(method=lm)



# Povezava med placo in st. tock

tockevsplaca.rank <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
tockevsplaca.rank <- tockevsplaca.rank[, c(1,3,11)]

#Izračun modela
fittocke.rank <- lm(Points.rank ~ Salary.rank, data=tockevsplaca.rank)
#Izris modela
ggplot(data=tockevsplaca.rank, aes(x=Points.rank, y=Salary.rank)) + geom_point() + geom_smooth(method=lm)
# Koliko točč bi glede na ta model moral dosegati posamezen igralec
# Vzamem igralce: 
# Stephen Curry -> Max salary




# Povezava med placo in st. tock #2 (Točke na minuto)

tockevsplaca <- statistika.ucinkovitosti[, c(1,2,5)]
# Vzamem samo evropejce
tockevsplaca <- filter(tockevsplaca, Country %in% fiba.lestvica$Country)
# V tabelo dodam plače
tockevsplaca <- inner_join(tockevsplaca, place, by="Player")
tockevsplaca <- tockevsplaca[,c(-2,-4)]
names(tockevsplaca) <- c("Player", "Tocke", "Placa")
tockevsplaca <- tockevsplaca %>% mutate(Placa = Placa/1e+06)
#Izračun modela
fittocke <- lm(Tocke ~ Placa, data=tockevsplaca)
ggplot(data=tockevsplaca, aes(x=Tocke, y=Placa)) + geom_point() + geom_smooth(method=lm)

#Koliko točk na minuto bi moral dosegati Stephen Curry, najbolje plačan igralec v ligi, da bi imel plačo kot jo ima:
# Dodam igralce, da imam 5 najbolje plačanih v ligi
# Predikcija
StephCurry <- data.frame(Placa=place[1:5,3])
predict(fittocke, StephCurry)
evropski.standard <- StephCurry %>% mutate(Tocke=predict(fittocke, .))

#Koliko točk v resnici dosega 5 najbolje plačanih
top5place <- place %>% select(1,3) %>% top_n(5) 
top5tocke <- inner_join(top5place, statistika.ucinkovitosti, by = "Player") %>% 
  select(1,2,6)
top5zdruzena <- rbind(evropski.standard %>% mutate(tip="Evropski_standard"), top5tocke %>% transmute(Tocke=PTS, Placa=Salary, tip="Ameriški_standard")) %>% 
  mutate(Placa = Placa / 1e+06)
######################################################################################## 

# Izris Predikcije
ggplot(data=tockevsplaca%>% mutate(Placa = Placa/1e+06), aes(x=Tocke, y=Placa)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) + 
  geom_vline(data=top5_zdruzena, aes(xintercept=Tocke, y=Placa,colour=tip), size=1) + 
  geom_point(data=top5_zdruzena, aes(x=Tocke, fill=tip), shape=21, size=2) + 
  ggtitle("Evropejci vs top 5 (Američani)") + 
  ylab("Plača (v milijonih)") + xlab("Točke") +
  scale_colour_manual(name = 'Moja legenda 1', 
                      values =c('Evropejci'='red','Američani'='green'))+
  scale_fill_manual(name = 'Moja legenda 2', 
                    values =c('Evropejci'='purple','Američani'='brown'))+
  theme_bw()



##############################################################################################

# Povezava med plačo in učinkovitostjo meta (ranking)

metvsplaca.rank <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
metvsplaca.rank <- metvsplaca.rank[, c(1,3,9)]

#Izračun modela
fitmet.rank <- lm(EffectiveFieldGoal.rank ~ Salary.rank, data=metvsplaca.rank)
#Izris modela
ggplot(data=metvsplaca.rank, aes(x=EffectiveFieldGoal.rank, y=Salary.rank)) + geom_point(col="blue") + geom_smooth(method=lm, col="red")


# Povezava med plačo in učinkovitostjo meta
metvsplaca <- statistika.ucinkovitosti[, c(1,2,3)]
# Vzamem samo evropejce
metvsplaca <- filter(metvsplaca, Country %in% fiba.lestvica$Country)
# V tabelo dodam plače
metvsplaca <- inner_join(metvsplaca, place, by="Player")
metvsplaca <- metvsplaca[,c(-2,-4)]
names(metvsplaca) <- c("Player", "Met", "Placa")
#Izračun modela
fitmet <- lm(Met ~ Placa, data=metvsplaca)
ggplot(data=metvsplaca, aes(x=Met, y=Placa)) + geom_point() + geom_smooth(method=lm)


# Povezava med plačo in podajami rank
podajevsplaca.rank <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
podajevsplaca.rank <- podajevsplaca.rank[, c(1,3,13)]

# Povezava med plačo in skoki rank
skokivsplaca.rank <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
skokivsplaca.rank <- skokivsplaca.rank[, c(1,3,12)]

# Povezava med igralnim časom in plačo rank
minutevsplaca.rank <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
minutevsplaca.rank <- minutevsplaca.rank[, c(1,3,6)]

##############################################################################################

# Kako se več spremenljivk obnaša glede na plačo rank
ggplot() + geom_smooth(data=metvsplaca.rank, aes(x=EffectiveFieldGoal.rank, y=Salary.rank, col="Met"), se=FALSE, size=2) +
  geom_smooth(data=tockevsplaca.rank, aes(x=Points.rank, y=Salary.rank, col="Tocke"), se=FALSE, size=2) + 
  geom_smooth(data=podajevsplaca.rank, aes(x=Assists.rank, y=Salary.rank, col="Podaje"), se=FALSE, size=2) + 
  geom_smooth(data=skokivsplaca.rank, aes(x=Rebounds.rank, y=Salary.rank, col="Skoki"), se=FALSE, size=2) +
  geom_smooth(data=minutevsplaca.rank, aes(x=MinutesPlayed.rank, y=Salary.rank, col="Odigrane minute"), se=FALSE, size=2) +
  geom_smooth(data=ucinkovitostvsplaca, aes(x=Sum.rank, y=Salary.rank, col="Skupno"), se=FALSE, size=4) +
  ylab("Plača") + xlab("Rank") + 
  labs(title="Obnašanje več spremenljivk glede na plačo, ranking") +
  theme_bw() +
  theme(legend.position = c(0.15, 0.75), legend.text=element_text(size=10), legend.title = element_blank())

########################################################################################################  
























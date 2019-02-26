#######  NAPREDNA ANALIZA ######

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
#Izračun modela
fittocke <- lm(Tocke ~ Placa, data=tockevsplaca)
ggplot(data=tockevsplaca, aes(x=Tocke, y=Placa)) + geom_point() + geom_smooth(method=lm)

#Koliko točk na minuto bi moral dosegati Stephen Curry, najbolje plačan igralec v ligi, da bi imel plačo kot jo ima:
# Dodam igralce, da imam 5 najbolje plačanih v ligi
# Predikcija
StephCurry <- data.frame(Placa=place[1:5,3])
predict(fittocke, StephCurry)
evropski.standard <- StephCurry %>% mutate(Tocke=predict(fittocke, .))

######################################################################################## 

# Izris Predikcije
ggplot(data=tockevsplaca, aes(x=Tocke, y=Placa)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) + 
  geom_vline(data=evropski.standard, aes(xintercept=Tocke, y=Placa), color='red', size=1) + 
  geom_point(data=evropski.standard, aes(x=Tocke, y=Placa), color='purple', size=2) + 
  ggtitle("Evropejci vs top 5 (Američani)") + 
  ylab("Plača") + xlab("Točke") +
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
  geom_smooth(data=ucinkovitostvsplaca, aes(x=Sum.rank, y=Salary.rank, col="Overall"), se=FALSE, size=4) +
  ylab("Plača") + xlab("Rank") + 
  labs(title="Obnašanje več spremenljivk glede na plačo, ranking") +
  theme_bw()

########################################################################################################  

# Razvrščanje


evropejci.razvrscanje <-  inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player") %>% 
  select(1, 6, 7, 11, 16, 9) 










#######  NAPREDNA ANALIZA ######

# Povezava med placo in ucinkovitostjo Overall
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
# Izris Predikcije
ggplot(data=tockevsplaca, aes(x=Tocke, y=Placa)) + 
  geom_point() + 
  geom_smooth(method=lm) + 
  geom_point(data=evropski.standard, aes(x=Tocke, y=Placa), color='red', size=2) + 
  ggtitle("Evropejci vs top 5(Američani)") + 
  theme_bw()


#Kako gre top 5 v realnosti s točkami

                                               
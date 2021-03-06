# 3. faza: Vizualizacija podatkov
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(munsell)
library(tidyverse)



# Uvozimo zemljevid Sveta
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries") %>%
  fortify()


# Zemljevid sveta skrčimo na zemljevid Evrope
Evropa <- filter(zemljevid, CONTINENT == "Europe" |NAME == "Turkey")
Evropa <- filter(Evropa, long < 55 & long > -45 & lat > 30 & lat < 85)

# Narišemo zemljevid Evrope
#dev.off()
#ggplot() + geom_polygon(data = Evropa, aes(x=long, y=lat, group=group,fill=id)) +
#  theme(legend.position="none")
  
# Drzave v zemljevidu Evrope
drzave <- unique(Evropa$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Country"

# Da bom lahko povezoval zemljevid evrope z mojimi tabelami moram nekatere vrstice preimenovati v mojih tabelah



# Ker je držav preveč je legenda nesmiselna, Dodam omejitev da imam samo Evropo na manjšem zemljevidu
ggplot(Evropa, aes(x=long, y=lat, group=group, fill=NAME)) + 
  geom_polygon() + 
  labs(title="Evropa - osnovna slika") +
  theme(legend.position="none")
 

# Da bom lahko povezoval zemljevid evrope z mojimi tabelami moram nekatere vrstice preimenovati v tabeli st.igralcev
# Naredim novo tabelo, da ne bo prihajalo do težav kasneje
st.igralcev.eu <- st.igralcev
st.igralcev.eu <- as.data.frame(st.igralcev.eu, stringsAsFactors=FALSE)
st.igralcev.eu[,1] <- as.character(st.igralcev.eu[,1])
colnames(st.igralcev.eu) <- c("Country", "Players")
st.igralcev.eu[2,1] <- "Bosnia and Herz."
st.igralcev.eu[4,1] <- "Czechia" 

ujemanje <- left_join(drzave, st.igralcev.eu, by="Country")
ujemanje$Players[is.na(ujemanje$Players)]<- 0

###############################################################################################

# Izrišem zemljevid Evrope, v katerem bo vsaka država pobarvana glede št. igralcev v ligi NBA
ggplot() + geom_polygon(data=left_join(Evropa, ujemanje %>% filter(Players > 0), by=c("NAME"="Country")),
                        aes(x=long, y=lat, group=group, fill=Players)) +
  ggtitle("Števila NBA igralcev v posamezni evropski državi") + xlab("") + ylab("") +
  guides(fill=guide_colorbar(title="Število igralcev")) + 
  geom_point(aes(x=2, y=46)) + geom_text(aes(x=2, y=46), label = "Fra-10") +
  geom_point(aes(x=2, y=46)) + geom_text(aes(x=14.4, y=46), label = "Slo-1") +
  scale_fill_gradient2(low = "#008000", mid = "yellow", high = "red", midpoint = 4.5) 
  
##################################################################################################  



# Fiba lestvica: Europe rank vs players vs population per 10 mills

fiba.lestvica.plot <- fiba.lestvica[,c(1,4,5,6)]
fiba.lestvica.plot[3] <- fiba.lestvica.plot[3] / 10000000
names(fiba.lestvica.plot)[2:4] <- c("Evropa.rank",  "Populacija", "St.Igralcev")
fiba.lestvica.plot <- fiba.lestvica.plot %>%
  arrange(St.Igralcev,Populacija) %>%
  mutate(Country=factor(Country,levels=Country,ordered=T)) %>%
  filter(St.Igralcev > 0)
plot2.tidy <- melt(fiba.lestvica.plot, id.vars="Country", measure.vars=colnames(fiba.lestvica.plot)[-1])

ggplot(data=plot2.tidy %>% filter(variable == "Evropa.rank") %>%
         transmute(Country, Evropa.rank=value) %>%
         inner_join(plot2.tidy %>%
                      filter(variable %in% c("Populacija", "St.Igralcev"))),
       aes(x=Country, y=value, fill=variable)) +
  geom_col(position="dodge") +
  coord_flip() + ylab("Število igralcev na 10 milijonov") + xlab("Država") +
  labs(title="Primerjava populacije in števila NBA igralcev (na 10 milijonov)")



#  Fiba lestvica: Europe rank vs players per population (10 mills)
fiba.lestvica.plot.population <- fiba.lestvica[,c(1,4,5,6)]
fiba.lestvica.plot.population[3] <- fiba.lestvica.plot.population[3] / 10000000
names(fiba.lestvica.plot.population)[2:4] <- c("Evropa.rank",  "Populacija", "St.Igralcev")
fiba.lestvica.plot.population$St.IgralcevNa10milijonov <- fiba.lestvica.plot.population$St.Igralcev / fiba.lestvica.plot.population$Populacija
fiba.lestvica.plot.population <- fiba.lestvica.plot.population %>%
  arrange(St.IgralcevNa10milijonov) %>%
  mutate(Country=factor(Country,levels=Country,ordered=T))
plot.pop.tidy <- melt(fiba.lestvica.plot.population, id.vars="Country", measure.vars=colnames(fiba.lestvica.plot.population)[-1])

####################################################################################################
# Nisem vključil

ggplot(data=plot.pop.tidy %>% filter(variable == "Evropa.rank") %>%
         transmute(Country, Evropa.rank=value) %>%
         inner_join(plot.pop.tidy %>%
                      filter(variable %in% c("St.IgralcevNa10milijonov"))),
       aes(x=Country, y=value, fill=variable)) +
  geom_col(position="stack") +
  coord_flip() + ylab("Število igralcev") + xlab("Država") +
  labs(title="Primerjava populacije(/10 milijonov) in števila NBA igralcev")

##############################################################################################

# Zemljevid igralcev glede na populacijo

ggplot() + geom_polygon(data=left_join(Evropa, fiba.lestvica.plot.population[,c(1,5)] %>% filter(St.IgralcevNa10milijonov > 0) , by=c("NAME"="Country")),
                        aes(x=long, y=lat, group=group, fill=St.IgralcevNa10milijonov)) +
  ggtitle("Števila NBA igralcev v posamezni evropski državi na 10 milijonov prebivalcev") + xlab("") + ylab("") +
  guides(fill=guide_colorbar(title="Število igralcev")) +
  scale_fill_gradient2(low = "#008000", mid = "yellow", high = "red", midpoint = 15)
  
####################################################################################################  

# Izrišem graf, ki prikazuje povezavo plačo in odstotkom meta igralcev, glede na povprečje

povprecni.rang <- ((540*541)/2) / 540 # Logično: 270.5

plot1 <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
plot1 <- plot1[,c(1,3,6,9,11)]
colnames(plot1)[c(2:5)] <- c("Placa.rank", "OdigraneMinute.rank", "UcinkovitostMeta.rank", "Tocke.rank")

plot1.tidy <- melt(plot1, id.vars="Player", measure.vars=colnames(plot1)[-1])

ggplot(data=plot1.tidy %>% filter(variable == "Tocke.rank") %>%
         transmute(Player, Tocke.rank=value) %>%
         inner_join(plot1.tidy %>%
                      filter(variable %in% c("UcinkovitostMeta.rank", "Placa.rank"))),
       aes(x=Tocke.rank, y=value, fill=variable)) +
  geom_col(position="dodge", width=3) +
  geom_hline(yintercept=povprecni.rang, colour="green") + 
  labs(title="Rank plač in odstotka meta glede na povprečje")



# Overall ucinkovitost vs place; Graf

ucinkovitost.overall <- statistika.ucinkovitosti
ucinkovitost.overall$Sum.rank <- rowSums(ucinkovitost.overall[,11:18])
ucinkovitost.overall <- ucinkovitost.overall %>%
  arrange(Sum.rank) %>%  
  mutate(Sum.rank = 1:nrow(.))

ucinkovitost.evropejcev.overall <- filter(ucinkovitost.overall, Country %in% fiba.lestvica$Country)
ucinkovitost.evropejcev.overall <- ucinkovitost.evropejcev.overall[,-c(3:10)]

povprecni.rang <- ((540*541)/2) / 540 # Logično: 270.5

plot.overall <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev.overall, by="Player")
plot.overall <- plot.overall[,c(1,3,11,17)]
colnames(plot.overall)[c(2:4)] <- c("Placa.rank", "Tocke.rank", "Skupni.rank")

  
plot.overall.tidy <- melt(plot.overall, id.vars="Player", measure.vars=colnames(plot.overall)[-1])

ggplot(data=plot.overall.tidy %>% filter(variable == "Tocke.rank") %>%
         transmute(Player, Tocke.rank=value) %>%
         inner_join(plot.overall.tidy %>%
                      filter(variable %in% c("Skupni.rank", "Placa.rank"))),
       aes(x=Tocke.rank, y=value, fill=variable)) +
  geom_col(position="dodge", width=5) +
  geom_hline(yintercept=povprecni.rang, colour="green") +
 coord_flip() + ylab("Rank") + xlab("Igralec") + 
  labs(title="Rank plač in skupni rank učinkovitosti glede na povprečje") 





  

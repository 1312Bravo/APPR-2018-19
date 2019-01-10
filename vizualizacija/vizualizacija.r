# 3. faza: Vizualizacija podatkov
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(munsell)




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
colnames(st.igralcev.eu) <- c("Country", "Players")
st.igralcev.eu[2,1] <- "Bosnia and Herz."
st.igralcev.eu[4,1] <- "Czechia" 

ujemanje <- left_join(drzave, st.igralcev.eu, by="Country")
ujemanje$Players[is.na(ujemanje$Players)]<- 0

# Izrišem zemljevid Evrope, v katerem bo vsaka država pobarvana glede št. igralcev v ligi NBA
ggplot() + geom_polygon(data=left_join(Evropa, ujemanje, by=c("NAME"="Country")),
                        aes(x=long, y=lat, group=group, fill=Players)) +
  ggtitle("Number of NBA players in each country") + xlab("") + ylab("") +
  guides(fill=guide_colorbar(title="Number of players"))
  
  




# 3. faza: Vizualizacija podatkov
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)

# Uvozimo zemljevid Sveta
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                             "ne_110m_admin_0_countries") %>%
  fortify()


# Zemljevid sveta skrčimo na zemljevid Evrope
Evropa <- filter(zemljevid, CONTINENT == "Europe" |NAME == "Turkey")
Evropa <- filter(Evropa, long < 90 & long > -40 & lat > 30 & lat < 85)

# Narišemo zemljevid Evrope
dev.off()
ggplot() + geom_polygon(data = Evropa, aes(x=long, y=lat, group=group,fill=id)) 
# Drzave v zemljevidu Evrope
drzave <- unique(Evropa$NAME) 

# Ker je držav preveč je legenda nesmiselna, Dodam omejitev da imam samo Evropo na manjšem zemljevidu
ggplot(Evropa, aes(x=long, y=lat, group=group, fill=NAME)) + 
  geom_polygon() + 
  labs(title="Evropa - osnovna slika") +
  theme(legend.position="none")# + 
  #coord_map(xlim = c(-40, 52),ylim = c(32, 82))
  


ggplot(data=evropejci, aes(x=evropejci$Player, y=evropejci$Country)) + geom_point()

ggplot(data=evropejci %>% filter(Country=="Croatia"), aes(x=Player, y=Country)) + geom_point()

# ggplot(data=evropejci, aes(x=evropejci$Player, y=evropejci$Country)) + geom_line()
# --> Negre, ker nimam pri vsaki drzavi samo enega igralca I guess

# ggplot(data=place, aes(x=place$Player, y=place$Salary)) + geom_line()
# --> To pa nevem zakaj ne gre

ggplot(data=st.igralcev, aes(x=Country, y=Players)) + geom_boxplot() 

# y -> st. drzav, ki ima x igralcev; x -> st. igralcev
ggplot(data=st.igralcev, aes(x=Players)) + geom_histogram() 
# Prikaz s procenti na y osi in povezan s krivuljo
ggplot(data=st.igralcev, aes(x=Players)) + geom_density()




ggplot(data=fiba.lestvica, aes(x=Europerank, y=Players)) + geom_point()
ggplot(data=fiba.lestvica, aes(x=Europerank, y=Players)) + geom_line()

ggplot(data=fiba.lestvicaNBA %>% filter(Europerank<=10), aes(x=Country, y=Players)) + geom_point()

# place vs tocke
placeVstocke <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
placeVstocke <- placeVstocke[,c(1,4,12)] %>% arrange(Salary.rank)
ggplot(data=placeVstocke, aes(x=Salary.rank, y=Points.rank)) + geom_line()


ggplot(data=zaupanje.evropejcem, aes(x=circumference)) + geom_histogram()

# Nič pametnega
ggplot(data=zaupanje.evropejcem, aes(x=Salary.rank, fill=Country))  + geom_histogram()
ggplot(data=zaupanje.evropejcem, aes(x=Salary.rank, color=Country))  + geom_histogram()
ggplot(data=ucinkovitost.evropejcev, aes(x=Points.rank, fill=Country))  + geom_histogram(color="black") 






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



ggplot(data=zaupanje.evropejcem, aes(x=circumference)) + geom_histogram()

# Nič pametnega
ggplot(data=zaupanje.evropejcem, aes(x=Salary.rank, fill=Country))  + geom_histogram()
ggplot(data=zaupanje.evropejcem, aes(x=Salary.rank, color=Country))  + geom_histogram()
ggplot(data=ucinkovitost.evropejcev, aes(x=Points.rank, fill=Country))  + geom_histogram(color="black") 


################################################################################################
# place vs tocke
placeVstocke <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
placeVstocke <- placeVstocke[,c(1,3,11)] %>% arrange(Salary.rank)
ggplot(data=placeVstocke, aes(x=Salary.rank, y=Points.rank)) + geom_line()


# Minute vs tocke
minuteVstocke <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
minuteVstocke <- minuteVstocke[,c(1,6,11)]
ggplot(data=minuteVstocke,aes(x=MinutesPlayed.rank, y=Points.rank)) + geom_line(colour="red")

# Minute vs (place, tocke)
minuteVsplacetocke <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
minuteVsplacetocke <- minuteVsplacetocke[,c(1,3,6,11)]

ggplot(data=minuteVsplacetocke, aes(x=MinutesPlayed.rank))+
  geom_point(aes(y=Points.rank), colour="red") +
  geom_point(aes(y=Salary.rank), colour="green") +
  geom_line(aes(y=povprecni.rang), colour="blue") +
  labs(title= "Points per minute, salary rank") 



# Fiba lestvica: Europe rank vs players vs population per 10 mills
fiba.lestvica.plot <- fiba.lestvica[,c(1,4,5,6)]
fiba.lestvica.plot[3] <- fiba.lestvica.plot[3] / 10000000


ggplot(data=fiba.lestvica.plot, aes(x=Europerank)) + 
  geom_line(aes(y=Population), colour="red") + 
  geom_line(aes(y=Players), colour="green") 




# Povprecni rang igralca 
povprecni.rang <- ((540*541)/2) / 540 # Logično: 270.5

# Graf, glede na povprečje
# X os ni važna, izbriši?
plot1 <- inner_join(zaupanje.evropejcem, ucinkovitost.evropejcev, by="Player")
plot1 <- plot1[,c(1,3,6,9,11)]


ggplot(data=plot1, aes(x=Points.rank), col=supp) +
  geom_point(aes(y=EffectiveFieldGoal.rank), colour="red") +
  geom_line(aes(y=povprecni.rang), colour="green") + 
  labs(title="Relative to the average rank") +
  geom_point(aes(y=Salary.rank))
                                        

  


# Razlikovanje od povprečja pri številu igralcev na 10 mills
#povprecje <- mean(fiba.lestvicaNBA$PlayersPer10Million)
#odstopanje <- ujemanje
#odstopanje$PlayersPer10Million <- odstopanje$PlayersPer10Million - povprecje





ggplot(data=evropejci, aes(x=evropejci$Player, y=evropejci$Country)) + geom_point()

ggplot(data=evropejci %>% filter(Country=="Croatia"), aes(x=Player, y=Country)) + geom_point()

# ggplot(data=evropejci, aes(x=evropejci$Player, y=evropejci$Country)) + geom_line()
# --> Negre, ker nimam pri vsaki drzavi samo enega igralca I guess

# ggplot(data=place, aes(x=place$Player, y=place$Salary)) + geom_line()
# --> To pa nevem zakaj ne gre

ggplot(data=st.igralcev, aes(x=Country, y=Players)) + geom_boxplot() 

# y -> st. drzav, ki ima x igralcev; x -> st. igralcev
ggplot(data=st.igralcev, aes(x=Players)) + geom_histogram() 





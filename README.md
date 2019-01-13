## Projekt pri Predmetu APPR v šolskem letu 2018/19

* [![Shiny](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/1312Bravo/APPR-2018-19/master?urlpath=shiny/APPR-2018-19/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/1312Bravo/APPR-2018-19/master?urlpath=rstudio) RStudio


## Naslov

Primerjava igralcev v ligi NBA glede na nacionalnost

## Avtor

 Urh Peček

## Osnovna ideja

- Igralce bom med seboj ločil glede na nacionalnost in jih primerjal po več kriterijih:
  - Statistični indeksi
  - dvig/padec forme
  - plača
  - poškodbe
  - pregled sezon (16,17) 18, predikcija za 2019 (tekoča)

## Glavni viri: 
- https://www.basketball-reference.com/ -> najbolj obsežna statistika, glaven vir
- http://www.espn.com/nba/statistics
- https://stats.nba.com/
- https://www.kaggle.com/jacobbaruch/nba-player-of-the-week -> igralci tedna 
- https://en.wikipedia.org/wiki/List_of_foreign_NBA_players -> Tuji igralci (v preteklosti, trenutno, lahko urediš)
- https://en.wikipedia.org/wiki/2018%E2%80%9319_NBA_season -> sezone 2017/18

  

# Plan dela
Podatke bom zbral iz sezone 17/18. 
Igralce bom med sabo primerjal po več kriterijih:
  - delež tujcev (ne Američanov) na vseh petih pozicijah.
  - povprečje točk glede na pozicije in na splošno
  - igralne minute glede na pozicije in na splošno
  - plača glede na pozicije in na splošno
  - število metov glede na pozicije in na splošno (Zaupanje ekipe posameznemu igralcu)
  - Število evropskih igralcev tedna

Naknadno bom primerjal med sabo še posamezne Evropske države:
  - Zbral podatke iz zgornje primerjave in vpeljal dodatne kriterije:
    - delež igralcev posamezne države
    - delež igralcev v NBA glede na število prebivalcev posamezne države
    - delež igralcev posamezne države  v NBA glede na ranking na lestvici FIBA  
    

  
  

##  Posodobljen plan dela $5.12.2108$
   Podatki bodo is sezone 2017/18


  Začel bom s povzetkom statističnih podatkov skozi celotno sezono, nato pa po potrebi dodal še statistike           posameznih tekem.

#  1) Groba statistika sezone
  https://www.basketball-reference.com/leagues/NBA_2018_totals.html
  
  Urejena tabela: https://www.basketball-reference.com/pi/shareit/U2cwM 
  tabela 1) CSV.txt
    
  Stolpci:
    - Player
    - Pos	(position)	
    - G (games played)
    -	MP (minutes played)
    - eFG% (efective field goal percentage)
    - FT% (free throw percentage)
    -	TRB (total rebounds)	  
    - AST	(asists)
    - STL (steals)
    - BLK (blocks)	
    - TOV	(turnovers)
    - PF (personal fouls)	
    - PTS (points)
  
 
  Nato bom igralce uredil po nacionalnosti.
  Razdelil jih bom na tujce in na američane, nato pa tujce še po posameznih državah.
  Za to bom uporabil tabelo iz wikipedije, ki jo bo treba še urediti, saj vsebuje tudi igralce, ki ne igrajo več,    vendar s tem ne bo velikih držav saj so še vedno aktivni igralci označeni z '*'.
  

#  2) Nacionalnost
   https://en.wikipedia.org/wiki/List_of_foreign_NBA_players
   Tabele še nisem prenel saj še neznam.
   
   http://pr.nba.com/nba-international-players-2017-18/
   Ta tabela je boljša. (View source so podatki v XML)
   
   
# 3) Združitev obstoječih tabel: grobe statistike in nacionalnosti
  Nastane nova tabela z dodano nacionalnostjo -> "Skupna tabela"
  

# 4) Dodatek: plače
  https://hoopshype.com/salaries/players/2017-2018/
  Skupni tabeli bom dodal še plače igralcev
  
# 5) Fiba ranking
  http://www.fiba.basketball/rankingmen#|tab=fiba_europe 
  Fiba_ranking.txt

## Tabele  

Tabela 1): TABELA_1)
TOP 20:
-points
-asists
-rebounds
-steals
-turnovers
-field goal percentage
-field goal attempts
-minutes played

Analiziral bom število Evropejcec med TOP 20 in kje so.

Tabela 2):
TOP 10 Non Europeans vs top 10 Europeans:
-points
-asists
-rebounds
-steals
-turnovers
-field goal percentage
-field goal attempts
-minutes played

Tabela 3)
Samo evropejci v NBA
-koliko jih prihaja iz katere države
-število igralcev države glede na število prebivalcev te države
-število igralcev glede na fiba ranking te države


  
 
  
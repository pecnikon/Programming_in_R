zelenina = c("Zelená okurka", "Červená paprika", "Zelená paprika", "Žlutá paprika")

# UPRAVY TEXTU
# jak se automaticky zbavit velkych pismen
zelenina = tolower(zelenina)

# vsechna pismena na velka
toupper(zelenina)

# zmenit na nazev
??str_to_title()
install.packages("stringr")
library(stringr)
source(stringr)
stringr::str_to_title(zelenina) # kazde prvni pismeno ve slove velke

# zbavit se diakritiky
iconv("žába", to = "ASCII//TRANSLIT")
iconv(zelenina, to = "ASCII//TRANSLIT")

# evaluace textu
x = c("zelenina")
eval(parse(text = x)) # to same jako napsat zelenina

rm(x)

# jak rozpojit text na vice casti:

    # 1) podle poctu znaku
      substr(x = zelenina, start = 1, stop = 5) # prvnich 5 znaku
      substr(x = zelenina, start = nchar(zelenina)-5, stop = nchar(zelenina)) # poslednich 5 znaku

    # 2) podle konkretniho znaku (napr. rozdeli tam kde je mezera)
      strsplit(x = zelenina, split = " ")
      strsplit(x = zelenina, split = " ") [[1]][1] # kdyz chci vypsat jen cast listu (prvni prvek a z nej prvni hodnotu)
      str(strsplit(x = zelenina, split = " ")) # ukaze strukturu vystupu
      zelenina2 = unlist(strsplit(x = zelenina, split = " ")) # zjednodusit list na vektor
      
    # 3) na vektor
      c(1:3)%/%2 # vysledek celociselneho deleni
      c(1:3)%%2 # zbytek celociselneho deleni

# chceme vypsat jen barvy, tedy liche prvky vektoru zelenina2
zelenina2 [(1:length(zelenina2)%%2)==1] # aby bylo liche, zbatek se musi rovnat jedne, vypise barvy
zelenina2 [(1:length(zelenina2)%%2)==0] # aby bylo sude, zbatek se musi rovnat nule, vypise zeleninu

# vyhledavani uvnitr retezcu
grep(pattern = "paprika", x = zelenina) # najde pozice, kde to naslo papriku
zelenina[grep(pattern = "paprika", x = zelenina)] # najde objekty, kde to naslo papriku

# nahrazovani casti retezcu
sub(pattern = "paprika", replacement = "cuketa", x = zelenina)

fix(zelenina) # muzu menit puvodni vektor, jednorazove upravy rucne
x = data.frame(1:8, zelenina2)
fix(x)

# PRACE S CASOVYMI DATY (casove zony)
# univerzalni casovy format: Posixct / Posixlt
ted = Sys.time()
str(ted)
Sys.getenv() # sada systemovych parametru
Sys.setenv(tz = "UTC")
ted
Sys.setenv(tz = "CET")
ted
as.POSIXct(as.numeric(ted), origin = "1980-01-01") # defaultni origin je 01.01.1970, timto nastavim vlastni
ted2 = as.POSIXlt(Sys.time()) # NEDOPORUCUJE SE
unlist(ted2)
ted2-ted
difftime(time1 = ted, time2 = ted2, units = "secs") # pocitani se spravne definovanym casem
ted*ted2 # NELZE, nasobeni casovuch hodnot nedava smysl

cas = "19.10.23 15hod 14min 01sek" # chceme z toho udelat spravny format
?strftime # zde najdu definici a pouziti formatu
cas2 = as.POSIXct(cas, format = "%d.%m.%y %Hhod %Mmin %Ssek")

# PODMINKY A SMYCKY

# Podminky

# ifelse()
    # vypsat zda je cislo sude nebo liche
    
    x = sample(-100:100, size = 1)
    ifelse(test = x%%2==0, yes = "Cislo je sude", no = "Cislo je liche")
    
    # to same napsat filtrem
    podminka = x%%2==0
    vysledek = c("Cislo je sude", "Cislo je liche")
    vysledek[c(podminka,!podminka)]
    
    ifelse(test = x%%2==0, yes = paste("Cislo",x,"je sude"), no = paste("Cislo" , x , "je liche"))


# if() else()
    if(x%%2==0){
      print("Cislo je sude")
    } else {
      print("Cislo je liche") # nebo misto print napsat cat
    }

# Cviceni: napiste podminku ktera vypise zda je cislo sude a zaroven vetsi nez 0
    
    {
      {
        x = sample(-100:100, size = 1)
        if (x %% 2 == 0) {
          if (x > 0) {
            cat(paste("Cislo je kladne sude "))
          } else {
            cat(paste("Cislo je zaporne sude "))
          }
        } else {
          if (x > 0) {
            cat(paste("Cislo je kladne liche "))
          } else {
            cat(paste("Cislo je zaporne liche "))
          }
        }
      }
    }

  x=c("brambora")
  switch(x,
         psenice = {
           cat(paste(x, "dobre kyne"))
         },
         zito = {
           cat(paste(x, "kyne spatne"))
         },
         brambora = {
           cat(paste(x, "nekyne vubec"))
         })
  
# Smycky
  # for loop (pracuje pro predem dany pocet opakovani)
   # vypsat cisla 1 az 10
  for (i in 1:10) {
    print(i)
  }
  
  for (i in c("jablko", "hruska", "tresen")) {
    print(i)
  }
  
  mozne.vysledky = c("jablko", "hruska", "tresen")
  for (i in 1:length(mozne.vysledky)) {
    print(mozne.vysledky[i])
  }
  
    # Cviceni: pomoci smycky vypsat malou nasobilku
  # 1. reseni
  nasobilka = matrix(0, nrow = 10, ncol = 10)
  for (i in 1:10) {
    nasobilka[i, ] = i * (1:10)
  }

  # 2. reseni
  for (i in 1:10) {
    for (j in 1:10) {
      nasobilka[i, j] = i * j
    }
  }
 
 # while loop (pracuje dokud se nesplni nejaka podminka) ! pozor na nekonecne smycky
  # 1D prochazka po minovem poli
  zivot="zivy"
  while (zivot=="zivy") {
    prst.VYBUCH = 0.1
    zivot = sample(c("zivy", "mrtvy"), size = 1, prob = c(1-prst.VYBUCH, prst.VYBUCH))
    if(zivot=="zivy") {
      readline(prompt = "Uf, pojd bliz!")
    } else {
      print("Au, uz nechod!")
    }
  }
  
 

  
  # repeat loop (pracuje porad, regulace je na uzivateli)
  
  
  
  
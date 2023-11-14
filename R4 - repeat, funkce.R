# napsat cyklus ktery spocita druhe mocniny cisel 1:10
# for cyklus
for (i in 1:10) {
  print(i**2)
}

# while cyklus
i = 1
while (i<=10) {
  print(i**2)
  i = i + 1
}

# repeat cyklus:
  # stop("chybova hlaska") -  prerusi cyklus s chybou
  # break - prerusi cyklus bez chyby
  # next - spusti dalsi beh cyklu
  # warning ("varovna hlaska") - spusti dalsi beh cyklu zaroven s varovnou hlaskou
i = 1
repeat {
  if (i <= 10){
    print(i**2)
    i = i + 1
    next
  } else {
    break
  }
}

# nekonecny cyklus
repeat{
  print(Sys.time())
  Sys.sleep(1)
}

# nechame vygenerovat nahodne cislo 1:20
# nechame nekoho hadat to cislo
# v pripade, ze se netrefi, tak pujde na dalsi pokus s hlaskou
    # "zkus dal!"
# v pripade, ze se trefi, tak skonci s chybou 
    # "urcite jsi svindloval!"

tip = readline(prompt = "Jake cislo si myslim:") # do konzole pak napisu text a ten se mi prepise mezi values jako tip


cislo = sample(x = 1:20, size = 1)
repeat{
  tip = readline(prompt = "Jake cislo si myslim:")
  if (tip == cislo) {
    stop("urcite jsi svindloval")
  } else {
    message("zkus dal")
    next
  }
}

# pokud tipneme - hlaska "Dobre ty!" - a preruseni bez chyby
# pokud tip bude mimo rezmezi 1:20 - varovani "Cislo musi byt mezi 1 a 20" a pujde dal

cislo = sample(x = 1:20, size = 1)

repeat{
  tip = readline(prompt = "Jake cislo si myslim:")
  if (!tip %in% 1:20) {
    message("Cislo musi byt mezi 1 a 20")
    next
  } else if (tip == cislo) {
    message("Dobre ty")
    break
  } else {
    message("zkus dal")
    next
  }
}


# FUNKCE
# funkce na treti odmocninu
to = function(x){
  t_o = x**(1/3)
  return(t_o)
}

to(25:32)

formals(to) # argumenty
body(to) # telo funkce
environment(to) # prostredi, kde je funkce vytvorena
# kde vsude mohou byt funkce
environment(ncol)

# externi knihovny
  install.packages("lme4")
  library(lme4)

# nahrani ulozene funkce ze souboru
  source(file = "to.R")

# kod jiz existujicich funkci
  chisq.test # kdyz funkci napisu bez zavorky, ukaze se jeji kod, muzu vyuzit k pochopeni funkce

# genericke funkce - na kazdou funkci funguji jinak, pouzivaji metody
  body(seq)
  methods(seq)
  body(seq.default)
  methods(summary)
?methods
# funkce primitivni
  sqrt
  body(sqrt)

# vytvorte funkci "je_delitelne", ktera overi, zda cislo a je delitelne cislem b beze zbytku
  3/2 # normalni deleni
  5%/%2 # celociselne deleni
  5%%2

je_delitelne = function(a, b){
  if(a%%b == 0){
    return("ANO")
  } else {
    return("NE")
  }
}
je_delitelne(a = 7, b = 3)

# predelat, aby:
  # a mohl byt i vektor cisel
  # misto ANO/NE to vracelo TRUE/FALSE
  # argument B byl defaultne nastaven na 2

je_delitelne = function(a, b = 2){
  odpoved = a%%b == 0
  return(odpoved)
}

je_delitelne(4, 3)

# predelat, aby:
  # fce otestovala, zda jsou oba argumenty numeric. pokud ne - chyba "Argumenty a i b musi byt cislo!"
  # fce zkontrolovala, ze argument b neni 0. pokud je - varovani "Nulou se nedeli ani v nedeli"

je_delitelne = function(a, b = 2) {
  if (!is.numeric(a)|!is.numeric(b)) {
    stop("Argumenty a i b musi byt cislo!")
  } else {
    if (b == 0) {
      warning("Nulou se nedeli ani v nedeli!")
    } else{
  odpoved = a %% b == 0
      return(odpoved)
    }
  }
}

# predelat fci tak, aby:
  # vracela:
    # odpovedi
    # zbytky
    # data (list slozeny z argumentu a a b)
  # priradime vystupu tridu "delitelnost"

je_delitelne = function(a, b = 2) {
  if (!is.numeric(a)|!is.numeric(b)) {
    stop("Argumenty a i b musi byt cislo!")
  } else {
    if (b == 0) {
      warning("Nulou se nedeli ani v nedeli!")
    } else{
  odpovedi = a %% b == 0
  zbytky = a %% b
  vystup = list(odpovedi = odpovedi, 
                zbytky = zbytky, 
                data = list(a,b))
  class(vystup) = "delitelnost"
      return(vystup)
    }
  }
}

x = je_delitelne(1:50, 5)
class(x)
x$zbytky [5:8] # vypsat paty az osmy zbytek (opakovani z minulych hodin)

summary.delitelnost = function(x) { #udela metodu summary ktera funguje pro tridu delitelnost
  cat(paste("\n", # posouva na dalsi radek, graficka uprava
            "celkem bylo testovano", 
            length(x$odpovedi),
            "hodnot" ,
            "\n",
            "z toho beze zbytku je delitelnych",
            length(x$odpovedi[x$odpovedi==T]),
            "hodnot"))
}

summary(x)
methods(summary)





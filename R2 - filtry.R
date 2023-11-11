# napoveda tabulatorem
?rnorm
set.seed(11) # vsem, kteri maji tento seed se hodnoty vygeneruji stejne, musim to vzdy pustit i se seedem,
# aby se hodnoty nemenily

# hmotnosti samcu a samic pred kladenim
m1f=round(abs(rnorm(n = 129, mean = 0.28, sd = 0.13)), digits = 2)
m1m=round(abs(rnorm(n = 129, mean = 0.29, sd = 0.15)), digits = 2)

# hmotnosti samcu a samic po kladeni
m2f=round(abs(rnorm(n = 129, mean = 0.19, sd = 0.08)), digits = 2)
m2m=round(abs(rnorm(n = 129, mean = 0.27, sd = 0.13)), digits = 2)

# pocet vajec pro samce i samice, stejne hodnoty dvakrat
vajec= rep(round(rnorm(n = 129, mean = 6000, sd = 500), digits = 0), 2)

# nakazeni, pravdepodobnost 0.24
tularemie= sample(x = c("ANO","NE"), size = 258, replace = T, prob = c(0.24, 1-0.24))

# tabulka
pijaci=data.frame(m1f, m1m, vajec, tularemie)

head(pijaci, n = 10) # zobrazi zacatek tabulky (n=pocet zobrazenych radku)
tail(pijaci, n = 10) # zobrazi konec tabulky

rownames(pijaci) # ukaze nazvy radku
colnames(pijaci) # ukaze nazvy sloupcu
colnames(pijaci)[1:2] # ukaze jen hodnoty v hranate zavorce, kdyz pridam '=c("xx", "yy")', nahradim nazvy
colnames(pijaci)[c(T, T, F, F)] # to, kde je T, ty hodnoty ukaze, F se nezobrazi

# podminky
# pocty vajec nakazenych jedincu
pijaci$vajec[pijaci$tularemie=="ANO"]

podminka=pijaci$tularemie=="ANO"
pijaci$vajec[which(podminka)] # zobrazi pozice kde plati podminka, ekvivalentni postup jako ten predchozi


# na.action="na.omit"

podminka=pijaci$tularemie=="ANO"
pijaci$vajec[podminka] # vypise TRUE i NA
pijaci$vajec[which(podminka)] # vypise pouze TRUE

# jak se zbavit NA
na.omit(podminka) # funguje i na datove ramce, vyradi cely radek
na.omit(pijaci)
pijaci[!is.na(podminka), ] # musi se napsat podminka pro radek i sloupec (je to tabulka), ale pro sloupec 
        # podminku nemame, takze prazdno za carkou
pijaci[pijaci$tularemie=="ANO", ] # pijaci nakazeni tularemii

# vypsat vsechny pijaky, kteri maji vice nez 5500 vajec
pijaci[pijaci$vajec>5500, ]
nrow(pijaci[pijaci$vajec>5500, ]) # kolik je pijaku s vetsim poctem vajec nez 5500
dim(pijaci) # ukaze pocet radku a sloupcu tabulky

# vypsat pocet vajec zdravych pijaku
pijaci$vajec[which(pijaci$tularemie=="NE")]

# pocet vajec zdravych pijaku, kde je hmotnost samcu mensi nebo rovna (<=) 0.29
pijaci$vajec[which(pijaci$tularemie=="NE"&pijaci$m1m<=0.29)]

# vypsat pijaky, kde je nadprumerny samec nebo samice
pijaci[which(pijaci$m1f>mean(pijaci$m1f)|pijaci$m1m>mean(pijaci$m1m)),]

# je alespon jeden samec tezsi nez 0.3g?
any(pijaci$m1m>0.3)

# jsou vsichni samci tezsi nez 0?
all(pijaci$m1m>0) # da se pouzit jako kontrola

# existuje objekt s nazvem pijaci?
exists(x = "pijaci")

# vsechny vytvorene objekty, existuje mezi nimi objekt "pijaci"?
any(ls()=="pijaci")

# je neco uvnitr nejakeho objektu?
"pijaci" %in% ls()


set.seed(12)
# tabulka samice kde budou pouze hodnoty samic
samice = data.frame(m1=m1f, 
                    m2=m2f, 
                    vajec=vajec[1:129],
                    tularemie = sample(x = c(T,F), size = 129, replace = T, prob = c(0.24, 1-0.24)))

# tabulka samcu kde budou pouze hodnoty samcu
samci = data.frame(m1=m1m, 
                   m2=m2m, 
                   vajec=vajec[1:129],
                   tularemie = sample(x = c(T,F), size = 129, replace = T, prob = c(0.24, 1-0.24)))


# rbind() spojuje po radcich 
pijaci = rbind(samci, samice) # aby funkce fungovala, musi byt stejny pocet sloucu v data.framech se stejnymi nazvy

pijaci$pohlavi = rep(c("samci", "samice"), each=129) # priradim pohlavi

# potrebuji zjistit typ promennych (strukturu)
str(pijaci)

pijaci=type.convert(pijaci, as.is=T) #zmeni datove typy na ty spravne
pijaci$pohlavi=as.factor(pijaci$pohlavi)
summary(pijaci$pohlavi)

# cbind() spojuje po sloupcich
pijaci2=cbind(samci,samice)

# specialni (NEDOPORUCENE) zpusoby spojovani tabulek:
# head(merge(samice,samci,by=c("cislo_paru"))) - prunik
# head(merge(samice,samci,by=c("cislo_paru","tularemie"),all=T)) - sjednoceni

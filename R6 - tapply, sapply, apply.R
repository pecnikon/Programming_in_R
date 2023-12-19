# TRIDA APPLY
data("ChickWeight")
# jakymkoliv zpusobem spocitat prumernou hmotnost hmotnost kurat pro kazdou dietu / kure

df = ChickWeight
names(df) = tolower(names(df))
mean(df$weight[df$diet == "1"])
mean(df$weight[df$diet == "2"])
mean(df$weight[df$diet == "3"])
mean(df$weight[df$diet == "4"])

# for cyklem
for(i in levels(df$diet)){
  print(mean(df$weight[df$diet == i]))
}

#_____________________________________________________________________________
# FUNKCE TAPPLY
tapply(X = df$weight, INDEX = df$diet, FUN = mean)
tapply(X = df$weight, INDEX = df$diet, FUN = mean) [2] # zobrazi druhou hodnotu

# prumerna hmotnost kurat
prumery = tapply(X = df$weight, INDEX = df$chick, FUN = mean)
max(prumery) # nejvetsi hmotnost kurat
which(prumery == max(prumery))
which.max(prumery) #  LEPSI

head(df) # ukaze prvnich 6 hodnot
tapply(df$weight, INDEX = df$chick, FUN = head) # prvnich 6 vah pro kazde kure     
str(tapply(df$weight, INDEX = df$chick, FUN = head)) # ukaze strukturu   
 
tapply(df$weight, INDEX = df$chick, FUN = "[") # vypise vsechny hodnoty uvnitr     

# spocitat stredni chybu prumeru (SEM) pro kazdou dietu
# SEM = sd(x)/sqrt(n)

sem = function(x){
  SEM = sd(x)/sqrt(length(x))
  return(SEM)
}

tapply(X = df$weight,INDEX = df$diet, FUN = sem)
tapply(X = df$weight,INDEX = df$diet, FUN = function(x){sd(x)/sqrt(length(x))}) # uzitecne kdyz nechci funkci ukladat

# Trideni dle vice faktoru
df$sex = "M"
df$sex[df$chick %in% 1:20] = "F"
df$sex[df$chick[1:25]] = "M"
df$sex = as.factor(df$sex)

# spocitat prumernou hmotnost kurat v ruznych dietach pro kazde pohlavi zvlast
tapply(X = df$weight, INDEX = list(df$diet, df$sex), FUN = mean, na.rm = T)
#_____________________________________________________________________________
# APPLY (aplikuje pres cely radek nebo cely sloupec)
apply(X = df, MARGIN = 2, class) # nefunguje, lze aplikovat pouze na matice

data("AirPassengers") # time series, chci prevest do matice
colnames(AirPassengers) # nefunguje
plot(AirPassengers)
attributes(AirPassengers) # vypise atributy
attr(AirPassengers, which = "tsp")
.preformat.ts(AirPassengsers)
df2 = matrix(AirPassengers, ncol = 12, byrow = T, 
             dimnames = list(rownames(.preformat.ts(AirPassengers)), 
                             colnames(.preformat.ts(AirPassengers))))

#prumerny pocet pasazerupro vsechny mesice
apply(X = df2, MARGIN = 2, mean)

#prumerny pocet pasazerupro vsechny roky
apply(X = df2, MARGIN = 2, mean)

data("Titanic")
str(Titanic)
apply(X = Titanic, MARGIN = c(1,2,3,4), "[")

# spocitejte sumu prezivsich a neprezivsich pasazeru
apply(X = Titanic, MARGIN = 4, FUN = sum)

# suma dospelych a deti kteri prezili/neprezili
apply(X = Titanic, MARGIN = c(3,4), FUN = sum)

#suma pohlavi, vek a preziti
apply(X = Titanic, MARGIN = c(2,3,4), FUN = sum)

# suma pres vek, tridu a preziti
apply(X = Titanic, MARGIN = c(1,3,4), FUN = sum)

# pravdepodobnost ze dospeli/dei prezili/neprezili
apply(X = apply(X = Titanic, MARGIN = c(3,4), FUN = sum), MARGIN = 1, FUN = function(x){x/sum(x)})

# _________________________________________________________________________

# FUNKCE SAPPLY - aplikuje funkce na kazdy prvek objektu (hlavne na seznamy), kdyz je to mozne, vystup je data frame
# FUNKCE LAPPLY - to stejne ale vystup je list

l1 = list(df, df2)
sapply(X = l1, FUN = head) # kdyz maji tabulky ruzny pocet vstupu
lapply(X = l1, FUN = head)

# aplikace funkce class na sloupce kazdeho prvku seznamu
sapply (X = l1, FUN = apply, MARGIN = 2, FUN = class) # nefunguje, nevi, ktere FUN ma pouzit
sapply (X = l1, FUN = apply, MARGIN = 2, class) # funguje

# _________________________________________________________________________
  # funkce REPLICATE je alternativa cyklu
replicate(n = 10, expr = sample(c(T,F)))

# <<- operator pro ukladani do global environment

i = 1
replicate(10, expr = {
  i<<-i+1
  print(i)
})
#___________________________________________________________________________
# mnohonasobna sumarizace
# balicky plyr x data table (data table lepsi)
install.packages("plyr")
install.packages("data.table")
library(plyr)
tapply(df$weight[df$sex == "M"], df$diet[df$sex == "M"], length)

# pocitame najednou pocty samcu a samic
ddply(.data = df, .variables = "diet", .fun = summarise, 
      samci = length(sex[sex == "M"]), samice = length(sex[sex == "F"]))

# pocitame najednou prumer i sd hmotnosti kurat
ddply(.data = df, .variables = "diet", .fun = summarise, 
      prumer = mean(weight), sdd = sd(weight))

# to stejne v data table
library(data.table)
dt = as.data.table(df)
# sumarizace prumerne hmotnosti a smodch hmotnosti pres dietu a pohlavi
dt[,mean(weight), by = .(diet)] # prumerna hmotnost pro vsechny diety
dt[,.(prumer = mean(weight), sdd = sd(weight)), by = .(diet, sex)] # prumerna hmotnost pro vsechny diety
dt[,.N, by = .(diet, sex)]

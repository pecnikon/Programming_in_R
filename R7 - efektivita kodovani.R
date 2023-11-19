# Efektivita kodu (rychlost vypoctu)
  # 1. Pracovat s celymi vektory
  # 2. Nenastavovat velikost objektu
  # 3. Pouzivat funkce napsane v efektivnejsim jazyce (C++)
  # 4. Zvolit si spravny datovy typ (matice nejrychlejsi, pak data.table)
  # 5. Davat si pozor na pocet operaci
  # 6. Paralelni procesovani

system.time(rnorm(1e7))
install.packages("microbenchmark")
install.packages("memoise")
install.packages("compiler")
library(microbenchmark)
library(data.table)

# ALTERNATIVA
pkg = sapply(c("microbenchmark", "data.table", "memoise", "compiler"), 
       require, 
       character.only = T)

sapply(c("microbenchmark", "data.table", "memoise", "compiler") [!pkg], 
       install.packages, 
       character.only = T)

pkg = sapply(c("microbenchmark", "data.table", "memoise", "compiler") [!pkg], 
       require, 
       character.only = T)

x = matrix(rpois(1e4, lambda = 10), ncol = 10) # pocetnost 1000 druhu na 10 lokalitach
colnames(x) = LETTERS[1:10]
y = matrix(0, nrow = 1e3, ncol = 10) # prazdna matice kterou budeme plnit pomery
colnames(y) = LETTERS[1:10]

#________________________________________________________
  # pomoci dvojiteho cyklu
f1 = function(x, y){ # spocitat podil druhu na dane lokalite
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      y[i,j] = x[i,j]/sum(x[i,])
    }
  }
    return(y)
}

y1 = f1(x,y)
sum(y1[1,])

x[1,]/sum(x[1,])
#________________________________________________________
# jednoduchy cyklus
f2 = function(x, y){ 
  for(i in 1:nrow(x)){
      y[i,] = x[i,]/sum(x[i,])
  }
    return(y)
}

#________________________________________________________
# redukce sum
f3 = function(x, y){ 
  rs = rowSums(x)
  for(i in 1:nrow(x)){
    y[i,] = x[i,]/rs[i]
  }
  return(y)
}
      f4 = function(x, y){ 
        rs = apply(X = x, MARGIN = 1, FUN = sum)
        for(i in 1:nrow(x)){
          y[i,] = x[i,]/rs[i]
        }
        return(y)
      }

#_________________________________________________________
# nahrazeni cyklu sumarizaci
f5 = function(x, y){ 
  y = apply(X = x, MARGIN = 1, FUN = function(x){x/sum(x)})
  return(y)
}

      f6 = function(x, y){ 
        for(i in 1:nrow(x)){
          y[i,] = x[i,]/rowSums(x)[i]
        }
        return(y)
      }
#_________________________________________________________
x2 = NULL
for(i in 1:ncol(x)){
  x2 = c(x2, x[i,])
}
g2 = rep(LETTERS[1:10], each = 1000)
xx = data.table(x2, g2)

  xx = data.table(stack(data.frame(t(x)))) # t je transponovani
f7 = function(x){
  return(x[,.(y = values/sum(values)), by = .(ind)])
}
#__________________________________________________________
  xx2 = stack(data.frame(x))
f8 = function(x){
  return(tapply(x$values, x$ind, function(x){x/sum(x)}))
}

#__________________________________________________________
# kompilace funkce (je jiz implemenotvana do R, netreba se stresovat)
f9 = cmpfun(f1)
dump("f1", file = "f1.R") # ulozi funkci do samostatneho skriptu (do pracovni slozky)
source("f1.R") # nacteni funkce

# ukladani vysledku do cache
f10 = memoise(f1)
f10(x,y)

rychlosti = microbenchmark(f1(x,y), f2(x,y), f3(x,y), f4(x,y), f5(x,y), f6(x,y), f7(xx), f8(xx2), f9(x,y),f10(x,y) , times = 100)
boxplot(rychlosti)
#__________________________________________________________
# zrychleni nacitani a ukladani
xx2 = data.frame(x)
write.table(xx2, file = "textak.txt") # jako textovy soubor
save(xx2, xx, x, file = "rd.RData") # jako RData, ulozi se environment

# nacist txt jako data frame
read.table("textak.txt", header = T)

# nacist RData
load("rd.RData")

# nacist jako data table
fread("textak.txt", header = T)

microbenchmark(read.table("textak.txt", header = T), fread("textak.txt", header = T), times = 100) # fread() mnohem rychlejsi

#__________________________________________________________
# filtr vs. podminka
xx$values
ff = function(x){
  x2 = numeric(length(x))
  for (i in 1:length(x)){
    if(x[i] == 0){
       x2[i] = 0
    }else {
      x2[i] = 1
    }
    }
  return(x2)
}

ff2 = function(x){
  x2 = numeric(length(x))
  podminka = (x==0)
  x2[!podminka]=1
  return(x2)
}

ff3 = function(x){
  x2 = ifelse(x==0,0,1)
  return(x2)
}

cetnosti = rpois(n = 1e5, lambda = 2)
sum(cetnosti ==0)
all(ff(cetnosti)==ff2(cetnosti))
microbenchmark(ff(cetnosti), 
               ff2(cetnosti), 
               ff3(cetnosti), 
               times = 100) # if a else je nejlepsi!!

#_______________________________________________________
# vypocet diferenci
(2:n)-(1:(n-1))
cetnosti = rep(cetnosti, 100)
microbenchmark(cetnosti[2:length(cetnosti)]-cetnosti[1:(length(cetnosti)-1)], diff(cetnosti))

# dmvnorm() vs. mvnfast
install.packages("mvnfast")
install.packages("emdbook")
mvnfast::dmvn(X = x, mu = 5)

# jak ziskat funkce v c
# RCpp
install.packages("RCpp")
# Rcpp::evalCpp()

# paralelni procesovani
# package future, multiprocess
# Microsoft R Open - pracuje s vice jadry


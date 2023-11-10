botky=read.table(file = "boticky.txt",
              header = T)

boty_filtr=botky[botky$velikost==c(39,40)&
        botky$cena<=2500&
        ((botky$barva=="cerna" & botky$pohodli>=9 & botky$smrdi=="NE")|botky$barva=="cervena"|botky$barva=="vinova")&
         botky$pohodli>=5&
       !(botky$smrdi== "ANO"),]
          
# a)	Průměrné ceny černých, červených a vínových bot
botky$barva = as.factor(botky$barva)
mean(botky$cena [botky$barva=="cerna"])
mean(botky$cena [botky$barva=="cervena"])
mean(botky$cena [botky$barva=="vinova"])



# b)	Počet červených bot v různých kategoriích zápachu
botky$smrdi = as.factor(botky$smrdi)
summary(botky$smrdi [botky$barva=="cervena" ])


# c)	Minimální cenu vínových bot s nejvyšším pohodlím a bez zápachu.
min(botky$cena [botky$barva == "vinova" &
                botky$pohodli == max(botky$pohodli [botky$barva == "vinova" & botky$smrdi == "NE"]) &
                botky$smrdi == "NE"]) 


# alternativni cast reseni pro a)
mean(botky$cena [botky$barva=="cerna" | botky$barva=="cervena" | botky$barva=="vinova"])

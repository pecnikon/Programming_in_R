set.seed(5)
# populace - "okrova", "plava", "slonovinova", od kazde 100 jedincu
# pohlavi - "M" nebo "F", nahodne, ale 55% samcu a 45% samic
# vaha - z normalniho rozdeleni, sd = 2, mean = 10g pro okrove, 12g pro plave a 13.5 pro slonovinove
# pocet blech - z poissonova rozdeleni s lambda = 10

populace = rep(x = c("okrova", "plava", "slonovinova"), times = 100)
pohlavi = sample(x = c("M", "F"), size = length(populace), prob = c(0.55, 0.45), replace = T)
vaha = rnorm(n = length(populace), mean = c(10, 12, 13.5), sd = 2)
blechy = rpois(n = length(populace), lambda = 10)
mysi = data.frame(populace, pohlavi, vaha, blechy)

head(mysi, 3) # prvni 3
tail(mysi, 3) # posledni 3
#___________________________________________________________________________
# x-y graf
plot(formula = mysi$blechy~mysi$vaha) # ta prvni je vzdy na ose y, druha na ose x
plot(x = mysi$blechy, y = mysi$vaha) # to stejne

plot(formula = mysi$blechy~mysi$vaha, 
     xlab = "Váha myši [g]", 
     ylab = "Počet blech", 
     main = "O myších a blechách",
     xlim = c(0,25), # upravi velikost (limit) grafu
     pch = 20, # symboly bodu
     col = "darkgreen",
     bty = "n") 
?pch
#___________________________________________________________________________
# BOXPLOT - uprostred median, kolem 25 a 75 kvartil, vousy do 1.58 * ta krabicka, zbytek jsou body
boxplot(mysi$vaha~mysi$populace, notch = T) 
# notch - 95% interval spolehlivosti, ze to bude ten median, statisticka signifikance na hranici 5% kdyz notch jednoho nezahrnuje median druheho
#___________________________________________________________________________
# BARPLOT
dat = xtabs(~mysi$pohlavi+mysi$populace)
barplot(dat, 
        beside = T, 
        col = c("hotpink", "darkgreen"))

barplot(dat, 
        beside = T, 
        horiz = T,
        names = c("Okrová", "Plavá", "Slonovinová"),
        col = c("indianred", "royalblue"),
        legend = T)
#___________________________________________________________________________
# HISTOGRAM
?hist
hist(mysi$vaha) # rozdeli rozdeleni na intervaly
hist(mysi$vaha, 
     breaks = 20, # na kolik sloupcu se to rozlame
     freq = F) # misto frequency je na y hustota
#___________________________________________________________________________
# KOLACOVY GRAF
dat = xtabs(~mysi$pohlavi)
pie(dat)

#___________________________________________________________________________
#___________________________________________________________________________
# ARGUMENT TYPE
x = seq(from = 4, to = 16, by = 0.5)
y = dnorm(x = x, mean = 10, sd = 2)

?plot
plot(y ~ x, type = "p") # p = points
plot(y ~ x, type = "l") # l = line
plot(y ~ x, type = "b") # b = both
plot(y ~ x, type = "o") # o = overplotted
plot(y ~ x, type = "s") # s = stairs
plot(y ~ x, type = "n") # n = no plotting, kdyz chci pak pridat krivku ktera data protina a puvodni data tam nechci

#___________________________________________________________________________
dev.new()
?par

dev.off() # zavru graficke okno

#___________________________________________________________________________
# PARAMETR CEX
plot(y ~ x, 
     type = "p",
     bty = "7") # bty - znaky ukazuji ohraniceni
par(cex=1) # musim spustit zaroven s plotem, ovlivni nam velikost popisku, pro 2 musi byt vetsi okno

par(cex = 1,
    cex.axis = 2, # velikost os
    col.axis = "yellow", # barva os
    col = "red", # barva ohraniceni
    #bg = "green",
    fg = "orange")
plot(y ~ x, 
     type = "p",
     bty = "7",
     cex = 2, # meni velikost uvnitr grafu
     col = "blue") 

par(font.main = 4,
    las = 3) # jak jsou popisky orientovany, 3 - horizontalne 
plot(formula = mysi$blechy~mysi$vaha, 
     xlab = "Váha myši [g]", 
     ylab = "Počet blech", 
     main = "O myších a blechách",
     xlim = c(0,25), # upravi velikost (limit) grafu
     pch = 20, # symboly bodu
     col = "darkgreen",
     bty = "n",
     font = 3) 

par(font.main = 4,
    las = 3,
    lwd = 2,
    lty = 4)
plot(y~x, type = "l")

dev.off()
boxplot(mysi$vaha~mysi$populace,
        notch = T,
        medlwd = 0.5,
        whisklty = 1,
        col = c("coral1","goldenrod", moje_barva)) # fousky
?bxt # napoveda k argumentum boxplotu

par(mar = c(0.7, 1, 0, 1), # margins ponechavaji rozmery i kdyz zmensime okno
    omi = c(1, 1, 1, 1)) # velikost okraju 

moje_barva = rgb(red = 4,
                 green = 159,
                 blue = 81,
                 alpha = 100, # dela barvu pruhlednou
                 maxColorValue = 255)

col2rgb("deeppink3", alpha = T) # ukaze i vcetne pruhlednosti, kdyz chci zachovat barvu

pruhlednost = function(barva, pruhlednost = 100){
  rgb_barva = col2rgb(barva)
  moje_barva = rgb(red = rgb_barva [1],
                   green = rgb_barva [2],
                   blue = rgb_barva [3],
                   alpha = pruhlednost,
                   maxColorValue = 255)
  return(moje_barva)
}

pruhlednost("lightblue", pruhlednost = 100)


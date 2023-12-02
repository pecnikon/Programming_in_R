populace = rep(x = c("okrova", "plava", "slonovinova"), times = 100)
pohlavi = sample(x = c("M", "F"), size = length(populace), prob = c(0.55, 0.45), replace = T)
vaha = rnorm(n = length(populace), mean = c(10, 12, 13.5), sd = 2)
blechy = rpois(n = length(populace), lambda = 10)
mysi = data.frame(populace, pohlavi, vaha, blechy)

mysi$pohlavi = as.factor(mysi$pohlavi)
mysi$populace = as.factor(mysi$populace)

# srafy
dat = xtabs(~mysi$pohlavi + mysi$populace)
barplot(dat, 
        beside = T,
        density = 20,
        angle = c(45, 325),
        col = "grey20") 
# kdyz pridam argument add, prida se format do uz existujiciho grafu

plot(formula = mysi$blechy~mysi$vaha,
     bty = "n",
     yaxt = "n", # vymaze osu y
     ylab = "",
     xlim = c(4,20),
     ylim = c(2, 23),
     type = "n")

points(x = mysi$vaha, 
       y = mysi$blechy,
       col = c("coral", "royalblue4") [mysi$pohlavi],
       pch = c(15,16,17) [mysi$populace],
       cex = 0.5)
mtext(text = "Pocet blech",
      side = 2, # podle hodinovych rucicek nastavime kde se nachazi text
      at = 10, # taky posouvani po ose
      adj = 0.5, # posouvani po ose
      line = 2,# defaultne 0
      cex = 0.8) # velikost textu

text(x = 17,
     y = 20,
     cex = 0.5,
     labels = "Z grafu je patrne, \n ze blechy mysi nevazi")

axis(side = 2,
     at = seq(from = 4, to = 20, by = 4),
     labels = as.roman(seq(from = 4, to = 20, by = 4)), # as.roman - na rimske cislice
     pos = 5, # v jake hodnote osy x chceme aby ji osa y protinala
     tcl = 1, # zaporne - pacicky jdou ven, kladne - pacicky jdou dovnitr
     las = 1) # popisky jsou vdorovne

# pridani linii
# AB line - je docela k nicemu
abline(a = 10,
       b = 0.5,
       lwd = 2, # line width
       lty = 4) # line type
  
abline(h = 10, # h = horizontal
       lwd = 2, # line width
       lty = 4) # line type
       
abline(v = 12, # v = vertical
       lwd = 2, # line width
       lty = 4) # line type

lines(x = c(min(mysi$vaha), max(mysi$vaha)),
      y = c(mean(mysi$blechy), mean(mysi$blechy)),
      col = "purple",
      lwd = 5)

# obdelnik na x 10:15 a na y 8:12
polygon(x = c(10, 10, 15, 15), 
        y = c(12, 8, 8, 12), 
        col = "mediumaquamarine", 
        border = "midnightblue",
        lwd = 3)
dev.new()
plot(formula = mysi$blechy~mysi$vaha)

# locator ukaze souradnice bodu, kam kliknu do grafu
locator(n = 5,# kolikrat budu klikat do grafu
        pch = 16,
        type = "p") # uvidime body tam kde jsme klikli

# _____________________________________________________________________

plot(formula = mysi$blechy~mysi$vaha,
     main = "O mysich a blechach",
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     xlim = c(4,20),
     ylim = c(2, 23),
     type = "n")

points(x = mysi$vaha, 
       y = mysi$blechy,
       col = c("darkred", "darkblue") [mysi$pohlavi],
       pch = c(15,16,17) [mysi$populace],
       cex = 1.5)

mtext(text = "Pocet blech",
      side = 2,
      line = 2,
      cex = 1) 
mtext(text = "Vaha mysi [g]",
      side = 1, 
      line = 2,
      cex = 1, 
      adj = 0.5)

text(x = 17,
     y = 19,
     cex = 1,
     labels = "okrova \n plava \n slonovinova")

axis(side = 2,
     at = seq(from = 4, to = 20, by = 4),
     pos = 4, 
     tcl = -1, 
     las = 1)

axis(side = 1,
     at = seq(from = 4, to = 20, by = 4),
     pos = 1.5,
     tlc = -1,
     las = 1)

points(x = c(18.5, 18.5, 18.5, 19.5, 19.5 ,19.5),
       y = c(20.2, 18.8, 17.5, 20.2, 18.8, 17.5),
       pch = c(15,16,17,15,16,17),
       col = c("darkred", "darkred", "darkred", 
               "darkblue", "darkblue", "darkblue"),
       cex = 1.5)

polygon(x = c(15.5, 20, 20, 15.5),
        y = c(17, 17, 22.6, 22.6))
text(x = 19,
     y = 21.5,
     cex = 1,
     labels = "M       F")

#______________________________________________________________________

plot(x = 5, 
     y = 400,
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     xlab = "",
     ylab = "",
     xlim = c(0, 4),
     ylim = c(0, 400))

axis(side = 1,
     at = seq(from = 0.5, to = 3.5, by = 0.5),
     pos = 0)     
     
axis(side = 2,
     at = seq(from = 0, to = 400, by = 100),
     pos = 0)     

polygon(x = c(1, 0.75, 0.9, 0.65, 0.9, 0.5, 
              1.5, 1.1, 1.35, 1.1, 1.25),
        y = c(130,  100,  100,  75,  75,  
              50,  50,  75,  75,  100,  100),
        col = "green4",
        border = "black",
        lwd = 1.5)

polygon(x = c(0.95, 1.05, 1.05, 0.95),
        y = c(0,0,50,50),
        col = "tan4",
        lwd = 1.5)

#_________________________________________

polygon(x = 1+c(1, 0.75, 0.9, 0.65, 0.9, 0.5, 
              1.5, 1.1, 1.35, 1.1, 1.25),
        y = 2*c(130,  100,  100,  75,  75,  
              50,  50,  75,  75,  100,  100),
        col = "green4",
        border = "black",
        lwd = 1.5)

polygon(x = 1+c(0.95, 1.05, 1.05, 0.95),
        y = 2*c(0,0,50,50),
        col = "tan4",
        lwd = 1.5)

polygon(x = 2+c(1, 0.75, 0.9, 0.65, 0.9, 0.5, 
              1.5, 1.1, 1.35, 1.1, 1.25),
        y = 3*c(130,  100,  100,  75,  75,  
              50,  50,  75,  75,  100,  100),
        col = "green4",
        border = "black",
        lwd = 1.5)

polygon(x = 2+c(0.95, 1.05, 1.05, 0.95),
        y = 3*c(0,0,50,50),
        col = "tan4",
        lwd = 1.5)
     
     
     
     
     
     
     
     
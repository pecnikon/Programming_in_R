populace = rep(x = c("okrova", "plava", "slonovinova"), times = 100)
pohlavi = sample(x = c("M", "F"), size = length(populace), 
                 prob = c(0.55, 0.45), replace = T)
vaha = rnorm(n = length(populace), mean = c(10, 12, 13.5), sd = 2)
blechy = rpois(n = length(populace), lambda = 10)
mysi = data.frame(populace, pohlavi, vaha, blechy)

mysi$pohlavi = as.factor(mysi$pohlavi)
mysi$populace = as.factor(mysi$populace)
#__________________________________________________
# spojovani grafu do jednoho panelu

par(mfrow = c(2,2))
plot(mysi$blechy~mysi$pohlavi)
barplot(xtabs(~mysi$populace+mysi$pohlavi))
hist(mysi$vaha)
boxplot(mysi$vaha~mysi$populace)
dev.off()
par(mfrow = c(1,1))
#__________________________________________________
layout(mat = matrix(data = c(1, 1, 1, 2, 3, 4), 
                    nrow = 3, 
                    byrow = F),
       widths = c(1.5, 1),
       heights = c(1, 1, 2))

plot(mysi$blechy~mysi$vaha)
barplot(xtabs(~mysi$populace+mysi$pohlavi))
hist(mysi$vaha)
boxplot(mysi$vaha~mysi$populace)

#__________________________________________________
layout(mat = matrix(data = c(1,1,1,1,1,3,2,1,1),
                    nrow = 3,
                    byrow = F))

plot(mysi$blechy~mysi$vaha)
barplot(xtabs(~mysi$populace+mysi$pohlavi))
hist(mysi$vaha)
boxplot(mysi$vaha~mysi$populace)
#__________________________________________________
layout(mat = matrix(data = c(3,3,3,3,3,3,3,3,
                             3,3,3,3,3,3,3,3,
                             2,2,2,2,2,3,3,3,
                             2,2,2,2,2,3,3,3,
                             2,2,2,2,4,4,3,3,
                             1,1,1,2,4,4,3,3,
                             1,1,1,2,2,3,3,3,
                             1,1,1,2,2,3,3,3),
                    nrow = 8,
                    byrow = T),
       widths = c(1,1,0.5,0.5,1),
       heights = c(1, 1, 0.5, 0.5, 1))

# EXPORT
# bud Export --> save as image
# nebo pomoci funkce
pdf(file = "vsechny grafy.pdf")

plot(mysi$blechy~mysi$vaha)
barplot(xtabs(~mysi$populace+mysi$pohlavi))
hist(mysi$vaha)
boxplot(mysi$vaha~mysi$populace)
dev.off()

tiff() # dobry pro komprimace
tiff(filename = "graf_mysi.tiff", 
     width = 140, height = 120, 
     units = "mm", # defaultne jsou tam pixely
     res = 1200,
     compression = "lzw")

plot(mysi$blechy~mysi$vaha)
dev.off()
#________________________________________________________
#________________________________________________________
# SAMOSTATNA PRACE
par(omi = c(.2,.2,.2,.2))# jsou okolo celeho panelu
layout(mat = matrix(data = c(1,2,3,4),
                    nrow = 2,
                    byrow = T))
par(mai = c(0,0.5,0,0))
plot(mysi$blechy~mysi$vaha,
     pch = 15,
     col = c("darkgoldenrod2","khaki1", "wheat"), bty = "n", 
     xaxt = "n", ylim = c(0, 20))
mtext(text = "Pocet blech v kozichu",
      side = 2,
      adj = 0.6,
      cex = 0.7)

par(bty = "n",
    mai = c(0,0,0,0))
boxplot(mysi$blechy~mysi$populace, notch = T, 
        col = c("darkgoldenrod2","khaki1", "wheat"), xlab = "", ylab = "", 
        xaxt = "n", yaxt = "n")

par(bty = "n",
    mai = c(0.5,0.5,0,0))
hist(mysi$vaha, main = "",
     col = "white", breaks = 13)
mtext(text = "Pocet jedincu/Predikovana hustota x 100",
      side = 2,
      adj = 0.6,
      cex = 0.7)

par(bty = "n",
    mai = c(0.5,0,0,0))
barplot(xtabs(~mysi$pohlavi+mysi$populace), beside = T, 
        col = c("darkgoldenrod2","darkgoldenrod2","khaki1","khaki1","wheat","wheat"), bty = "n", yaxt = "n")
barplot(xtabs(~mysi$pohlavi+mysi$populace), 
        beside = T,
        density = 20,
        angle = 45,
        col = c("black","darkgoldenrod2","black","khaki1","black","wheat"),
        add = T,
        yaxt = "n")







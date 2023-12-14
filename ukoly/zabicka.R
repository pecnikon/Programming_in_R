populace = rep(x = c("okrova", "plava", "slonovinova"), times = 100)
pohlavi = sample(x = c("M", "F"), size = length(populace), 
                 prob = c(0.55, 0.45), replace = T)
vaha = rnorm(n = length(populace), mean = c(10, 12, 13.5), sd = 2)
blechy = rpois(n = length(populace), lambda = 10)
mysi = data.frame(populace, pohlavi, vaha, blechy)

mysi$pohlavi = as.factor(mysi$pohlavi)
mysi$populace = as.factor(mysi$populace)

{
plot(formula = mysi$blechy~mysi$vaha,
     main = "Zabicka",
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     xlim = c(3,21),
     ylim = c(-2, 23),
     type = "n")

polygon(x = c( 16.56380, 17.63368, 19.07037, 18.48958, 17.17516), 
        y = c( 4.864316, 3.805186,  9.936989, 11.442068, 10.494426),
        col = "khaki", border = "khaki4")

polygon(x = c( 6.465161, 7.280307, 10.160488, 14.875193, 17.686999, 16.464281, 14.589446, 12.442896, 10.649575), 
        y = c( 15.445272, 18.207928, 19.331720, 14.927831,  9.264415, 3.504980, 2.053415, 3.224032, 10.060434),
        col = "darkolivegreen", border = "darkolivegreen")

polygon(x = c(10.175093,9.013510, 8.132719,  8.860670,  9.961117), 
        y = c(19.30192, 19.66916 ,18.58874, 16.96069, 17.51813),
        col = "darkolivegreen", border = "darkolivegreen")

polygon(x = c(9.5, 8.7, 8.6,  9,  9.6, 9.8), 
        y = c(19.5, 19.2 ,17.7, 17.4, 17.6, 19),
        col = "lightgoldenrod4", border = "lightgoldenrod4")

polygon(x = c(8.646695, 9.196918, 9.716573, 9.135782), 
        y = c(18.46577, 18.74449, 18.29854, 18.13131),
        col = "gray15")

polygon(x = c( 6.465161, 11.360488, 17.686999, 16.464281, 14.589446, 12.442896, 10.649575), 
        y = c( 15.445272, 15.045272, 9.264415, 3.504980, 2.053415, 3.224032, 10.060434),
        col = "khaki", border = "khaki4")


polygon(x = c( 12.442896, 11.029977, 11.817951, 11.328863, 10.350689,  7.796566,  7.008592,  7.524851,  9.312825,  9.312825, 11.627750), 
        y = c( 3.2708564, 3.9732265, -0.1005203, -0.9901891, -0.7560657,  3.4581551,  8.9366422,  9.7326617,  7.9533240,  6.6422330,  6.4549343),
        col = "khaki", border = "khaki4")
segments(9.312825,6.6422330,9.012825,4.6422330, col = "khaki4")

polygon(x = c(0, 0 ,15,23, 23, 15), 
        y = c(7, 9, 8,9, 7, 6),
        col = "darkseagreen4", border = "darkseagreen4")

polygon(x = c(17.89481, 18.55639, 18.62583, 17.35856, 16.65550), 
        y = c(9.212321, 8.540012, 6.479189, 5.143034, 6.484268),
        col = "darkolivegreen", border = "darkolivegreen")

polygon(x = c( 10.649575, 10.389068, 10.603044, 10.113957,  9.019190,  9.286005, 10.05282), 
        y = c(10.060434, 9.012321, 7.484268, 6.180882, 6.871088, 9.323809, 10.82889),
        col = "darkolivegreen", border = "darkolivegreen")

polygon(x = c( 13.90438, 16.13585, 16.45550, 16.28868, 15.21881, 15.73846, 14.16858, 14.24063, 15.18824, 15.18256, 14.54586), 
        y = c(12.668428 , 13.002890, 10.327195 , 8.187654 , 7.930217,5.690676,  6.536626  ,8.320423  ,9.156578 ,10.884631, 10.940375),
        col = "darkolivegreen", border = "darkolivegreen")

segments(9.012825,4.6422330,10.312825,1.6422330, col = "khaki4")
}
locator(n = 4,# kolikrat budu klikat do grafu
        pch = 16,
        type = "p") # uvidime body tam kde jsme klikli
dev.off()

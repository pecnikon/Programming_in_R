# DATAFRAME
{
jedinec = rep(x = 1:20, each = 20)
jezci = data.frame(ID = jedinec, cas = character(400), datum = character(400), datumocas = character(400), 
                   pohlavi = character(400) , vek = character(400), ODBA = character(400), denni_doba = character(400), teplota = character(400))

for (i in 1:400) {
cas_hod = sample(x = 0:23, size = 400, replace = T)
cas_min = sample(x = 0:59, size = 400, replace = T)
jezci$cas[i] = paste(cas_hod[i], cas_min[i], sep = ":")
}

datum1 = rep(x = c("1.6.2021","2.6.2021"), each = 10)
jezci$datum = rep(datum1, times = 20)

pohlavi_nahodne = sample(x = c("samec", "samice"), size = 20, replace = T)
jezci$pohlavi = rep(x = pohlavi_nahodne, each = 20)

vek_nahoda = sample(x = 1:5, size = 20, replace = T)
jezci$vek = rep(x = vek_nahoda, each = 20)

jezci$ODBA = abs(rnorm(n = 400, mean = 0, sd = 0.7))

jezci$datumocas = as.POSIXct(paste(jezci$datum, jezci$cas, sep = " "), 
                       format = "%d.%m.%Y %H:%M")

cas_posix = as.POSIXct(jezci$cas, format = "%H:%M")
den.noc = function(cas){
  vychod = as.POSIXct("4:56", format = "%H:%M")
  zapad = as.POSIXct("21:05", format = "%H:%M")
  denni_doba = ifelse(test = (cas_posix<vychod|cas_posix > zapad), "noc", "den")
  return(denni_doba)
}
jezci$denni_doba = den.noc(jezci$cas)

}

for(i in 1:400){
if(jezci$denni_doba[i] == "noc"){
  jezci$teplota[i] = rnorm(1, mean = 15, sd = 2)
} else {
  jezci$teplota[i] = rnorm(1, mean = 24, sd = 2)
}
}

tapply(X = jezci$ODBA, INDEX = jezci$ID, FUN = mean)
tapply(X = jezci$ODBA, INDEX = jezci$pohlavi, FUN = mean)

dev.new()
par(mfrow = c(4,5))
for(i in 1:20){
  boxplot(jezci$ODBA[(i - 1) * 20 + 1:(i * 20)]~jezci$denni_doba[(i - 1) * 20 + 1:(i * 20)],
          main=c("ID:", i), ylab = "", xlab = "", col = c("#E5DE44", "#355C7D"), frame = F)
}
tiff(filename = "graf_jezci2.tiff", 
     width = 4096, height = 2304,
     res = 300,
     units = "px",
     compression = "lzw")
dev.off()
# poradi ukladani grafu: dev.new(), tiff, par, plot, dev.off()

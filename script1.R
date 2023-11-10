getwd() #cesta k pracovni slozce
setwd("C:/Users/xpecn006/Desktop/r_cv1") #zmena pracovni slozky na plochu (wd = working directory), 
#opacna orientace lomitek

(3+(10**(1/2)))*(5/3)

3*((((25-10)*((3*2)**3))**(1/3))/(32/(15**(1/2))))

#objekt
x=5
x=c(1,2,3.5) #numeric
class(x)
typeof(x)
mode(x)

x_integer=c(1L,2L,3L)#integer
class(x_integer)
typeof(x_integer)
x_character=c("jedna","dva","3") #text vzdy pisu do uvozovek
x_logical=c(TRUE, FALSE,T,F) #TRUE, FALSE vzdy pisu s velkym pismenem
x_factor=as.factor(x_integer) #znaci hladiny, kategorie, neda se s tim pocitat

x_NA=NA #not available
x_inf=c(Inf,-Inf,5/0) #nekonecno
x_null=NULL
x_null={}

?matrix
matice=matrix(c(1,4, 2,5, 3,6), nrow=3, ncol=2, byrow = TRUE)
matice=matrix(data=1:6, nrow = 3) #stejne jako to nahore

pole=array(data=1:27,dim = c(3,3,3)) #jako rubikova kostka

datovy_ramec=data.frame(cislo=1:5,
                        nazev=c("jedna", "dva", "tri", "ctyri", "pet"))

seznam=list(dat_ramec=datovy_ramec,
            matice=matice,
            pole=pole,
            x_factor=x_factor)

matice[1,] #vypise prvni radek
matice[,2] #vypise druhy sloupec
matice[2,1] #druha hodnota v prvnim sloupci
datovy_ramec[2:5,] #druhy az paty sloupec

pole#vypiste druhy radek ve treti vrstve
pole[2,,3]

seznam$dat_ramec$nazev

str(seznam)
seznam[[2]][3,2] #poradi subobjektu, pak [radek,sloupec,vrstva]
seznam[[1]][4,2]

seznam$dat_ramec$nazev[4]

?seq

seq(0, 100, by = 0.1)

seq(0, 100, length.out = 105)

?rep
#rada cisel 1:5 ktere budou:
#kazde zopakovane 10x

rep(1:5, each=10)

#10x zopakovana rada 1:5

rep.int(1:5, times = 10)

#kazde zopakovane 10x a to cele 10x

rep.int(rep(1:5, each=10), 10)

#kazde zopakovane 10x a to cele 10x, do delky 450 hodnot

rep_len(rep.int(rep(1:5, each=10), times = 10), 450)

?sample #vygenerovat nahodny vyber 20 hodnot z vektoru 1:5, kde jednotlive hodnoty budou zastoupeny s relativni
#cetnosti 1:1/2:1/3:1/4:1/5

sample(1:5,20, replace = T, prob = c(1,1/2,1/3,1/4,1/5))

#export dat
write.table(x=datovy_ramec,
            file="data.txt",
            sep=";",
            dec = ",")
#import dat
df=read.table(file = "data.txt",
              header = T,
              sep = ";",
              dec = ",")
df$factor=as.factor(df$factor)

save(seznam,df,matice,
     file = "data.RData")

rm(matice)
rm(list = ls()) #vymaze vse

load(file = "data.RData") #nahraje

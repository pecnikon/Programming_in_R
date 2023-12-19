bobci=data.frame(Číslo_epizody= seq(1,30),
                 Kamarád=sample(c("Kryštůfek", "Jolanka", "Zpytihněvíček", "Glórijka", "Barnabášek"), size=30, replace = T),
                 Sledovanost=rpois(30, 133407),
                 velikost_klobouku=round(rnorm(30, 30, 3), digits = 2),
                 velikost_bobka = character(30),
                 velikost_boba = character(30))

for(i in 1:30){
bobci$velikost_bobka[i] = round(sample(x = (bobci$velikost_klobouku[i]/2):bobci$velikost_klobouku[i], size = 1), digits = 2)
}

for (i in 1:30) {
bobci$velikost_boba[i] = round(as.numeric(bobci$velikost_klobouku[i]) - as.numeric(bobci$velikost_bobka[i]), digits = 2)
}

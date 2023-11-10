bobci=data.frame(Číslo_epizody= seq(1,30),
                 Kamarád=sample(c("Kryštůfek", "Jolanka", "Zpytihněvíček", "Glórijka", "Barnabášek"), size=30, replace = T),
                 Sledovanost=rpois(30, 133407),
                 velikost_klobouku=rnorm(30, 30, 3))



bobci$velikost_bobka <- with(bobci, runif(n = nrow(bobci), min = bobci$velikost_klobouku / 2, max = bobci$velikost_klobouku))


# hra s pocitacem
who_wins = function(x, y) {
  if (x == "kamen" & y == "nuzky" |
      x == "nuzky" & y == "papir" |
      x == "papir" & y == "kamen") {
    return(0)
  } 
  if (x == "nuzky" & y == "kamen" |
        x == "kamen" & y == "papir" |
        x == "papir" & y == "nuzky") {
    return(1)
  } 
  if (x == y) {
    return(2)
  }
}
  
repeat{
  pocitac_vyber = sample(x = c("kamen", "nuzky", "papir"), size = 1)
  hrac_vyber = readline(prompt = "Co vybiras?")
  winner = who_wins(pocitac_vyber, hrac_vyber)
  if(winner == 0){
    message("Pocitac vyhral")
    break
  } else if(winner == 1){
    message("Pocitac prohral")
    break
  } else if(winner == 2){
    message( "Remiza - dalsi kolo")
    next
  }
}

#___________________________________________________________________________

# dva hraci
repeat{
  hrac_1 = readline(prompt = "Vybira hrac 1:")
  cat(paste("\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n",
      "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "hrac 2"))
  hrac_2 = readline(prompt = "Vybira hrac 2:")
  winner = who_wins(hrac_1, hrac_2)
  if(winner == 0){
    message("Hrac 1 je vitez")
    break
  } else if(winner == 1){
    message("Hrac 2 je vitez")
    break
  } else if(winner == 2){
    message( "Remiza - dalsi kolo")
    next
  }
}

#___________________________________________________________________________

# vice hracu
pocet_hracu = function(size) {
  poradi = seq(from = 1, to = size)
  return(poradi)
}

hra = function() {
  uvod = readline(prompt = "Pocet hracu:")
  poradi = pocet_hracu(size = uvod)
  repeat {
    for (i in 1:uvod) {
      cat(paste("Nyni hraje hrac", poradi[i], " a hrac", poradi[i + 1]))
      hrac_1 = readline(prompt = "Vybira hrac 1:")
      cat(
        paste("\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n",
              "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "hrac 2")
      )
      hrac_2 = readline(prompt = "Vybira hrac 2:")
      winner = who_wins(hrac_1, hrac_2)
      if (winner == 0) {
        message("Hrac 1 je vitez")
        break
      } else if (winner == 1) {
        message("Hrac 2 je vitez")
        break
      } else if (winner == 2) {
        message("Remiza - dalsi kolo")
        next
      }
    }
    
  }
}

hra()

poradi = function(size){
  seq(from = 1, to = length(size), by = 1)
}
  
seq(from = 1, to = length(size), by = 1)


pocet_hracu = function(size){
poradi = seq(from = 1, to = size)
return(poradi)
}

poradi = pocet_hracu(size = 10)  

  
  
# hromadne nacitani dat a jejich spojeni do dataframu
x = list.files(pattern = "THM1") # nalezne soubory obsahujici...
xx = read.table(x [1], header = T, sep = ";")


  # 1) vytvorit seznam souboru s danym patternem
  # 2) vsechny je nacist
  # 3) pridat sloupec s ID souboru
  # 4) spojit je do jednoho
  # 5) vratit vysledny data frame

read.all = function(pattern, ...){
  x = list.files(pattern = pattern) # 1)
  output =NULL # prazdny objekt
  
  for (file in 1:length(x)) {
    xx = read.table(x[file],...) # 2) # odsud to bere ty tri tecky v argumentu
    xx$ID_file = substr(x = x[file], start = 6, stop = nchar(x[file])-4) # 3)
    output = rbind(output, xx) # 4)
  }
  return(output) # 5)
}

data = read.all("THM1", header = T, sep = ";")

# ___________________________________________________________________________

# funkce pro hromadny prevod mezi datovymi typy
substitute(sample) # z nazvu objektu udela textvy retezec
get("sample") # vezme textovy retezec a interpretuje ho jako nazev objektu

prevod = function(x, puvodni, novy) {
  puvodni = paste0("is.", as.character(substitute(puvodni)))
  novy = paste0("as.", as.character(substitute(novy)))
  for (i in 1:ncol(x)) {
  if(puvodni == "is.factor" & novy == "as.numeric"){
    if (get(puvodni)(x[, i]) == T){
    x[,i]=get(novy)(as.character(x[,i]))
    }
  } else{
    if (get(puvodni)(x[, i]) == T) {
      x[, i] = get(novy)(x[, i])
    }
  }
  }
  return(x)
}

xx = prevod(x = data, puvodni = numeric, novy = factor)
as.numeric(as.character(xx$TA))


















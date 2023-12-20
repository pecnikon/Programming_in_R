df=read.table(file = "jehlicky.txt",
              header = T)
str(df$hostina)

df[, 2:3] = apply(X = df[,2:3], MARGIN = 2, FUN = function(x){x*60})

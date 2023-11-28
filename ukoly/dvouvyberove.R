# 1)
mereni1 = rnorm(n = 30, mean = 7, sd = 2)
mereni2 = rnorm(n = 30, mean = 7.4, sd = 1.5)
# test normality - H0: je normalni, H1: neni normalni
shapiro.test(mereni1)
shapiro.test(mereni2)

# H0: jsou stejne
# H1: nejsou stejne
var.test(x = mereni1, y = mereni2)
# p = 0.12 > 0.05 -> H0 nezamitam

# 2) systematicky jine vysledky??


# 3)
# H0: jsou stejne
# H1: jsou jine
vytycky = rnorm(n = 10, mean = 3, sd = 0.5)
shapiro.test(vytycky) # 0.72 > 0.05 rozdleni je normalni
t.test(x = vytycky, mu = 4) # 4e-05 < 0.05 H0 se zamita

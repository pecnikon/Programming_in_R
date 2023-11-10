
pocet_polozek = sample(x = 1:15, size = 100, replace = T)
pocet_minut = sum(pocet_polozek)
pocet_minut_celk = pocet_minut + (100/4 - 1)*10.5 + 24/3*9.5
pocet_hodin_celk = pocet_minut_celk/60
celk_cas = pocet_hodin_celk/7.5

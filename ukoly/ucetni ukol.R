simulations = 100
size_t = 100

results = numeric(simulations)

for (i in 1:100){
  pocet_polozek = sample(x = 1:15, size = 100, replace = T)
  pocet_minut = sum(pocet_polozek)
  pocet_minut_celk = pocet_minut + (100/4 - 1) * 10.5 + (24/3) * 9.5
  pocet_hodin_celk = pocet_minut_celk/60
  celk_cas = pocet_hodin_celk/7.5
  results[i] = celk_cas
}
hist(results, main = "Simulation Results", xlab = "Total Time (days)", col = "lightblue")


# tahle cast nevim co je ale myslim ze to je z chatgpt
for (i in 1:simulations){
  time = sum(sample(x = 1:15, size = 100, replace = T))
  time_all_hours = (time + (size_t/4 - 1) * 10.5 + (24/3) * 9.5)/60
  time_all_days = time_all_hours/7.5
  results[i] = time_all_days
}
summary(results)
hist(results, main = "Simulation Results", xlab = "Total Time (days)", col = "lightblue")

#beat
for(j in 1:100){
polozek=sample(x = 1:15, size = 100, replace = T)
cas=NULL
for(i in 0:length(polozek)){
  cas[i]=polozek[i]*1
  if(i%%4==0){
    cas[i]=cas[i]+10.5
  }
  if(i%%12==0){
    cas[i]=cas[i]+20
  }
  cas_hod[j] = sum(cas)/60
  cas_prum = mean(cas_hod[j])
}
}

(7.5*5)-sum(cas)/60

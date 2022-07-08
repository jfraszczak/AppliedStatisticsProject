prepareDataset<-function(dataset){

  dataset_nice_names <- dataset #I create a copy so I can modify it
  
  colnames(dataset_nice_names) <- c('NUTS','Air_passengers','Hospital_beds','Deaths',
                                    'Compensation_of_employees','Deaths2','Percentage_early_leavers_from_education',
                                    'Hours_worked','Farm_labour_force','Health_personnel','Discharges_after_respiratory_disease',
                                    'Life_expectancy','Longterm_beds','GDP','Percentage_of_people_studying_or_training',
                                    'Population_density','Population','Students','GVA','Vehicles','Students_in_tertiary_education',
                                    'Unemployement_rate','Utilised_agricoltural_area','Percentage_of_NEET','First_wave_density',
                                    'Second_wave_density','Country','Longitude','Latitude')
  
  dataset_nice_names <- dataset_nice_names[c('NUTS','Country','Population','Population_density','Students_in_tertiary_education','Percentage_early_leavers_from_education',
                                             'Percentage_of_NEET','Percentage_of_people_studying_or_training','Students','Life_expectancy','Deaths',
                                             'Deaths2','Discharges_after_respiratory_disease','Longterm_beds','Hospital_beds',
                                             'Health_personnel','Air_passengers','Vehicles','Farm_labour_force','Utilised_agricoltural_area',
                                             'Unemployement_rate','Compensation_of_employees','Hours_worked',
                                             'GDP','GVA','First_wave_density','Second_wave_density','Latitude','Longitude')]
  
  # I changed the name of the columns and I've ordered them, grouping together columns referring to the same category. 
  # I inverted Longitude and Latitude
  
  
  #I remove countries that are far away
  dataset_nice_names <- dataset_nice_names[-c(66:70,113,114),]
  
  
  # it seems no sense to me to keep both Deaths2 and Deaths. I will now check the difference between them,
  # scaling them myself using the Population
  difference <- (dataset_nice_names$Deaths2)*100000/(dataset_nice_names$Population) - dataset_nice_names$Deaths
  difference_percentage <- difference*100/dataset_nice_names$Deaths2 #not high. I remove death, but first I fill the gaps in dataset_nice_names$Deaths2
  dataset_nice_names[120,11] <- dataset_nice_names$Deaths2[120]*100000/dataset_nice_names$Population[120]
  dataset_nice_names[122,11] <- dataset_nice_names$Deaths2[122]*100000/dataset_nice_names$Population[122]
  dataset_nice_names[123,11] <- dataset_nice_names$Deaths2[123]*100000/dataset_nice_names$Population[123]
  dataset_nice_names[124,11] <- dataset_nice_names$Deaths2[124]*100000/dataset_nice_names$Population[124]
  dataset_nice_names[127,11] <- dataset_nice_names$Deaths2[127]*100000/dataset_nice_names$Population[127]
  dataset_nice_names[128,11] <- dataset_nice_names$Deaths2[128]*100000/dataset_nice_names$Population[128]
  dataset_nice_names[129,11] <- dataset_nice_names$Deaths2[129]*100000/dataset_nice_names$Population[129]
  dataset_nice_names <- dataset_nice_names[,-c(12)]
  
  #German avalaible beds
  ge_beds <- 657662
  ge_gdp <- sum(dataset_nice_names$GDP[20:35])
  ge_nuts_beds <- vector(mode="integer", length=16)
  ge_nuts_beds_scaled_on_100k <- vector(mode="integer", length=16)
  for (i in 1:16) {
    ge_nuts_beds[i] <- ge_beds * dataset_nice_names$GDP[19+i] /ge_gdp
    ge_nuts_beds_scaled_on_100k[i] <-  ge_nuts_beds[i]*100000/dataset_nice_names$Population[19+i] 
    dataset_nice_names[19+i,14] <-  ge_nuts_beds_scaled_on_100k[i] }
  
  #Netherlands avalaible beds
  ne_beds <- 53394
  ne_gdp <- sum(dataset_nice_names$GDP[91:102])
  ne_nuts_beds <- vector(mode="integer", length=12)
  ne_nuts_beds_scaled_on_100k <- vector(mode="integer", length=12)
  for (j in 1:12) {
    ne_nuts_beds[j] <- ne_beds * dataset_nice_names$GDP[90+j] /ne_gdp
    ne_nuts_beds_scaled_on_100k[j] <-  ne_nuts_beds[j]*100000/dataset_nice_names$Population[90+j] 
    dataset_nice_names[90+j,14] <-  ne_nuts_beds_scaled_on_100k[j] }
  
  #German pupils/Students
  ge_Students <- 3296249
  ge_Population <-sum(dataset_nice_names$Population[20:35])
  ge_nuts_Students <- vector(mode="integer", length=16)
  for (i in 1:16) {
    ge_nuts_Students[i] <- ge_Students * dataset_nice_names$Population[19+i] /ge_Population
    dataset_nice_names[19+i,9] <- ge_nuts_Students[i] }
  
  #Portugal Vehicles
  pt_Vehicles <- 5170611
  pt_Population <- sum(dataset_nice_names$Population[103:107])
  pt_nuts_Vehicles <- vector(mode="integer", length=5)
  for (i in 1:5) {
    pt_nuts_Vehicles[i] <- pt_Vehicles * dataset_nice_names$Population[102+i] /pt_Population
    dataset_nice_names[102+i,17] <- pt_nuts_Vehicles[i] }
  
  #Estonia Vehicles
  ee_Vehicles <- 1045587
  dataset_nice_names[41,17] <- ee_Vehicles
  
  #Germany Students in tertiary education
  ge_Students_tert <- 3296249
  ge_Population <-sum(dataset_nice_names$Population[20:35])
  ge_nuts_Students_tert <- vector(mode="integer", length=16)
  for (i in 1:16) {
    ge_nuts_Students_tert[i] <- ge_Students_tert * dataset_nice_names$Population[19+i] /ge_Population
    dataset_nice_names[19+i,5] <- ge_nuts_Students_tert[i] }
  
  #Belgium farm labour force
  dataset_nice_names[10,18] <- 10
  
  #Italy farm labour force
  dataset_nice_names[68,18] <- 2180
  dataset_nice_names[79,18] <- 14860
  
  #Italy agricultural area
  dataset_nice_names[79,19] <- 227900
  dataset_nice_names[68,19] <- 52490
  
  #Denmark has no value for covid density second case. We remove Denmark from dataset_nice_names
  dataset_nice_names <- dataset_nice_names [-c(36:40),]
  
  #Finland has FI13 and FI1A that have always the same value. I remove the first one
  dataset_nice_names <- dataset_nice_names [-127,]
  
  #Health personnel. Several data were found and now we add them in the dataset_nice_names.
  dataset_nice_names[20,15] <- 46043
  dataset_nice_names[21,15] <- 59716
  dataset_nice_names[22,15] <- 20168
  dataset_nice_names[23,15] <- 9463
  dataset_nice_names[24,15] <- 3802
  dataset_nice_names[25,15] <- 11808
  dataset_nice_names[26,15] <- 26773
  dataset_nice_names[27,15] <- 7079
  dataset_nice_names[28,15] <- 30743
  dataset_nice_names[29,15] <- 80962
  dataset_nice_names[30,15] <- 16445
  dataset_nice_names[31,15] <- 4805
  dataset_nice_names[32,15] <- 16841
  dataset_nice_names[33,15] <- 9007
  dataset_nice_names[34,15] <- 12504
  dataset_nice_names[35,15] <- 8941
  
  dataset_nice_names[56,15] <- 4629
  dataset_nice_names[57,15] <- 7031
  dataset_nice_names[58,15] <- 63
  dataset_nice_names[127,15] <- 10955
  dataset_nice_names[128,15] <- 4757
  #ES64 (Melilla) has no data for health personnel, but it has very similar Population as ES63(Ceuta) and it is
  #also very close geographically, so I use the same value
  dataset_nice_names[54,15]  <- dataset_nice_names[53,15]
  
  
  #Hospital Discharges: we were not able to find any data online.
  # we fill gaps using global average for the data we have scaled on the Population of that NUTS
  tot_Population <- 0
  tot_discharges <- 0
  for (i in 1:128) {
    if (is.na(dataset_nice_names$Discharges_after_respiratory_disease[i]) == FALSE) {
      tot_Population <- tot_Population + dataset_nice_names$Population[i]                           #I compute the total Population of the NUTS we have data on discharges
      tot_discharges <- tot_discharges + dataset_nice_names$Discharges_after_respiratory_disease[i] #I compute the total discharges of the NUTS we have data on discharges
    }
  }
  discharges_per_capita <- tot_discharges / tot_Population
  
  dataset_nice_names[20,12] <- discharges_per_capita *dataset_nice_names$Population[20]
  dataset_nice_names[21,12] <- discharges_per_capita *dataset_nice_names$Population[21]
  dataset_nice_names[26,12] <- discharges_per_capita *dataset_nice_names$Population[26]
  dataset_nice_names[28,12] <- discharges_per_capita *dataset_nice_names$Population[28]
  dataset_nice_names[29,12] <- discharges_per_capita *dataset_nice_names$Population[29]
  dataset_nice_names[30,12] <- discharges_per_capita *dataset_nice_names$Population[30]
  dataset_nice_names[32,12] <- discharges_per_capita *dataset_nice_names$Population[32]
  dataset_nice_names[56,12] <- discharges_per_capita *dataset_nice_names$Population[56]
  dataset_nice_names[57,12] <- discharges_per_capita *dataset_nice_names$Population[57]
  dataset_nice_names[58,12] <- discharges_per_capita *dataset_nice_names$Population[58]
  dataset_nice_names[127,12] <- discharges_per_capita *dataset_nice_names$Population[127]
  dataset_nice_names[128,12] <- discharges_per_capita *dataset_nice_names$Population[128]
  dataset_nice_names[83,12] <- discharges_per_capita *dataset_nice_names$Population[83]
  dataset_nice_names[85,12] <- discharges_per_capita *dataset_nice_names$Population[85]
  
  
  #I delete long_term_care_beds. Infact 35% of the data are missing and I already have avalaible_beds
  dataset_nice_names <- dataset_nice_names[,-13]

  
  dataset_nice_names

}

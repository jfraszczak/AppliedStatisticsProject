install.packages("plyr")
install.packages("tidyverse")

library(plyr)
library(tidyverse)

loadDataset<-function(dataset_name){
  dataset = read.csv(dataset_name, header=T)
  dataset
}

scale_by_population_dataset<-function(dataset){
  features_to_be_scaled = c(
    "Students_in_tertiary_education",
    "Students",
    "Deaths",
    "Discharges_after_respiratory_disease",
    "Health_personnel",
    "Vehicles",
    "Farm_labour_force",
    "Compensation_of_employees",
    "Hours_worked",
    "GDP"
  )
  
  dataset <- cbind(dataset[ , -which(names(dataset) %in% features_to_be_scaled)], dataset[,features_to_be_scaled] / dataset[,"Population"] * 10e4)
  dataset <- dataset[ , -which(names(dataset) == "Population")]
  dataset
}

selectQuantitativeFeatures<-function(dataset){
  cols_numeric <- unlist(lapply(dataset, is.numeric))
  dataset_numeric <- dataset[ , cols_numeric]
  dataset_numeric
}

getColumnsWithMissingData<-function(dataset){
  is.na(dataset)
  colSums(is.na(dataset))
  which(colSums(is.na(dataset))>0)
  names <- names(which(colSums(is.na(dataset))>0))
  names
}

fillMissingData<-function(dataset){
  dataset_complete<-dataset
  dataset_quantitative <- selectQuantitativeFeatures(dataset)
  features<-getColumnsWithMissingData(dataset_quantitative)
  
  countries<-unique(dataset$country)
  for(country in countries){
    for(feature in features){
      values<-dataset[dataset$Country == country, feature]
      if(all(is.na(values))){
        dataset_complete[dataset_complete$Country == country, feature] <- mean(dataset_complete[,feature], na.rm = TRUE) 
      }
    }
  }
  
  dataset_complete <- dataset_complete %>% 
    group_by(Country) %>% 
    mutate_if(is.numeric, 
              function(x) ifelse(is.na(x), 
                                 median(x, na.rm = TRUE), 
                                 x))
  
  dataset_complete
}

selectSignificatFeatures<-function(dataset, include_country){
  features <- c(
    'First_wave_density', 'Second_wave_density',
    'Life_expectancy', 'Population_density', 'GVA',
    'Percentage_of_people_studying_or_training', 'Percentage_of_NEET', 'Percentage_early_leavers_from_education',
    'Hospital_beds', 'Farm_labour_force', 'Utilised_agricoltural_area',
    'Hours_worked', 'Unemployement_rate'
    )
  
  if(include_country){
    features <- append(features, 'Country')
  }
  
  dataset<-dataset[, features]
  dataset
}




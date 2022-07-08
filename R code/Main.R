initialize<-function(directory_path, directory_plots){
  rm(list = ls())
  setwd(directory_path)
  unlink(directory_plots, recursive = TRUE)
  dir.create(directory_plots, showWarnings = FALSE)
  source("DatasetPreprocessing.R")
  source("PlotsGeneration.R")
  source("PrepareDataset.R")
}

#Initialization
directory_path<-'~/University Courses/Polimi/Semester 2/Applied Statistics/Project/R code'
directory_plots <- "Plots"
initialize(directory_path, directory_plots)

#Data preprocessing
dataset_name <- 'dataset.csv'
dataset<-loadDataset(dataset_name)
dataset <- prepareDataset(dataset)
dataset<-scale_by_population_dataset(dataset)
dataset_complete <- fillMissingData(dataset)
dataset_quantitative <- selectQuantitativeFeatures(dataset_complete)
write.csv(dataset,"DatasetPreprocessed.csv", row.names = FALSE)

#Plot correlation matrix and PCA
plotCorrelationMatrix(dataset_complete, directory_plots, "Correlation matrix.png")
performPCA(dataset_complete, directory_plots, "PCA.png", "PCA - Countries.png")

#Select potentially most significant features
dataset_quantitative <- selectSignificatFeatures(dataset_quantitative, FALSE)
dataset_complete <- selectSignificatFeatures(dataset_complete, TRUE)
dataset <- selectSignificatFeatures(dataset, TRUE)

#Generate plots on reduced dataset
plotAllFeautresAgainstCases(dataset, directory_plots, "FeautresAgainstCases")
plotAllBivariateFeautresAgainstCases(dataset, directory_plots, "BivariateFeautresAgainstCases")
plotBoxplotsGroupedByCountries(dataset, directory_plots, "Boxplots")
performPCA(dataset_complete, directory_plots, "PCA - After features selection.png", "PCA - Countries - After features selection.png")

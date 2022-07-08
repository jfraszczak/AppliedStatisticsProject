#Initialization
directory_path<-'~/University Courses/Polimi/Semester 2/Applied Statistics/Project/R code'
setwd(directory_path)

source("DatasetPreprocessing.R")
source("PlotsGeneration.R")
source("PrepareDataset.R")
library(car)
library(caret)
install.packages('glmnet')
require(glmnet)
install.packages('tree')
require(tree)
install.packages("pls")
library(pls)
install.packages("lightgbm")
library(lightgbm)


regression_tree <- function(x, y){
  tr = tree(y ~., data=x)
  tr = prune.tree(tr, best = 3)
  summary(tr)
  plot(tr)
  text(tr)
}

linear_regression <- function(x, y, x_with_response){
  fm <- lm(y ~.,data = x)
  print(summary(fm))
  crPlots(fm)
  
  par(mfcol=c(2,1))
  res <- resid(fm)
  plot(fitted(fm), res)
  abline(0,0)
  qqnorm(res)
  qqline(res)
  print(shapiro.test(residuals(fm)))
  
  ctrl <- trainControl(method = "LOOCV")
  model <- train(Cases_density ~.,data = x_with_response, method = "lm", trControl = ctrl)
  print(model)
}

lasso_parameter_search <- function(x_with_response){
  Grid_la_reg = expand.grid(alpha = seq(1, 2, by = 1),
                            lambda = seq(0.000001, 0.1, by = 0.001))
  
  ctrl <- trainControl(method = "LOOCV")
  model <- train(Cases_density ~.,data = x_with_response, method = "glmnet", trControl = ctrl, tuneGrid = Grid_la_reg)
  print(model)
  plot(model, main = "Lasso Regression - Parameter Search")
}

lasso_regression <- function(x, y, lambda){
  lasso.fit = glmnet(x, y, alpha=1, lambda = lambda)
  coef(lasso.fit)
}

pca_regression <- function(x_with_response){
  model <- pcr(Cases_density ~.,data = x_with_response, scale=TRUE, validation="LOO")
  print(coef(model))
  
  ctrl <- trainControl(method = "LOOCV")
  model <- train(Cases_density ~.,data = x_with_response, method = "pcr", trControl = ctrl)
  print(model)
}

random_forest <- function(x_with_response, wave){
  ctrl <- trainControl(method = "LOOCV")
  model <- train(Cases_density ~.,data = x_with_response, method = "rf", trControl = ctrl)
  print(model)
  
}


#-----------------------------------------------------------------------------------------
#Data preprocessing
dataset_name <- 'dataset.csv'
dataset<-loadDataset(dataset_name)
dataset <- prepareDataset(dataset)
dataset<-scale_by_population_dataset(dataset)
dataset_complete <- fillMissingData(dataset)
dataset_quantitative <- selectQuantitativeFeatures(dataset_complete)

first_wave_density <- dataset_quantitative$First_wave_density * 100
second_wave_density <- dataset_quantitative$Second_wave_density * 100
dataset_quantitative <- dataset_quantitative[ , -which(names(dataset_quantitative) %in% c("Latitude", "Longitude", "First_wave_density", "Second_wave_density"))]

#Adding nonlinear features

dataset_nonlinear_features_wave1 <- dataset_quantitative
dataset_nonlinear_features_wave1['Vehicles_squared'] <- dataset_quantitative['Vehicles'] ^ 2
dataset_nonlinear_features_wave1['Unemployement_rate_squared'] <- dataset_quantitative['Unemployement_rate'] ^ 2
dataset_nonlinear_features_wave1['GVA_squared'] <- dataset_quantitative['GVA'] ^ 2

dataset_nonlinear_features_wave2 <- dataset_quantitative
dataset_nonlinear_features_wave2['Utilised_agricoltural_area_squared'] <- dataset_quantitative['Utilised_agricoltural_area'] ^ 2
dataset_nonlinear_features_wave2['Utilised_agricoltural_area_cubic'] <- dataset_quantitative['Utilised_agricoltural_area'] ^ 3
dataset_nonlinear_features_wave2['GVA_squared'] <- dataset_quantitative['GVA'] ^ 2
dataset_nonlinear_features_wave2['Air_passengers_squared'] <- dataset_quantitative['Air_passengers'] ^ 2
dataset_nonlinear_features_wave2['Air_passengers_cubic'] <- dataset_quantitative['Air_passengers'] ^ 3


#Final datasets
dataset <- as.data.frame(scale(dataset_quantitative))
dataset_with_country <- dataset
dataset_with_country['Country'] <- dataset_complete['Country']

dataset_nonlinear_features_wave1 <- as.data.frame(scale(dataset_nonlinear_features_wave1))
dataset_nonlinear_features_wave2 <- as.data.frame(scale(dataset_nonlinear_features_wave2))

dataset_with_response_wave1 <- dataset
dataset_with_response_wave1['Cases_density'] <- first_wave_density
dataset_with_response_and_country_wave1 <- dataset_with_response_wave1
dataset_with_response_and_country_wave1['Country'] <- dataset_complete['Country']

dataset_with_response_wave2 <- dataset
dataset_with_response_wave2['Cases_density'] <- second_wave_density
dataset_with_response_and_country_wave2 <- dataset_with_response_wave2
dataset_with_response_and_country_wave2['Country'] <- dataset_complete['Country']

dataset_nonlinear_features_with_response_wave1 <- dataset_nonlinear_features_wave1
dataset_nonlinear_features_with_response_wave1['Cases_density'] <- first_wave_density

dataset_nonlinear_features_with_response_wave2 <- dataset_nonlinear_features_wave2
dataset_nonlinear_features_with_response_wave2['Cases_density'] <- second_wave_density


#-----------------------------------------------------------------------------------------
#Regression tree - discover interactions between features

#First wave
regression_tree(dataset, first_wave_density)

#Second wave
regression_tree(dataset, second_wave_density)
#-----------------------------------------------------------------------------------------


#Histograms of densities of cases
hist(first_wave_density)
hist(second_wave_density)

#-----------------------------------------------------------------------------------------
#Linear regression

#First wave
linear_regression(dataset, first_wave_density, dataset_with_response_wave1)

#Second wave
linear_regression(dataset, second_wave_density, dataset_with_response_wave2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#Linear regression - nonlinear features

#First wave
linear_regression(dataset_nonlinear_features_wave1, first_wave_density, dataset_nonlinear_features_with_response_wave1)

#Second wave
linear_regression(dataset_nonlinear_features_wave2, second_wave_density, dataset_nonlinear_features_with_response_wave2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#Lasso regression

#First wave
lasso_parameter_search(dataset_nonlinear_features_with_response_wave1)
lasso_regression(dataset_nonlinear_features_wave1, first_wave_density, 0.0)

#Second wave
lasso_parameter_search(dataset_nonlinear_features_with_response_wave2)
lasso_regression(dataset_nonlinear_features_wave1, first_wave_density, 0.0)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#PCA Regression

#First wave
pca_regression(dataset_nonlinear_features_with_response_wave1)

#Second wave
pca_regression(dataset_nonlinear_features_with_response_wave2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#Random forest

#First wave
random_forest(dataset_with_response_wave1)

#Second wave
random_forest(dataset_with_response_wave2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#Random forest with countries

#First wave
random_forest(dataset_with_response_and_country_wave1)

#Second wave
random_forest(dataset_with_response_and_country_wave2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#GRADIENT BOOSTING TREE - LGBM

lgb.train1 = lgb.Dataset(data=as.matrix(dataset), label=first_wave_density)

params1 <- list(
  objective = "regression"
  , metric = "mae"
  , learning_rate = 0.01
  , num_leaves = 5
  , min_data_in_leaf = 5
  , feature_fraction = 0.7
  , bagging_fraction = 0.7
)

model1 <- lgb.cv(
  params = params1
  , data = lgb.train1
  , nrounds = 1000L
  , nfold = 10L
)

model1


lgb.train2 = lgb.Dataset(data=as.matrix(dataset), label=second_wave_density)

params2 <- list(
  objective = "regression"
  , metric = "mae"
  , learning_rate = 0.01
  , num_leaves = 5
  , feature_fraction = 0.7
  , bagging_fraction = 0.7
)

model2 <- lgb.cv(
  params = params2
  , data = lgb.train2
  , nrounds = 1000L
  , nfold = 10L
)

model2




#GRADIENT BOOSTING TREE - LGBM with countries

lgb.train1 = lgb.Dataset(data=as.matrix(dataset_with_country), label=first_wave_density)

params1 <- list(
  objective = "regression"
  , metric = "mae"
  , learning_rate = 0.01
  , num_leaves = 5
  , min_data_in_leaf = 5
  , feature_fraction = 0.7
  , bagging_fraction = 0.7
)

model1 <- lgb.cv(
  params = params1
  , data = lgb.train1
  , nrounds = 1000L
  , nfold = 10L
)

model1


lgb.train2 = lgb.Dataset(data=as.matrix(dataset_with_country), label=second_wave_density)

params2 <- list(
  objective = "regression"
  , metric = "mae"
  , learning_rate = 0.01
  , num_leaves = 5
  , feature_fraction = 0.7
  , bagging_fraction = 0.7
)

model2 <- lgb.cv(
  params = params2
  , data = lgb.train2
  , nrounds = 1000L
  , nfold = 10L
)

model2



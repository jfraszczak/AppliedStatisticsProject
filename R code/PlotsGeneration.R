install.packages("ggplot2")
install.packages("colorspace")
install.packages("gridExtra")
install.packages("knitr")
install.packages("corrplot")

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

library(corrplot)
library(ggplot2)
library(colorspace)
library(gridExtra)
library(knitr)


plotSingleFeautreAgainstCases<-function(x, y, group, feature, wave){
  y_label <- 'Cases density '
  title <- 'Dependency between feature and density of cases - '
  if(wave == 1){
    title <- paste(title, "1st wave")
    y_label <- paste(y_label, "1st wave")
  }
  if(wave == 2){
    title <- paste(title, "2nd wave")
    y_label <- paste(y_label, "2nd wave")
  }
  
  colors = c("red", "blue", "green", "orange", "black", "gray",
             "cyan", "yellow", "blueviolet", "brown", "darkgoldenrod1", "darkgreen",
             "deeppink", "olivedrab1", "palegreen", "royalblue1", "rosybrown1", "tan2", "steelblue4", "violetred", "red4")
  
  plot(x, y,
       pch = 19,
       col = colors,
       xlab=feature, 
       ylab=y_label,
       main=title)
  
  # Legend
  legend("topright",
         legend = levels(factor(group)),
         pch = 19,
         col = colors)
}

plotAllFeautresAgainstCases<-function(dataset, directory_plots, subdirectory){
  features<-names(dataset_quantitative)
  group <- dataset[,"Country"]
  y1 <- dataset[,'First_wave_density']
  y2 <- dataset[,'Second_wave_density']
  
  subdirectory_path = file.path(directory_plots, subdirectory)
  dir.create(subdirectory_path, showWarnings = FALSE)
  
  for(feature in features) {
    x <- dataset[, feature]
    
    title <- paste(feature, " - First Wave.png", sep="")
    title <- str_replace_all(title, "%", "percent")
    file_path= file.path(subdirectory_path, title)
    png(file=file_path, type = "cairo")
    plotSingleFeautreAgainstCases(x, y1, group, feature, 1)
    dev.off()
    
    title <- paste(feature, " - Second Wave.png", sep="")
    title <- str_replace_all(title, "%", "percent")
    file_path= file.path(subdirectory_path, title)
    png(file=file_path, type = "cairo")
    plotSingleFeautreAgainstCases(x, y2, group, feature, 2)
    dev.off()
    
  }
}

plotBivariateFeautresAgainstCases<-function(x1, x2, y, feature1, feature2, wave){
  title <- "Bivariate dependency between features and density of cases - "
  if(wave == 1){
    title <- paste(title, "1st wave")
  }
  if(wave == 2){
    title <- paste(title, "2nd wave")
  }
  
  vec = y / max(y)
  myColors = c("black", "red")
  myRangeFunction = colorRamp(myColors)
  myColors = myRangeFunction(vec)
  myColors <- rgb(myColors[, 1], myColors[, 2], myColors[, 3], maxColorValue=255)
  
  plot(x = x1,
       y = x2,
       cex = 2,
       pch = 19,
       col = myColors,
       xlab=feature1, 
       ylab=feature2,
       main=title)
  
}

plotAllBivariateFeautresAgainstCases<-function(dataset, directory_plots, subdirectory){
  features<-names(dataset_quantitative)
  features<-features[features != 'First_wave_density']
  features<-features[features != 'Second_wave_density']
  group <- dataset[,"Country"]
  y1 <- dataset[,'First_wave_density']
  y2 <- dataset[,'Second_wave_density']
  
  subdirectory_path = file.path(directory_plots, subdirectory)
  dir.create(subdirectory_path, showWarnings = FALSE)
  
  for(i in 1:(length(features) - 1)) {
    for(j in (i + 1):length(features)){
      feature1 <- features[i]
      feature2 <- features[j]
      
      x1 <- dataset[, feature1]
      x2 <- dataset[, feature2]
      
      title <- paste(feature1, feature2, sep=" and ")
      title <- paste(title, " - First Wave.png", sep="")
      title <- str_replace_all(title, "%", "percent")
      file_path= file.path(subdirectory_path, title)
      png(file=file_path, type = "cairo")
      plotBivariateFeautresAgainstCases(x1, x2, y1, feature1, feature2, 1)
      dev.off()
      
      title <- paste(feature1, feature2, sep=" and ")
      title <- paste(title, " - Second Wave.png", sep="")
      title <- str_replace_all(title, "%", "percent")
      file_path= file.path(subdirectory_path, title)
      png(file=file_path, type = "cairo")
      plotBivariateFeautresAgainstCases(x1, x2, y2, feature1, feature2, 2)
      dev.off()

      
    }
  }
}

plotBoxplotsGroupedByCountries<-function(dataset, directory_plots, subdirectory){
  dataset_quantitative <- selectQuantitativeFeatures(dataset)
  features<-names(dataset_quantitative)
  group <- dataset[,"Country"]
  
  subdirectory_path = file.path(directory_plots, subdirectory)
  dir.create(subdirectory_path, showWarnings = FALSE)
  
  for(feature in features) {
    title <- paste(feature, ".png", sep="")
    title <- str_replace_all(title, "%", "percent")
    file_path= file.path(subdirectory_path, title)
    print(file_path)
    png(file=file_path, type = "cairo")
    
    boxplot(dataset[,feature] ~ group,
            xlab='Country', 
            ylab=feature,
            main="Comparison of data distribution between countries")
    
    dev.off()
  }
}

plotCorrelationMatrix<-function(dataset, directory_plots, title){
  nums <- unlist(lapply(dataset_complete, is.numeric))
  ds <- dataset_complete[ , nums]
  
  file_path= file.path(directory_plots, title)
  #png(file=file_path, type = "cairo")
  corrplot(cor(ds))
  #dev.off()
}

performPCA<-function(dataset, directory_plots, title1, title2){
  dataset_quantitative <- selectQuantitativeFeatures(dataset)
  dataset_quantitative.pca <- prcomp(dataset_quantitative, center = TRUE, scale. = TRUE)
  
  file_path = file.path(directory_plots, title1)
  ggbiplot(dataset_quantitative.pca)
  ggsave(file_path)
  
  file_path = file.path(directory_plots, title2)
  ggbiplot(dataset_quantitative.pca,ellipse=TRUE, groups=dataset$Country)
  ggsave(file_path)
}
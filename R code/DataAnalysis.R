#-------------------------------------------------------------------------------
#Univariate Analysis: histogram, box plot, we can also consider to normalize data(applying a certain power, or log)
#Bivariate Analysis: plots to find correlation between variables



#histograms of choose features


#function to plot histograms and pairs() of targeted features
TargetedPLot <- function(columns) {
  
  #Univariate plot: boxplot + histogram
  x11(); par( mfrow= (c( 1,length(columns) ) +1) )
  
  boxplot( dataset.scaled[columns ], las=2 )
  
  #par( mfrow= c( 1,length(columns) ) +1)#layout(matrix(c(2,3,1,3),2,byrow=T))
  for( i in  columns){
    hist( dataset.scaled[[i]],  main= colnames(dataset[i]) )
    box()
  }
  
  
  #To plot the variance
  #barplot(sapply(dataset[columns],sd)^2, las=2, main='Original Variables', ylab='Variances')
  
  # ??x11()
  # ??qqplot(qnorm((1:1000/1000-0.5/1000)), z, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
  # ??abline(0, 1, col='blue')
  
  
  
  #---------------------------------------------------------------------------------------
  #Bivariate Analysis: pairs()
  x11()
  pairs( cbind(dataset.scaled[columns], dataset[c("cases/population_first_wave","cases/population_second_wave") ] ) )
  
  #S <- cov(dataset[indexes$education]) covariance of a part of the dataset

}

#to choose the columns that we want to analyse

columns=colnames(dataset[indexes$education])
TargetedPLot(columns)

columns=colnames(dataset[indexes$population])
TargetedPLot(columns)

columns=colnames(dataset[indexes$healthcare])
TargetedPLot(columns)

columns=colnames(dataset[indexes$mobility])
TargetedPLot(columns)

columns=colnames(dataset[indexes$primary_sector])
TargetedPLot(columns)

columns=colnames(dataset[indexes$economy_category])
TargetedPLot(columns)
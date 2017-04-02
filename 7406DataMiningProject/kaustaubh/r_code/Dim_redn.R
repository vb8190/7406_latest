#Kaustubh's Dimension Reduction Script

# MCA in R reference: 
# http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining

#setwd("C:/Users/Kaustubh/Documents/DMSL/Project")

#install.packages("FactoMineR")
install.packages("factoextra")
require(FactoMineR)
require(factoextra)

dfDimFact <- read.table("becca_features.csv", sep = ",", header = TRUE)

dim(dfDimFact)
names(dfDimFact)

sapply(dfDimFact,class)

for (i in 1: length(dfDimFact))
{
  dfDimFact[,i] <- as.factor(dfDimFact[,i])
}



mcaTest <- MCA(dfDimFact[,-c(1,2)], graph = FALSE)

summary(mcaTest, nb.dec = 2, ncp = 2, nbelements = 20)

plot(mcaTest, choix = "var")



# variable categories - i.e. factor levels within each variable
var <- get_mca_var(mcaTest)

var$contrib


fviz_mca_var(mcaTest)

#install.packages("corrplot")
library("corrplot")
  corrplot(var$cos2, is.corr = FALSE)
  

## Prettier charts    
# source: http://gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/ 
#install.packages("ggplot2")
require(ggplot2)
  
  # data frame with variable coordinates
  cats = apply(dfDimFact[,-c(1,2)], 2, function(x) nlevels(as.factor(x)))
  mcaTest_vars_df = data.frame(mcaTest$var$coord, Variable = rep(names(cats), cats))
  
  # data frame with observation coordinates
  mcaTest_obs_df = data.frame(mcaTest$ind$coord)
  
  # plot of variable categories
  ggplot(data=mcaTest_vars_df, 
         aes(x = Dim.1, y = Dim.2, label = rownames(mcaTest_vars_df))) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_text(aes(colour=Variable)) +
    ggtitle("MCA plot of variables using R package FactoMineR")
  
  ggplot(data = mcaTest_obs_df, aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_point(colour = "gray50", alpha = 0.7) +
    geom_density2d(colour = "gray80") +
    geom_text(data = mcaTest_vars_df, 
              aes(x = Dim.1, y = Dim.2, 
                  label = rownames(mcaTest_vars_df), colour = Variable)) +
    ggtitle("MCA plot of variables using R package FactoMineR") +
    scale_colour_discrete(name = "Variable")


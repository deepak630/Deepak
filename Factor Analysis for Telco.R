# Clear environment
rm(list = ls())
-----------------------------------------------------------------------------------------
  # SET THE WORKING DIRECTORY BY BROWSING DIRECTORY
  dir<-choose.dir()
  setwd(dir)
  getwd()
  
  #IMPORTING FILE BY BROWISING FILE
  file<-choose.files()                             ### Browse the file
  telco0 <- read.csv("D:/Analytics/R/BA class 3+4/Factor & Cluster Analysis/Factor/telco_csv.csv", header = TRUE)   ### Import file
  str(telco, list.len = 150)               ### STRUCTURE OF THE DATA for 150 columns
  names(telco)  
  ### COLUMN NAMES OF DATA SET
  View(telco)
#------------------------------------------------------------------------------------

  inc<-c("tenure",
         "age",
         "marital",
         "income",
         "retire",
         "gender",
         "reside",
         "tollfree",
         "equip",
         "callcard",
         "wireless",
         "longmon",
         "tollmon",
         "equipmon",
         "cardmon",
         "wiremon",
         "multline",
         "voice",
         "pager",
         "internet",
         "callid",
         "callwait",
         "forward",
         "confer",
         "ebill")
  
telco <- telco[inc]

## FACTOR ANALYSIS 
corrm<- cor(telco)                                 ### CORRELATION MATRIX


require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

eigen(corrm)$values                                                    ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

write.csv(eigen_values, "D:/Analytics/R/BA class 3+4/Factor & Cluster Analysis/Factor/EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=T) ### SCREE PLOT

plot(eigen_values$pct_var,type='b')

FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS

print(FA)                                                    ### PRINT THE RESULTS

FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS

ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(telco),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings, "D:/Analytics/R/BA class 3+4/Factor & Cluster Analysis/Factor/loadings.csv") ### SAVING THE FILE

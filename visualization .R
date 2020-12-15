#question 1 
# Importing all the data sets 
getwd()
setwd("C:\\New folder\\R case study 2 (Credit card)")

library(dplyr)
require(dplyr)

cust_Acq <- read.csv("Customer Acqusition.csv")
Repayment <- read.csv("Repayment.csv")
spend <- read.csv("spend.csv")

View(cust_Acq)
View(Repayment)
View(spend)

 
spend <- rename(spend,"spend_date"= "Month")



# USING THE MERGE FUNCTION  
tp <- merge(x = trans,y = prod,by.x = "prod_subcat_code",by.y = "prod_cat_code",all.y = T)
View(tp)

#using the dplyr code 

x<- inner_join(cust_Acq,spend,"Customer"="Customer")
View(x)

credit_card <- inner_join(cust_Acq ,Repayment,by="Customer")

View(credit_card)

#Incase age is less than 18, replace it with mean age values.

Q11$age_group <- with(Q11,case_when(age =< 18)replace(0,18))

age>= 16 & age <= 18 ~ "Age Group 16 18"
age > 12 &  age <= 1 ~ "Age Group 12- 14"
age >08 &   age_group 10 "Age Group 08_10"
TRUE ~ as.character(age)))





Q11<- with(Q11,tapply(vector, index, function))



require(dplyr)
library(dplyr)
# stacked  bar chart

counts <- table("Region","central,East,West")

barplot(counts,main = " sales", 
   xlab = "Region", col=c("Orange","green"),
   
   legend = rownames(counts))

# pie charts sales each reigon in 2016

library(plogr)            
require(dplyr)
library(dplyr)

slice <- d(10,12,4,16,8)

lbls <- d("central", "East","West")

pie(slice, labels= lbls,explode=0.1,
      main= "pie Chart of countries")










library(arules)
library (dplyr)

#read the file 

datasetPath<- read.csv(readline("Enter your file : "))

#k-means

print("please Enter number between 2 & 4 ")
numberOfClusters <- as.numeric( readline("Enter Your number of Cluster: "))
if(numberOfClusters>=2 & numberOfClusters<= 4){
  group<-group_by(datasetPath,age)
  group<-summarise(group,sumoftotal1=sum(total))
  DF <- data.frame(group)
  k_mean<-kmeans(x =DF, centers = numberOfClusters)
  print(k_mean)
  smalltable<-cbind(group,k_mean$cluster)
  print(smalltable)
  
  DF_2 <- data.frame(datasetPath$total, datasetPath$age)
  k_mean2<-kmeans(x = DF_2, centers = numberOfClusters)
  print(k_mean2)
  the_columns<- select(datasetPath,customer,age,total)
  the_main_table<- cbind(the_columns,k_mean2$cluster)
  print(the_main_table)
} else{
  print("wrong number")
}

#association rules 

items<- read.transactions(readline("Enter your file : "), sep="," )
minSupport<-as.numeric(readline("Enter the support: "))
minConfidince<-as.numeric(readline("Enter the confidince: "))
if(minSupport>=0.001 & minSupport<=1 & minConfidince>=0.001 & minConfidince<=1 ){ 
  apriori_rules <- apriori(items  ,parameter = list(supp = minSupport,conf = minConfidince,minlen=2))
  inspect(apriori_rules)
} else {
  print("wrong number") 
}

#Data visualization 

par(mfrow = c(3,3)) 


#1

payment_type=table(datasetPath$paymentType)
pie(payment_type,
    main = "Cash and Credit total ")

#2

group2<-group_by(datasetPath,age)
group2<-summarise(group2,sumoftotal1=sum(total))

pie(
  x =group2$sumoftotal1,
  labels = group2$age,
  main = "Age and Sum of Total spending")


barplot(height = group2$sumoftotal1,
        main = "Age and Sum of Total spending",
        name = group2$age,
        xlab = "Total spending",
        ylab = "Age",
        col = "navyblue",
        xlim = range(0,2000000),
        horiz =TRUE,
        las=1)


plot(
  x = group2$age,
  y = group2$sumoftotal1,
  main = "Age and Sum of Total spending",
  xlab = "Age",
  ylab = "Total spending",
)

#3

total_spending <-arrange(datasetPath,city,desc(total))
total_spending
group3<-group_by(datasetPath,city)
group3<-summarise(group3,sumoftotal=sum(total))

pie(
  x =group3$sumoftotal,
  labels = group3$city,
  main = "Total spending of each city")

barplot(
  height = group3$sumoftotal,
  name = group3$city,
  col = "maroon",
  main = "Total spending of each city",
  xlab = "Total spending",
  xlim = range(0,3000000),
  horiz = TRUE,
  las=1)

#4

boxplot(
  datasetPath$total,
  main = "The Distribution of Total spending",
  xlab = "Total spending",
  range(0,3000),
  col = "gray")






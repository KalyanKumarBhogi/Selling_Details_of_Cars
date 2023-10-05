#File : Module6
#Project: Introduction to Analytics
#Author : Kalyan Kumar Bhogi

rm(list=ls())

#loading file
tmp <- read.csv("Car Details.csv")
getwd()


#pie chart
rows<-nrow(tmp)
rows
x<-1
owne<-c("First Owner","Second Owner","Third Owner")
count<-c(0,0,0)
owner<-data.frame(owne,count)
owner
for(x in 1:rows)
{
    if(tmp[x,8]=="First Owner")
    {
      owner[1,2]<-owner[1,2]+1
    }
    if(tmp[x,8]=="Second Owner")
    {
      owner[2,2]<-owner[2,2]+1
    }
    if(tmp[x,8]=="Third Owner")
    {
      owner[3,2]<-owner[3,2]+1
    }
  }
labels <-paste("Percent of",unique(sort(tmp$owner)),"is",round((table(tmp$owner)/2971)*100,2))
pie(owner$count,labels, main = "Owner pie chart",
    col = rainbow(length(owner$count)))

#Bar Plot of fuel
fuel<-c("Petrol","Diesel","LPG","CNG")
count1<-c(0,0,0,0)
type<-data.frame(fuel,count1)
type
x<-1
for(x in 1:rows)
{
  if(tmp[x,5]=="Petrol")
  {
    type[1,2]<-type[1,2]+1
  }
  if(tmp[x,5]=="Diesel")
  {
    type[2,2]<-type[2,2]+1
  }
  if(tmp[x,5]=="LPG")
  {
    type[3,2]<-type[3,2]+1
  }
  if(tmp[x,5]=="CNG")
  {
    type[4,2]<-type[4,2]+1
  }
}
type
par(mai=c(5,3,2,1),mar=c(5,3,2,1), mgp=c(2,1,0))
barplot(type$count1~type$fuel, main= "Usage of different types of fuel", ylab = "Frequency",ylim = c(0,2000),xlab = "Fuel")



#bar plot of seller individual
seller<-c("Dealer","Individual","Trustmark Dealer")
count2<-c(0,0,0)
type1<-data.frame(seller,count2)
type1
x<-1
for(x in 1:rows)
{
  if(tmp[x,6]=="Dealer")
  {
    type1[1,2]<-type1[1,2]+1
  }
  if(tmp[x,6]=="Individual")
  {
    type1[2,2]<-type1[2,2]+1
  }
  if(tmp[x,6]=="Trustmark Dealer")
  {
    type1[3,2]<-type1[3,2]+1
  }
}
type1
barplot(type1$count2~type1$seller)
par(mfcol=c(1,1),mai=c(1,8,4,4),mar=c(5,8,4,2))
barplot(type1$count2~type1$seller,main="Different types of Sellers", xlab = "Seller", ylab = "Frequency", col =  c("yellow","grey","red"), ylim = c(0,3000))


#bar plot of transmission
transmission<-c("Manual","Automatic")
count3<-c(0,0)
type2<-data.frame(transmission,count3)
type2
x<-1
for(x in 1:rows)
{
  if(tmp[x,7]=="Manual")
  {
    type2[1,2]<-type2[1,2]+1
  }
  if(tmp[x,7]=="Automatic")
  {
    type2[2,2]<-type2[2,2]+1
  }
  
}
type2
barplot(type2$count3~type2$transmission, main="Different types of Transmission", ylim=c(0,3000),ylab="count", xlab="Transmission", col=terrain.colors(4))

#Selling price ranging from 1990 to 2020 
library(ggplot2)
library(dplyr)
library(hrbrthemes)

ggplot(tmp, aes(x=year, y=selling_price)) +
geom_line(color="#69b3a2", size=2) +
ggtitle("Year: Range 1990-2020") +
theme_ipsum()


rm(list = ls())
dev.off()
####







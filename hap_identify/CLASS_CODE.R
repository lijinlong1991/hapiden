

setwd("E:/队友/CC/鉴别")
getwd()
data<-read.csv("data.csv",header=T)
head(data)

dataH<-data[which(data$Type=="Haploid"),]  ##筛选单倍体数据集
dataC<-data[which(data$Type=="Crossed"),] ##筛选二倍体数据集

meanArea_H<-mean(dataH$Area)  ##单倍体平均值
sdArea_H<-sd(dataH$Area)      ##单倍体标准差
meanArea_C<-mean(dataC$Area)  ##二倍体平均值
sdArea_C<-sd(dataC$Area)      ##二倍体标准差


a_dataA<-matrix(nrow=800,ncol=7)  ##建立数据集
K<-nrow(dataH)/nrow(dataC)      ##单倍体比率设置
for (i in 1:800) {
  j<-i*0.01+10
  W_H<-pnorm(j,meanArea_H,sdArea_H)   
  W_C<-pnorm(j,meanArea_C,sdArea_C)
  W<-k*W_H+(1-k)*W_C   
  Q<-((1-k)*W_C)/W    ###FDR
  B<-1-W_H            ###FNR 
  F<-(2*(1-Q)*(1-B))/(2-Q-B)  ###F值
  a_dataA[i,1]<- j
  a_dataA[i,2]<-Q 
  a_dataA[i,3]<-B
  a_dataA[i,4]<-F
  a_dataA[i,5]<-W_H
  a_dataA[i,6]<-W_C
  a_dataA[i,7]<-"dataA"
}
plot(a_dataA[,1],a_dataA[,4])

colnames(a_dataA)<-c("Area","Q","B","F","W_H","W_C","Material")
write.csv(a_dataA,"pre_dataA_Area2.csv")






##密度图
library(ggplot2)



####画图
library(easyGgplot2)
library(devtools)
ACC<-as.matrix(ACC)
y<-read.table("clipboard",header=T,sep='\t') 
write.csv(ACC,"ACC.csv")
ACC<-read.csv("ACC.csv")
p1 <- ggplot(ACC, aes(x = Area, y = HCR))+
  geom_line(aes(color = Material),size=1.5)
p1 <- p1 + labs(x = "")

p2 <- ggplot(ACC, aes(x = Area, y = FDR))+
  geom_line(aes(color = Material),size=1.5)  
p2 <- p2 + labs(x = "")


p3 <- ggplot(ACC, aes(x = Area, y =HMR))+
  geom_line(aes(color = Material),size=1.5) 

ggplot2.multiplot(p1,p2,p3, cols=1)



##p1 <- ggplot(ACC, aes(x = Area, y = HCR))+
geom_line(aes(color = Material),size=1.5)+
  labs(title="HCR~Area",size=10)+
  theme(plot.title = element_text(hjust = 0.5))  #也就加上这一行





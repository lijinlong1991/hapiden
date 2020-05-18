setwd("E:/单倍体鉴别/假设质量")
#b<-read.csv("cpmg3.csv")
b<-read.table("clipboard",header=T)

head(b)
library(dplyr)
dataA<-b[which(b$i=="A"),]
dataB<-b[which(b$i=="B"),]
dataC<-b[which(b$i=="C"),]
dataD<-b[which(b$i=="D"),]
dataD<-b[which(b$i=="E"),]
dataF<-b[which(b$i=="F"),]
dataG<-b[which(b$i=="G"),]
dataH<-b[which(b$i=="H"),]


dataA_H<-dataA[which(dataA$type=="H"),]
typeD<-dataA[which(dataA$type=="D"),]
dataA_D<-sample_n(typeD,nrow(dataA_H))
dataA_sel<-rbind(dataA_H,dataA_D)


##密度图
library(ggplot2)


ggplot(dataA_sel, aes(x = oil2, fill = type)) +
  
  # 密度曲线函数：alpha设置填充色透明度
  
  geom_density(alpha = 0.3)


###阈值检索
len<-nrow(dataA_sel)
type<-dataA_sel$type
oil<-dataA_sel$oil2
x<-vector(length=len)
a_dataA<-matrix(nrow=800,ncol=5)
for (i in 1:800) {
  k<-i*0.01
  TH=0
  FH=0
  TD=0
  FD=0
  for (j in 1:len) {
    if (oil[j] < k) { x[j]="H"  }
    else x[j]="D"
    
    if (x[j]=="H" & type[j]=="H"){
      TH=TH+1 }
    if (x[j]=="H" & type[j]=="D"){
      FH=FH+1 }
    if (x[j]=="D" & type[j]=="D"){
      TD=TD+1 }
    if (x[j]=="D" & type[j]=="H"){
      FD=FD+1 }
    
  }
  
  HCR=(TH+TD)/(TH+TD+FH+FD)###准确率
  FDR=FH/(TH+FH)###误选率
  HMR=FD/(TH+FD)###漏选率
  
  a_dataA[i,1]<- k
  a_dataA[i,2]<-HCR 
  a_dataA[i,3]<-FDR 
  a_dataA[i,4]<-HMR
  a_dataA[i,5]<-"dataA"
}
plot(a_dataA[,1],a_dataA[,2])

colnames(a_dataA)<-c("oil","HCR","FDR","HMR","Material")
write.csv(a_dataA,"pre_dataA_oil.csv")
plot(a_dataA[,3])
max(a_dataA[,2])

ACC<-rbind(a_dataA,a_dataB,a_dataC,a_dataF,a_dataG,a_dataA)


####画图
library(easyGgplot2)
library(devtools)
ACC<-as.matrix(ACC)
y<-read.table("clipboard",header=T,sep='\t') 
write.csv(ACC,"ACC.csv")
ACC<-read.csv("ACC.csv")
p1 <- ggplot(ACC, aes(x = oil, y = HCR))+
  geom_line(aes(color = Material),size=1.5)
p1 <- p1 + labs(x = "")

p2 <- ggplot(ACC, aes(x = oil, y = FDR))+
  geom_line(aes(color = Material),size=1.5)  
p2 <- p2 + labs(x = "")


p3 <- ggplot(ACC, aes(x = oil, y =HMR))+
  geom_line(aes(color = Material),size=1.5) 

ggplot2.multiplot(p1,p2,p3, cols=1)



##p1 <- ggplot(ACC, aes(x = oil, y = HCR))+
geom_line(aes(color = Material),size=1.5)+
  labs(title="HCR~Oil",size=10)+
  theme(plot.title = element_text(hjust = 0.5))  #也就加上这一行

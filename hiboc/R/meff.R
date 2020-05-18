# meff function
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


meff<-function(data,plot=TRUE,print=TRUE){
eff_all<-data.frame()
res<-data.frame()
ng<-nrow(data)
for (n in 1:ng){
  #n=1
  oc_res<-data.frame()
  name<-as.character(data[n,1])
  m_H=data[n,2];sd_H=data[n,3];m_C=data[n,4];sd_C=data[n,5];hir=data[n,6]
  for (i in 1:1000) {
    #i=100
    j<-i*0.01

    W_H<-pnorm(j,m_H,sd_H)
    W_C<-pnorm(j,m_C,sd_C)
    W<-hir*W_H+(1-hir)*W_C
    Q<-((1-hir)*W_C)/W    ###FDR
    B<-1-W_H            ###FNR
    F<-(2*(1-Q)*(1-B))/(2-Q-B)  ###F
    acc<-(hir*W_H+(1-hir)*(1-W_C))  ###ACC
    oc_res[i,1]<- j
    oc_res[i,2]<-Q
    oc_res[i,3]<-B
    oc_res[i,4]<-F
    oc_res[i,5]<-name
    colnames(oc_res)<-c("oil","Q","B","F","Materials")

  }
  res<-rbind(res,oc_res)
  max<-oc_res[which(oc_res[,4]==max(oc_res[,4])),]
  oil_n<-mean(max[,1])
  HCR_n<-mean(max[,2])
  FDR_n<-mean(max[,3])
  F_n<-mean(max[,4])
  acc_sum<-c(name,oil_n,HCR_n,FDR_n,F_n)
  hh<-t(data.frame(acc_sum))
  colnames(hh)<-c("Materials","oil","FNR","FDR","F")
  eff_all<-rbind(eff_all,hh)
  oo<-list(res,eff_all)
  names(oo)<-c("res","sum")
}

if (plot == TRUE ){
  ng2<-sqrt(ng)
  ncol<-ceiling(ng2)
  nrow<- trunc(ng2)
  par(mfrow=c(nrow,ncol))
  for (n in 1:ng){

    res_n<-res[which(res$Materials == data[n,1]),]
    res_n<-res_n[order(res_n$oil),]
    plot(res_n$oil,res_n$F,type="b",pch=15,lty=1,col="red",ylim=c(0,1),xlim=c(1,10),
       xlab = "Oil content threshold (t) / %", ylab = "Value",main=data[n,1])
    lines(res_n$oil,res_n$B,type ="b",pch=16,lty=2,col="blue")
    lines(res_n$oil,res_n$Q,type ="b",pch=17,lty=3,col="green")
    legend("right",inset=0.05,cex=0.6,title="Index",c("F","FNR","FDR"),lty=c(1,1,1),pch=c(15,15,15),col =c("red","blue","green"))

  }
}
if (print == TRUE){
  print(oo[[2]])
}
return(oo)
}







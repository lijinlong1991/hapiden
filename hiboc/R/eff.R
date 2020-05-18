# eff function
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


eff<-function(m_H,sd_H,m_C,sd_C,hir,plot=TRUE,print=TRUE){
  oc_res<-data.frame()  #建立数据集
  for (i in 1:1000) {
    j<-i*0.01
    W_H<-pnorm(j,m_H,sd_H)
    W_C<-pnorm(j,m_C,sd_C)
    W<-hir*W_H+(1-hir)*W_C
    Q<-((1-hir)*W_C)/W    ###FDR
    B<-1-W_H            ###FNR
    F<-(2*(1-Q)*(1-B))/(2-Q-B)  ###F值
    acc<-(hir*W_H+(1-hir)*(1-W_C))  ###ACC
    res_i<-c(j,Q,B,F)
    oc_res<-rbind(oc_res,res_i)
    colnames(oc_res)<-c("oil","Q","B","F")
  }
  max<-oc_res[which(oc_res[,4]==max(oc_res[,4])),]
  oil_n<-mean(max[,1])
  HCR_n<-mean(max[,2])
  FDR_n<-mean(max[,3])
  F_n<-mean(max[,4])
  acc_sum<-c(oil_n,HCR_n,FDR_n,F_n)
  hh<-t(data.frame(acc_sum))
  colnames(hh)<-c("oil","FNR","FDR","F")
  oo<-list(oc_res,hh)
  if (plot == TRUE ){
    plot(oc_res$oil,oc_res$F,type="b",pch=15,lty=1,col="red",ylim=c(0,1),main= "",
         xlab = "Oil content threshold (t) / %", ylab = "Value")
    lines(oc_res$oil,oc_res$B,type ="b",pch=17,lty=2,col="blue")
    lines(oc_res$oil,oc_res$Q,type ="b",pch=18,lty=3,col="green")
    legend("topleft",inset=0.05,title="Index",c("F","FNR","FDR"),lty=c(1,2,3),pch=c(15,17,18),col =c("red","blue","green"))
  }
  if (print == TRUE){
    print(oo[[2]])
  }
  return(oo)
}

# pre function
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


pre<-function(fpoc,ioc,hir){
pre_T<- 4.81*fpoc + 0.13*ioc - 0.07*hir - 9.67
return(pre_T)
}








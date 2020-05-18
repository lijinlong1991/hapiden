\name{meff}
\alias{meff}
\title{Multi-group haploid identification by oil content!}
\usage{
meff(data,plot=TRUE,print=TRUE)
}
\description{

need to put in data

data[,1]: Materials

data[,2]: m_H, the mean oil content of hapliod kernels.

data[,3]: sd_H, the standard deviation of haploid kernels.

data[,4]: m_C, the mean oil content of crossed kernels.

data[,5], sd_C, the standard deviation of crossed kernels.

data[,6], hir, haploid induce rate.

plot: TRUE or FALSE, plot the result.

print: TRUE or FALSE, print the result.

}
\examples{
## Estimate the efficiency and print the best oil content threshold

meff(data,plot=TRUE,print=TRUE)

## or

mydata=data.frame(rbind(c(1,3,0.6,5,0.6,0.1),c(2,3,0.8,8,0.6,0.1),c(3,4,0.8,8,0.6,0.1),c(4,5,0.8,8,0.6,0.1),c(5,2,0.8,8,0.6,0.1)))

meff(mydata)

}

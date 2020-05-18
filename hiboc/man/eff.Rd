\name{eff}
\alias{eff}
\title{Haploid identification by oil content!}
\usage{
eff(ml_H,sd_H,m_C,sd_C,hir,plot=TRUE,print=TRUE)
}
\description{
m_H: The mean oil content of hapliod kernels.

sd_H: The standard deviation of haploid kernels.

m_C: The mean oil content of crossed kernels.

sd_C: The standard deviation of crossed kernels.

hir: Haploid induce rate.

plot: TRUE or FALSE, plot the result.

print: TRUE or FALSE, print the result.
}
\examples{
## Estimate the efficiency and print the best oil content threshold
eff(m_H=4.2,sd_H=0.6,m_C=5.8,sd_C=0.7,hir=0.15,plot=TRUE,print=TRUE)
## or
eff(4,0.6,6,0.6,0.12)
}

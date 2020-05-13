\name{eff}
\alias{eff}
\title{haploid identification by oil content!}
\usage{
eff(meanoil_H,sdoil_H,meanoil_C,sdoil_C,hir,plot=TRUE,print=TRUE)
}
\description{
meanoil_H: The mean oil content of hapliod kernels.
sdoil_H: The standard deviation of haploid kernels.
meanoil_C: The mean oil content of crossed kernels.
sdoil_C: The standard deviation of crossed kernels.
hir: Haploid induce rate
plot: TRUE or FALSE, plot the result.
print: TRUE or FALSE, print the result.
}
\examples{
## Estimate the efficiency and print the best oil content threshold
eff(meanoil_H=4.2,sdoil_H=0.6,meanoil_C=5.8,sdoil_C=0.7,hir=0.15,plot=TRUE,print=TRUE)
## or
eff(4,0.6,6,0.6,0.12)
}

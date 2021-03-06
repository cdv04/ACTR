% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msep_mu.R
\name{msep_mu}
\alias{msep_mu}
\title{Estimating the Mean Squared Error of Prediction for Mu parameter, of several linear models, using cross validation}
\usage{
msep_mu(PAC.df)
}
\arguments{
\item{PAC.df}{A dataframe containing the weighted parameters in log10 scale (wMuA_lg, wMuC_lg, wSigmaA_lg, wSigmaC_lg)}
}
\value{
The function return a data frame with the MSEP of the 6 linear models
}
\description{
6 linear models linear are compared (cf Details)
}
\details{
The 6 linear models are :

mod1  : wMuC_lg ~ 1

mod2  : wMuC_lg ~ wMuA_lg

mod3  : wMuC_lg ~ wMuA_lg + wSigmaA_lg

mod4  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2)

mod5  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2) + wSigmaA_lg

mod6  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2) + wSigmaA_lg + I(wSigmaA_lg^2)
}
\examples{
library(ACTR)
data(cipr)
ciprKP <- subset_data(cipr, Class,6)
PAC.bt <- lapply(ciprKP.bt,est_PAC,Class)
errMod_mu <- lapply(PAC_bt,msep_mu)
errMod_sigma <- lapply(PAC_bt,msep_sigma)
}


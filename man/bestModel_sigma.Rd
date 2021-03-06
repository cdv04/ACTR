% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bestModel_sigma.R
\name{bestModel_sigma}
\alias{bestModel_sigma}
\title{Selecting the best model, by bootstrap and Cross Validation,  to predict wSigmaC_lg}
\usage{
bestModel_sigma(errMod_Sigma.list)
}
\arguments{
\item{errMod_Sigma}{A list containing the error of prediction for each model
and each bootstrap samples}
}
\value{
The function return the formula of the best selected model
}
\description{
6 linear models are compared using Cross Validation on the basis of
the least error of prediction
}
\details{
The 6 linear models are :

mod1  :  wSigmaC_lg  ~ 1

mod2  :  wSigmaC_lg  ~ wSigmaA_lg

mod3  : wMuC_lg  ~ wMuA_lg  + wSigmaA_lg

mod4  :  wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2)

mod5  : wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2) + wMuA_lg

mod6  : wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2) + wMuA_lg  + I(wMuA_lg ^2)
}
\examples{
data(cipr)
ciprKP <- subset_data(cipr, Class,6)
PAC.bt <- lapply(ciprKP.bt,est_PAC,Class)
errMod_mu <- lapply(PAC.bt,msep_mu)
errMod_sigma <- lapply(PAC.bt,msep_sigma)
bm_sigma <- bestModel_mu(errMod_sigma)
}


#' Return wMu and wSigma of ED contained into a data frame
#' @param dat A data frame containing the Ed values, SpeciesComp and var-group (i.e. Class)
#' @examples
#'est_para(edr10_act)
#'lapply(edr10_obs_list, est_para)
#' @return A data frame with the estimated parameters
#' @export



# dat <- filter (edr10_all,type=="obs")
# dat <- filter (dat,Class=="Branchiopoda")
# dat <- dat[, c(1:6)]

est_para <- function(dat) {

  dat.ls <- split(dat, droplevels(dat$SpeciesComp))
  dat.ls <- lapply(dat.ls, function(x) {
    x$n_i <- length(x$ED)
    x
    })
  dat <- data.frame(Reduce(rbind,dat.ls))

  dat$SpeciesComp <- droplevels(dat$SpeciesComp)
  dat$n_s <- nlevels(dat$SpeciesComp)
  dat$w_ij <- 1/(dat$n_i*dat$n_s)
  dat$s_ij_w <- log10(dat$ED)*dat$w_ij
  wMu<-sum(dat$s_ij_w)
  dat$sss_ij <- (log10(dat$ED)-wMu)^2
  dat$sss_ij_w <- dat$sss_ij * dat$w_ij
  n<-dim(dat)[1]
  wSigma <- sqrt(sum(dat$sss_ij_w)*n/(n-1))
  res<-data.frame(dat[1,1],wMu, wSigma)
  names(res)[1] <- names(dat)[1]
  return(res)
  }




#' Coefficient of best models applied on observed data
#'
#'
#' @param PAC_obs dataframe containing the observed  acute and chronic weighted parameters
#' @param form_mu Formula of the best model selected to predict muC_act
#' @param form_sigma Formula of the best model selected to predict SigmaC_act
#' @return a list with the regression coefficients of the best model applied on observed data
#' @examples
#' library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC_obs <- est_PAC(ciprKP,Class)
#' coef_Mod <- coef_reg (PAC_obs,bm_mu, bm_sigma)



#' @export
# PAC_obs <- wpara_obs_lg
# form_mu <- bm_mu
# form_sigma <- bm_sigma
coef_reg <- function (PAC_obs,form_mu, form_sigma)
{


  form_mu<-as.character(form_mu)
  form_sigma<-as.character(form_sigma)

  dat <- PAC_obs

  mod_mu <- lm (formula=form_mu,data=dat)
  mod_sigma <- lm (formula=form_sigma,data=dat)

  coef_Mu <- as.numeric(sprintf("%4.2e",mod_mu$coefficients))
  coef_Sigma <- as.numeric(sprintf("%4.2e",mod_sigma$coefficients))
  coef <- list( coef_Mu,coef_Sigma)
  names(coef) <- c("Mod_Mu","Mod_Sigma")

  return(coef)
}





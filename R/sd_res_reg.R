


#' Return standard deviation of residuals  of the best models (1 for Mu, 1 for Sigma) applied on observed data
#'
#' @param PAC_obs dataframe containing the observed  acute and chronic weighted parameters
#' @param form_mu Formula of the best model selected to predict muC_act
#' @param form_sigma Formula of the best model selected to predict SigmaC_act
#' @return a list with the residuals standrd deviation of the best model applied on observed data
#' @examples
#' library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC_obs <- est_PAC(ciprKP,Class)
#' sd_res <- sd_res_reg (PAC_obs,bm_mu, bm_sigma)



#' @export
# PAC_obs <- wpara_obs_lg
# form_mu <- bm_mu
# form_sigma <- bm_sigma

sd_res_reg <- function (PAC_obs,form_mu, form_sigma)
{

  form_mu<-as.character(form_mu)
  form_sigma<-as.character(form_sigma)

  dat <- PAC_obs

  mod_mu <- lm (formula=form_mu,data=dat)
  mod_sigma <- lm (formula=form_sigma,data=dat)

  #  mean ( residuals(mod_sigma)) =  0 (by definition)
  sd_resMu <- as.numeric(sprintf("%4.2e",sd ( residuals(mod_mu))))

  # m_resSigma <- mean ( residuals(mod_sigma)) =  0 (by definition)
  sd_resSigma <-  as.numeric(sprintf("%4.2e",sd ( residuals(mod_sigma))))

  sd_res<-data.frame (Mu = sd_resMu, Sigma = sd_resSigma)
  names( sd_res) <- c("Mod_Mu","Mod_Sigma")

  return(sd_res)
}





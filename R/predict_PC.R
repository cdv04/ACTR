
#' Prediction of muC_reg and SigmaC_reg
#'
#' The function takes in argument the observed weigted parameters of ED50
#' i.e mu_A and sigma_A, and the formula of the best models for prediction
#' of mu_C_act and sigma_C_act and return the muC_act and SigmaC_act
#' They are estimated using a cross validation. When parameters of one class are
#' predicted data of that class are not taken in acount in the regression step
#'
#'
#' @param PAC_obs dataframe containing the observed  acute and chronic weighted parameters
#' @param form_mu Formula of the best model selected to predict muC_act
#' @param form_sigma Formula of the best model selected to predict SigmaC_act
#' @return a list with the Chronic predicted  parameters (MuC_act and SigmaC_act) ,
#' regression coefficients for MuC_reg and SigmaC_reg and sd of residuals for  both regressions
#' NB : means of residuals = 0 by definition
#' @examples
#' library(ACTpckg)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC_obs<-est_PAC(ciprKP,Class)
#' PC_pred <- predict_PC(PAC_obs,bm_mu, bm_sigma)
#'


#' @export
# PAC_obs <- PAC_obs
# form_mu <- bm_mu
# form_sigma <- bm_sigma


predict_PC <- function (PAC_obs,form_mu, form_sigma)
{


  coef_Mod <- coef_reg (PAC_obs,form_mu, form_sigma)
  coef_mu <- coef_Mod [[1]]
  coef_sigma <- coef_Mod [[2]]


  # récupère la formula
  form_mu<-form_mu[[1]]


  # cré un data frame avec les variables du best model, pris dans PAC_obs
  #(ici wMuC_lg wMuA_lg), necessaire pour créer la matrice des X
  modFr_mu <- model.frame(form_mu, PAC_obs)

  # cré une matrice de paramètres (ici intercept = 1 et wMuA_lg pr set seed 1234)
  mat_mu_X <- model.matrix(form_mu,modFr_mu)

  # cré une matrice avec les coeff des paramètres du best model
  mat_mu_beta <-  matrix(coef_mu, nrow=length(coef_mu), byrow=TRUE)

  #cré une matrice avec les wMuC_lg predits = MuC_lg_eq
  mat_muC_eq <- mat_mu_X  %*% mat_mu_beta


  # idem pour sigma
  form_sigma<-form_sigma[[1]]
  modFr_sigma <- model.frame(form_sigma, PAC_obs)
  mat_sigma_X <- model.matrix(form_sigma,modFr_sigma)
  mat_sigma_beta <-  matrix(coef_sigma, nrow=length(coef_sigma), byrow=TRUE)
  mat_sigmaC_eq <- mat_sigma_X  %*% mat_sigma_beta




  # mise en forme des resultats
  MuC_pred <- as.numeric(sprintf("%4.2e",mat_muC_eq ))
  SigmaC_pred  <- as.numeric(sprintf("%4.2e",mat_sigmaC_eq))

  para <- data.frame(PAC_obs[,1],
                      MuC_pred= MuC_pred,
                     SigmaC_pred = SigmaC_pred)

  names(para)[1] <- names(PAC_obs)[1]


  return(para)
}



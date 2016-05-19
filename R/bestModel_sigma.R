#' Selecting the best model, by bootstrap and Cross Validation,  to predict wSigmaC_lg
#'
#'6 linear models are compared using Cross Validation on the basis of
#'the least error of prediction
#'
#'The 6 linear models are :
#'
#' mod1  :  wSigmaC_lg  ~ 1
#'
#' mod2  :  wSigmaC_lg  ~ wSigmaA_lg
#'
#' mod3  : wMuC_lg  ~ wMuA_lg  + wSigmaA_lg
#'
#' mod4  :  wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2)
#'
#' mod5  : wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2) + wMuA_lg
#'
#' mod6  : wSigmaC_lg  ~ wSigmaA_lg  + I(wSigmaA_lg ^2) + wMuA_lg  + I(wMuA_lg ^2)
#'
#'
#' @param errMod_Sigma A list containing the error of prediction for each model
#' and each bootstrap samples
#' @return The function return the formula of the best selected model
#' @examples
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC.bt <- lapply(ciprKP.bt,est_PAC,Class)
#' errMod_mu <- lapply(PAC.bt,msep_mu)
#' errMod_sigma <- lapply(PAC.bt,msep_sigma)
#' bm_sigma <- bestModel_mu(errMod_sigma)
#' @export




bestModel_sigma<-function (errMod_Sigma.list) {

  err_mod1_list <- lapply(errMod_Sigma.list,"[", 1, 2)
  err.mod1_df <- data.frame(Reduce(rbind,err_mod1_list), row.names = NULL)
  names(err.mod1_df) <-c("err")
  err.mod1_mean <- mean( err.mod1_df$err )


  err_mod2_list <- lapply(errMod_Sigma.list,"[", 2, 2)
  err.mod2_df <- data.frame(Reduce(rbind,err_mod2_list), row.names = NULL)
  names(err.mod2_df) <-c("err")
  err.mod2_mean <- mean( err.mod2_df$err )


  err_mod3_list <- lapply(errMod_Sigma.list,"[", 3, 2)
  err.mod3_df <- data.frame(Reduce(rbind,err_mod3_list), row.names = NULL)
  names(err.mod3_df) <-c("err")
  err.mod3_mean <- mean( err.mod3_df$err )


  err_mod4_list <- lapply(errMod_Sigma.list,"[", 4, 2)
  err.mod4_df <- data.frame(Reduce(rbind,err_mod4_list), row.names = NULL)
  names(err.mod4_df) <-c("err")
  err.mod4_mean <- mean( err.mod4_df$err )

  err_mod5_list <- lapply(errMod_Sigma.list,"[", 5, 2)
  err.mod5_df <- data.frame(Reduce(rbind,err_mod5_list), row.names = NULL)
  names(err.mod5_df) <-c("err")
  err.mod5_mean <- mean( err.mod5_df$err )


  err_mod6_list <- lapply(errMod_Sigma.list,"[", 6, 2)
  err.mod6_df <- data.frame(Reduce(rbind,err_mod6_list), row.names = NULL)
  names(err.mod6_df) <-c("err")
  err.mod6_mean <- mean( err.mod6_df$err )

  mod1_form <- formula(wSigmaC_lg ~1)
  mod2_form <- formula(wSigmaC_lg ~wSigmaA_lg )
  mod3_form <- formula(wSigmaC_lg ~wSigmaA_lg  + wMuA_lg )
  mod4_form <- formula(wSigmaC_lg ~wSigmaA_lg +I(wSigmaA_lg ^2))
  mod5_form <- formula(wSigmaC_lg ~wSigmaA_lg +I(wSigmaA_lg ^2)+wMuA_lg )
  mod6_form <- formula(wSigmaC_lg ~wSigmaA_lg +I(wSigmaA_lg ^2)+wMuA_lg +I(wMuA_lg ^2))



  mod_name <- paste("mod", c(1:6), sep="_" )
  mean_err_pred <-c (err.mod1_mean,err.mod2_mean,err.mod3_mean,err.mod4_mean,
                     err.mod5_mean,err.mod6_mean )

  mod_form <-c (mod1_form, mod2_form, mod3_form, mod4_form, mod5_form, mod6_form )
  mod_form_char <- as.character (mod_form )
  res<-data.frame(mod_name,mean_err_pred,mod_form_char )
  print(res )

  ind_bm <- which.min(res$mean_err_pred) # index of the best model (lowest pred err)
  bm_form <- mod_form[ind_bm]
  return(bm_form)
}





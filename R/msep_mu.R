#' Estimating the Mean Squared Error of Prediction for Mu parameter, of several linear models, using cross validation
#'
#'6 linear models linear are compared (cf Details)
#'
#'The 6 linear models are :
#'
#' mod1  : wMuC_lg ~ 1
#'
#' mod2  : wMuC_lg ~ wMuA_lg
#'
#' mod3  : wMuC_lg ~ wMuA_lg + wSigmaA_lg
#'
#' mod4  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2)
#'
#' mod5  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2) + wSigmaA_lg
#'
#' mod6  : wMuC_lg ~ wMuA_lg + I(wMuA_lg^2) + wSigmaA_lg + I(wSigmaA_lg^2)
#'
#'
#' @param PAC.df A dataframe containing the weighted parameters in log10 scale (wMuA_lg, wMuC_lg, wSigmaA_lg, wSigmaC_lg)
#' @return The function return a data frame with the MSEP of the 6 linear models
#' @examples
#' library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC.bt <- lapply(ciprKP.bt,est_PAC,Class)
#' errMod_mu <- lapply(PAC_bt,msep_mu)
#' errMod_sigma <- lapply(PAC_bt,msep_sigma)
#' @export



msep_mu<-function (PAC.df)
{


  library(boot)

    glm.fit1 <- glm(wMuC_lg~1, data=PAC.df)
    cv.err1 <- cv.glm(PAC.df, glm.fit1)$delta[1]

    glm.fit2 <- glm(wMuC_lg~wMuA_lg, data=PAC.df)
    cv.err2 <- cv.glm(PAC.df, glm.fit2)$delta[1]

    glm.fit3 <- glm(wMuC_lg~wMuA_lg+wSigmaA_lg, data=PAC.df)
    cv.err3 <- cv.glm(PAC.df, glm.fit3)$delta[1]

    glm.fit4 <- glm(wMuC_lg~wMuA_lg+I(wMuA_lg^2), data=PAC.df)
    cv.err4 <- cv.glm(PAC.df, glm.fit4)$delta[1]

    glm.fit5 <- glm(wMuC_lg~wMuA_lg+I(wMuA_lg^2)+wSigmaA_lg, data=PAC.df)
    cv.err5 <- cv.glm(PAC.df, glm.fit5)$delta[1]

    glm.fit6 <- glm(wMuC_lg~wMuA_lg+I(wMuA_lg^2)+wSigmaA_lg+I(wSigmaA_lg^2), data=PAC.df)
    cv.err6 <- cv.glm(PAC.df, glm.fit6)$delta[1]


    mod_name <- paste("mod", c(1:6), sep="_")
    err_pred <-c (cv.err1,cv.err2,cv.err3,cv.err4,cv.err5,cv.err6)
    mod_form <-c (glm.fit1$formula,
                  glm.fit2$formula,
                  glm.fit3$formula,
                  glm.fit4$formula,
                  glm.fit5$formula,
                  glm.fit6$formula)
    mod_form_char <- as.character (mod_form)
    res<-data.frame(mod_name,err_pred,mod_form_char)
    return(res)


}



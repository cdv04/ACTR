
#' Calculating IC of HDR (th) of edr10_act from log normal distribution
#'
#' Number of bootstrap samples is determinated by PAC_boot
#'
#' @param data A dataframe with the data
#' @param bestModelMu A linear model
#' @param bestModelSigma A linear model
#' @param th The desired probability (i.e. 0.05 for HDR5)
#' @param B The desired number of bootstrap sample from which confidence intervals are build
#' @return a data frame with estimated HDR5_act and 95% confidence interval (not in log scale)
#' @examples
#' hdr5_act <- HDR5_act ( ciprKP, BM_Mu, BM_Sigma,PAC_boot )
#' @export


## besoin
# dat <- ciprKP
# BM_Mu <- bm_mu
# BM_Sigma <- bm_sigma
# PAC_boot <- PAC_bt
# varGrp <- c("Class")
# dat.bt <- boot_data(dat,Class,100)

IC_HdrLN_act <- function (dat,varGrp,BM_Mu, BM_Sigma,th,B)
{



  ##############################################################################
  ## Pre process
  ##

  #dat<- ciprKP
  #PAC_boot <- PAC_bt

  MyVarGrp <- substitute(varGrp)






  ##############################################################################
  #       generating edr10 from ED50 obs and  calculating hdr th_act
  ##############################################################################


  # on calcul les wMuA_lg, WSigmaA_lg, des donnes ED50_obs
  # (données dat (i.e CIPRKP) selon VarGrp (i.e Class) grace à la fonction
  # est_PAC (qui calcule aussi les paramètres des EDR10_obs mais on ne s'en sert
  # pas
  PAC_obs <- do.call(est_PAC,list(dat,MyVarGrp))


  # on récupère seulement les paramètres des EDR50 _obs cad les wMuA_lg,
  # WSigmaA_lg calculés précédemment
  PA_obs<-arrange(PAC_obs[,1:3],PAC_obs[,1])



  # on injecte les paramètres des ED50 dans les équation des best models pour
  # déterminer les WMuC_lg et wSigmaC_lg prédits
  PC_pred <- predict_PC(PAC_obs,BM_Mu,BM_Sigma)


  # on met ensemble dans un data frame les paramètres des ED50 et les paramètres
  # chronic prédits  () WMuC_lg et wSigmaC_lg prédits), calculés précédemment
  PC_pred <- arrange(PC_pred,PC_pred[,1])
  PAC_gen <- cbind(PA_obs,PC_pred[,-1])


  # on récupère les données acute cad les ED50
  dat.ac <- filter(dat, DoseType=="Acute")

  # on génére les edr10_act à partir des ED50 et des paramètres wMuA_lg,
  # WSigmaA_lg calculés et WMuC_lg et wSigmaC_lg prédits selon les best models
  edr10_act <- gen_edr10(PAC_gen,dat.ac)

  # on calcule hdr th des edr10_act provenant des ED50_obs, selon loi log normale
  hdr_LNA <- Hdr_LN(edr10_act,th)


  ##############################################################################
  ##  generating bootstrap sample of ED50 and calculating WMuA_lg et wSigmaA_lg
  ##
  ##############################################################################


  # estimation of weighted (on species) log10 Mu_A, Mu_C, Sigma_A and Sigma_C of
  #bootstrap sample


  # on fait des échantillons bootstrap des données ciprKP (on ne gardera que les
  #ED50)
  dat.bt <- do.call (boot_data,list(dat,MyVarGrp,B ))



  # on estime les paramètres des ED50 (on estime aussi ceux des EDR10 mais on
  #ne s en servira pas)
  PAC_bt <- vector("list",B)
  for (i in 1 : B) {

    PAC_bt[[i]] <- do.call(est_PAC, list(dat.bt[[i]],MyVarGrp))
  }



  ##############################################################################
  ## Collecting MuC_eq and SigmaC_eq for each boostrap sample using coeff of
  ## best models (thats why there are named with "eq", it means using the
  ## equation and adding error from normal distribution
  ##############################################################################


  N <- dim (PAC_obs)[1] # nb of VarGrp level

  #  on récupère les coeff des models best, necessaire à la prediction des
  #WMuC_lg et wSigmaC_lg  des ech bootstrap
  coef_Mod <- coef_reg (PAC_obs,BM_Mu, BM_Sigma)
  coef_mu <- coef_Mod [[1]]
  coef_sigma <- coef_Mod [[2]]
  # on récupère les écart type des résidus que l'on va injecter par la suite
  #pour prédire les WMuC_lg et wSigmaC_lg  des ech bootstrap
  sd_res <- sd_res_reg (PAC_obs,BM_Mu, BM_Sigma)

  # récupère la formula a formulae du best model, necessaire pour creer les matrices des X
  # et des Beta, necessaire à la prédiction des MuC_eq and SigmaC_eq
  # for each boostrap
  form_mu<-BM_Mu[[1]]
  form_sigma<-BM_Sigma[[1]]


  PAC_eq_bt <- vector("list",B)

  for (b in 1 :B )
  {


    # cré un data.frame avec les variables du best model, pris dans PAC_bt
    #(ici wMuC_lg wMuA_lg), necessaire pour créer la matrice des X
    modFr_mu <- model.frame(form_mu, PAC_bt[[b]])

    # cré une matrice de paramètres (ici intercept = 1 et wMuA_lg )
    mat_mu_X <- model.matrix(form_mu,modFr_mu)

    # cré une matrice avec les coeff des paramètres du best model
    mat_mu_beta <-  matrix(coef_mu, nrow=length(coef_mu), byrow=TRUE)

    # cré une matrice avec les wMuC_lg predits = MuC_lg_eq
    mat_muC_eq <- mat_mu_X  %*% mat_mu_beta

    # adding error from MC into normale (0,sd=sd_res$Mod_Mu)

    er_Mu <- rnorm(N,mean=0, sd=sd_res$Mod_Mu)
    MuC_eq <- as.numeric(sprintf("%4.2e",mat_muC_eq + er_Mu))
    MuC_eq <- as.data.frame(MuC_eq)




    ### idem pour sigma
    modFr_sigma <- model.frame(form_sigma, PAC_bt[[b]])
    mat_sigma_X <- model.matrix(form_sigma,modFr_sigma)
    mat_sigma_beta <-  matrix(coef_sigma, nrow=length(coef_sigma), byrow=TRUE)
    mat_sigmaC_eq <- mat_sigma_X  %*% mat_sigma_beta

    # adding error from MC into normale (0,sd=sd_res$Mod_sigma)
    er_sigma <- rnorm(N,mean=0, sd=sd_res$Mod_Sigma)
    SigmaC_eq <- as.numeric(sprintf("%4.2e",mat_sigmaC_eq + er_sigma))
    SigmaC_eq <- as.data.frame(SigmaC_eq)


    # recupère les paramètres prédits selon l'équation des best models
    PC_eq <- cbind(MuC_eq ,SigmaC_eq  )

    # recupère les paramètres des ED50_obs
    PA_bt <- PAC_bt [[b]][,1:3]

    # cré un data.frame avec les paramètres des ED50_obs et des paramètres
    # chroniques prédits, nécessaire pour la génération des EDR10 à partir des
    # ED50_obs
    PAC_eq_bt [[b]] <- cbind(PA_bt, PC_eq)

  }


  ##############################################################################
  ## generating EDR10_act for each bootstraped samples using  wMuA_lg ,
  ## wSigmaA_lg MuC_eq SigmaC_eq contained into  PAC_eq_bt
  ##############################################################################


  var_grp <- names(PAC_bt[[1]])[1]
  ind_grp <- which(names(dat.bt[[1]])==var_grp)


  # on ne récupère que les données acute des échantillons bootstrap
  acute_bt_list<-lapply(dat.bt, function(x){
    x<-filter(x,DoseType=="Acute")
    x <- arrange (x,x[,ind_grp],SpeciesComp)
    return(x)
  })





  for ( b in 1 : B) # b index of bootstrap sample
  {
    # tmp.ls prend un ech bootstrap de données acute et met les class ds une list
    # tmp.ls contient les données de chaque level de var_grp ds un element
    tmp.ls <- split(acute_bt_list[[b]], acute_bt_list[[b]][,ind_grp])

    for (i in 1 : N ) # i index of var_grp level (i.e. Class)
    {
      wMuA_lg <- PAC_eq_bt[[b]][i,]$wMuA_lg
      wSigmaA_lg <- PAC_eq_bt[[b]][i,]$wSigmaA_lg
      MuC_eq <- PAC_eq_bt[[b]][i,]$MuC_eq
      SigmaC_eq <- PAC_eq_bt[[b]][i,]$SigmaC_eq

      tmp.ls[[i]]$EDR10_act <- 10^(SigmaC_eq * ((log10(tmp.ls[[i]]$ED) - wMuA_lg )/  wSigmaA_lg) + MuC_eq)

    }
    tmp <- data.frame (Reduce (rbind, tmp.ls))
    acute_bt_list[[b]] <- tmp
  }

  toMatch <- c("EDR10_act", "SpeciesComp", "ID", "sub.id", "Umbrella")
  ind_tmp <- unique (grep(paste(toMatch,collapse="|"),names(acute_bt_list[[1]]),value=FALSE))

  ind_kpt <- c(ind_grp,ind_tmp)
  edr10_act_bt <- lapply(acute_bt_list, "[",,ind_kpt)





  ##############################################################################
  ## Calculating hdr_th_act_bt cad hdr_th pour tous les echantillons d'edr10_act
  ##############################################################################

  hdr_act_bt<-vector("list",B) # initialize hdr th vector
  hdr_act_bt <- lapply ( edr10_act_bt, Hdr_LN,th)




  ## putting in a list (1 element by var_grp level), all the B bootstrap sample hdr5
  hdr_tmp <- vector("list",N)
  for (i in 1 : N) {
    hdr_tmp[[i]] <-sapply(hdr_act_bt,"[", i,2 )
  }
  names(hdr_tmp) <-  hdr_act_bt[[1]][,1]



  binf <- vector(length=N)
  bsup <- vector(length=N)

  for (i in 1:N) {
    binf[i] <- 10^(quantile(log10(hdr_tmp[[i]]),0.025) - quantile(log10(hdr_tmp[[i]]),0.5) + log10(hdr_LNA[i,2]))
    bsup[i] <- 10^(quantile(log10(hdr_tmp[[i]]),0.975) - quantile(log10(hdr_tmp[[i]]),0.5) + log10(hdr_LNA[i,2]))
  }

  binf <- as.numeric(sprintf("%4.3e",binf))
  bsup <- as.numeric(sprintf("%4.3e",bsup))



  ## a build a dataframe of results
  res<- cbind(hdr_LNA,binf,bsup)
  names(res) [3:4]<- c("q2.5%", "q97.5%")
  rownames(res) <- NULL
  res <- res [,-5]
  return(res)
}




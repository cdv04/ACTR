#' Plot Log normal cumulative distribution function) of edr10_obs act and edr10_act
#' @param edr10_all A Data frame containing edr10_act and edr10_act, with their plotting position
#' @param PC_act A Data frame containing the parameters of edr10_act
#' @param PC_act A Data frame containing the parameters of edr10_act
#' @examples
#' plot_ln <- plotLN(edr10_all,PC_act, PC_gen)
#' @return A ggplot object
#' @export

plotLN <- function (edr10_all)
{

  library(dplyr)
  library(ggplot2)
  edr10_all$type <- as.factor(edr10_all$type)
  edr10_obs <-  filter(edr10_all, type=="obs")
  edr10_obs_list <- split(edr10_obs, droplevels(edr10_obs[,1]))
  N_obs <- length( edr10_obs_list)
  pred_obs_list <-vector("list",N_obs)


  ### generating prediction according Log Normal distribution, for edr10_obs

  for (i in 1 : N_obs)
  {

    dat_tmp <- edr10_obs_list[[i]]
    wMu <- est_para(dat_tmp)[1,2]
    wSigma <- est_para(dat_tmp)[1,3]
    x_min <- min(log10(dat_tmp$EDR10)) - 1
    x_max <- max(log10(dat_tmp$EDR10)) + 2
    s_ij <- seq(x_min,x_max, length.out=100)
    p_i_w <- pnorm (s_ij, wMu, wSigma)
    grp <- rep(edr10_obs_list[[i]][1,1],length(s_ij))
    pred_obs_list[[i]] <- data.frame(grp,s_ij,p_i_w)
  }

  pred_obs_df <- data.frame(Reduce(rbind,pred_obs_list))
  pred_obs_df$type <- c("obs")




  ### generating prediction according Log Normal distribution, for edr10_act

  edr10_act <-  filter(edr10_all, type=="act")
  edr10_act_list <- split(edr10_act, droplevels(edr10_act[,1]))
  N_act <- length( edr10_act_list)
  pred_act_list <-vector("list",N_act)




  for (i in 1 : N_act)
  {

    dat_tmp <- edr10_act_list[[i]]
    wMu <- est_para(dat_tmp)[1,2]
    wSigma <- est_para(dat_tmp)[1,3]
    x_min <- min(log10(dat_tmp$EDR10)) - 1
    x_max <- max(log10(dat_tmp$EDR10)) + 2
    s_ij <- seq(x_min,x_max, length.out=100)
    p_i_w <- pnorm (s_ij, wMu, wSigma)
    grp <- rep(edr10_act_list[[i]][1,1],length(s_ij))
    pred_act_list[[i]] <- data.frame(grp,s_ij,p_i_w)
  }


  pred_act_df <- data.frame(Reduce(rbind,pred_act_list))
  pred_act_df$type <- c("act")



  pred_all_df <- rbind ( pred_obs_df, pred_act_df)
  pred_all_df$type <- as.factor( pred_all_df$type)
  names(pred_all_df)[1] <-c("grp")

  edr10_all<-rbind(edr10_obs, edr10_act)
  edr10_all$type <- as.factor( edr10_all$type)
  names(edr10_all)[1] <-c("grp")


 gLN <-  ggplot(edr10_all, aes(x=10^s_ij, y=p_i_w, colour=type))+
          geom_point(size=3, aes(colour=type, shape=type))+
          scale_x_log10()+
          facet_wrap(~grp, scale="free")+
          geom_line (data=pred_all_df, size=1)+
          xlab("EDR10") +
          ylab ( "Cumulative weighted probability")+
          ggtitle("Log Normal")+
          theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
          theme_bw()+
          scale_colour_manual(values=c("grey60", "grey20"))+
          theme(strip.text = element_text(face="bold", size=rel(1.5)))




  return(gLN)
}




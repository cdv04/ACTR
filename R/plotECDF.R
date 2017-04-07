#' Plot ECDF (empirical cumulative distribution function) of edr10_obs and edr10_act
#' @param plot_position.df A Data frame containing the plotting position
#' @examples
#' plot_ecdf <- plotECDF(pp)
#' @return A ggplot object
#' @export

# plot_position.df <- pp

plotECDF <- function (plot_position.df) {
  library(ggplot2)

  pp <- plot_position.df
  gEcdf <- ggplot(pp, aes(x=10^s_ij, y=p_i_w, colour=type)) +
            geom_point(size=3, aes(colour=type, shape=type))+
            geom_step () +
            facet_wrap(~ Class, scale="free") +
            scale_x_log10() +
            xlab("EDR10") +
            ylab ( "Cumulative weighted probability")+
            ggtitle("Empirical")+
            theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
            theme_bw()+
            scale_colour_manual(values=c("grey60", "grey20"))+
            theme(strip.text = element_text(face="bold", size=rel(1.5)))



  return(gEcdf)
}

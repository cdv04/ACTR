#' Return a dataset without the identified outliers according coefficient
#' This function is based on boxplot.stat function
#' @param data A dataframe containing the edr10 or ed50
#' @param coef this determines how far the plot ‘whiskers’ extend out from the
#' box. If coef is positive, the whiskers extend to the most extreme data point
#' which is no more than coef times the length of the box away from the box.
#' A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned).

#' @examples
#' ciprKP1_5.Ac<-rm_outliers(ciprKP0.Ac,1.5)
#' @return the dataset without the outliers
#' @export


rm_outliers<-function(data,coef)
{

  data.List<-split(data, droplevels(data$Class))
  Res.List<-vector("list", length(data.List))
  Ind.List<-vector("list", length(data.List))

  names(Res.List)<-names(data.List)
  names(Ind.List)<-names(data.List)


  for (i in 1 :length(data.List))
  {
    EDOut<-10^boxplot.stats(log10(data.List[[i]]$ED),coef=coef)$out
    if(length(EDOut)!=0)
    {
      toMatch<-EDOut
      Ind.List[[i]] <- unique (grep(paste(toMatch,collapse="|"),data.List[[i]]$ED, value=FALSE))
      resdf<-data.List[[i]][Ind.List[[i]], c(4,13,14,19)]
      resdf<-arrange(resdf, ED)
      Res.List[[i]]<-resdf
      rm(resdf)
    }
    else
    {
      Res.List[[i]]<-NA
    }

  }




  data.List2<-data.List

  for (i in 1 : length(data.List2))
  {
    if(is.null(Ind.List[[i]])==FALSE)
      data.List2[[i]]<-data.List2[[i]][-Ind.List[[i]],]
  }


  data2<-data.frame(Reduce(rbind,data.List2))
  return(data2)

}
#




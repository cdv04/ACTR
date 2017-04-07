

#' Return the number of outliers in the dataset taken in entry and according the coefficient.
#' This function is based on boxplot.stat function
#' @param data A dataframe containing the edr10 or ed50
#' @param var_grp A group variable telling how counts have to be done (ie.by Class)
#' @param coef this determines how far the plot ‘whiskers’ extend out from the
#' box. If coef is positive, the whiskers extend to the most extreme data point
#' which is no more than coef times the length of the box away from the box.
#' A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned).

#' @examples
#'  nb_outliers(ciprKP0.Ac,Class,coef)
#' @return the number of Outliers according the chosen coefficient
#' @export



#data <- ciprKP0.Ac
# grp <- substitute(Class)

nb_outliers<-function(data,var_grp,coef)
{

  grp <- substitute(var_grp)
  ind_grp <- which(colnames(data)==grp)


  data.List<-split(data, droplevels(data[, ind_grp]))

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

    }
  }

  nb.out<-0
  for (i in 1 : length(Ind.List))
  {
    nb.out<-nb.out+length(Ind.List[[i]])

  }

  return(nb.out)

}




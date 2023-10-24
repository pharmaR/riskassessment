
# utils ####
#' @name splitrel
#' @title Split Half-Based Reliability Coefficients
#' @seealso \link{covrel}
NULL

#' @describeIn splitrel Perform a Spearman-Brown correction on the provided correlation score.
#'
#' @param corr To-be-corrected correlation coefficient
#' @param ntests An integer indicating how many times larger the full test is, for which the corrected correlation coefficient is being computed.
#' When \code{ntests=2}, the formula will compute what the correlation coefficient would be if the test were twice as long.
#' @param fix.negative Determines how to deal with a negative value. "nullify" sets it to zero,
#' "bilateral" applies the correction as if it were a positive number, and then sets it to negative.
#' "none" gives the raw value. It should be noted that negative values are not supposed to occur,
#' and there is no commonly accepted way to deal with them when they do occur.
#' @return Spearman-Brown-corrected correlation coefficient.
#' @export
#'
#' @examples
#'
#' SpearmanBrown(.5)
SpearmanBrown<-function(corr,ntests=2,fix.negative=c("none","nullify","bilateral")){
  fix.negative<-match.arg(fix.negative)
  if(fix.negative=="bilateral"){
    s<-sign(corr)
    corr<-abs(corr)
    sb<-ntests*corr / (1+(ntests-1)*corr)
    return(s*sb)
  }else{
    sb<-ntests*corr / (1+(ntests-1)*corr)
    if(fix.negative=="nullify"){
      return(ifelse(sb<0,0,sb))
    }else{
      return(sb)
    }
  }
}

#' @describeIn splitrel Compute the true reliability using the Flanagan-Rulon formula,
#' which takes into account inequal variances between split halves.
#' @param x1 scores from half 1
#' @param x2 scores from half 2
#' @export
#'
#' @examples
#' FlanaganRulon(a<-rnorm(50),rnorm(50)+a*.5,fix.negative="bilateral")
FlanaganRulon<-function(x1,x2,fix.negative=c("none","nullify","bilateral")){
  fix.negative<-match.arg(fix.negative)
  d<-var(x1-x2)
  k<-var(x1+x2)

  if(fix.negative=="none"){
    return(1-d/k)
  }else if(fix.negative=="bilateral"){
    fr<-(1-d/k)
    #fr<-ifelse(fr>0,fr,fr / (1-fr))
    fr<-fr/max(1, 1-fr)
    return(fr)
  }else if(fix.negative=="nullify"){
    fr<-1-d/k
    return(ifelse(fr>0,fr,0))
  }
}

#' @describeIn splitrel Compute split-half reliability using the Raju formula,
#' which takes into account unequal split-halves and variances.
#'
#' @param prop Proportion of the first half to the complete sample
#'
#' @export
#'
#' @examples
#' a<-rnorm(50)
#' b<-rnorm(50)+a*.5
#' RajuCoefficient(a,b,prop=.4,fix.negative="bilateral")
RajuCoefficient<-function(x1,x2,prop,fix.negative=c("none","nullify","bilateral")){
  fix.negative<-match.arg(fix.negative)
  covar<-cov(x1,x2)
  if(fix.negative=="bilateral"){
    sumvar<-var(x1)+var(x2)+2*abs(covar)
  }else{
    sumvar<-var(x1)+var(x2)+2*covar
  }

  raju<-covar / (prop * (1-prop) * sumvar)
  return(ifelse(fix.negative=="nullify" & raju<0,0,raju))
}


#' @name covrel
#' @title Covariance Matrix-Based Reliability Coefficients
#' @description These functions allow for the computation of the reliability of a dataset
#' from the covariance matrix of its variables.
#' @seealso \link{splitrel}
#' @examples
#' # compute reliability from covariance
#' h<-cov(iris[,1:4])
#' calpha(h)
#' lambda2(h)
#' lambda4(h)
#' # Lambda-2 and Lambda-4 are significantly larger because
#' # some of the variables in the iris dataset are negatively correlated.
NULL



#' @describeIn covrel Cronbach's alpha
#' @param covmat a covariance matrix
#' @export
calpha<-function(covmat){
  (nrow(covmat)/(nrow(covmat)-1))*(1 - sum(diag(covmat))/sum(covmat))
}

#' @describeIn covrel Guttman's Lambda-2
#' @export
lambda2<-function(covmat){
  offs<-covmat[upper.tri(covmat)]
  covs<-2*sum(offs)
  sqcov<-2*sum(offs^2)
  sums<-sum(covmat)
  n<-dim(covmat)[1]
  covs/sums + sqrt(n/(n-1)*sqcov)/sums
}

#' @describeIn covrel Guttman's Lambda-4. This algorithm tries to get the highest attainable reliability by
#' @export
lambda4<-function(covmat){
  flip<-rep(1,ncol(covmat))
  itermaxid<- -1
  itermax<- -1
  while(itermaxid != 0){
    itermaxid<-0
    for(i in seq_along(flip)){
      key<-rep(1,ncol(covmat))
      key[i]<- -1
      itera<-calpha(t(t(covmat*flip*key)*flip*key))
      if(itera>itermax){
        itermax<-itera
        itermaxid<-i
      }
    }
    if(itermaxid>0){
      flip[itermaxid]<- -flip[itermaxid]
    }
  }
  calpha(t(t(covmat*flip)*flip))
}

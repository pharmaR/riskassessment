#' @name correlation-tools
#' @title Correlation tools
#' @description Helper functions to compute important statistics from correlation coefficients.
#' @param r,r1,r2 a correlation value
#' @param z a Z-score
#' @param n,n1,n2 sample sizes
#' @param alpha the significance level to use
#' @seealso \link{cormean}, \link{multiple.cor}, \link{partial.cor}
#' @examples
#' z <- r2z(.5)
#' r <- z2r(z)
#' t<-r2t(r,30)
#' r2p(r,30)
#' print(rconfint(r,30))
#' print(compcorr(.5,.7,20,20))
NULL

#' @export
#' @describeIn correlation-tools converts correlation coefficients to z-scores
r2z<-function(r){
  z<-.5 * (log(1+r) - log(1-r))
  return(z)
}
#' @export
#' @describeIn correlation-tools converts z-scores to correlation coefficients
z2r<-function(z){
  r<-(exp(2*z)-1)/(exp(2*z)+1)
  rma<-which(is.nan(r))
  r[rma]<-ifelse(z[rma]>0,1,-1)
  return(r)
}
#' @export
#' @describeIn correlation-tools Converts correlation coefficients to t-scores
r2t<-function(r,n){ (r*sqrt(n-2))/sqrt(1-r^2) }
t2r<-function(t,n){ sqrt(t/sqrt(t^2+n-2)) }
#' @export
#' @describeIn correlation-tools Computes the two-sided p-value for a given correlation
r2p<-function(r,n){ 2*pt(abs(r2t(r,n)),n-2,lower.tail=FALSE) }
#' @export
#' @describeIn correlation-tools Computes confidence intervals for a given correlation coefficient
rconfint<-function(r,n,alpha=.05){
  z<-r2z(r)
  zint<-qnorm(1-alpha/2) * sqrt(1/(n-3))
  confints<-c(z2r(z-zint),z2r(z+zint))
  return(rconfint)
}
#' @export
#' @describeIn correlation-tools computes the significance of the difference between two correlation coefficients
compcorr<-function(r1,r2,n1,n2){
  zval<-abs(r2z(r1)-r2z(r2)) / sqrt((1/(n1-3)) + (1/(n2-3)))
  pval<-min(1,pnorm(abs(zval),lower.tail=F)*2)
  return(structure(list(zscore=zval,pvalue=pval),class="compcorr"))
}

print.compcorr<-function(x,...){
  cat("Two-tailed Z-test for the difference between two correlation coefficients.",
      "\nZ =",x$zscore,"\np =",x$pvalue,"\n")
}

#' Compute a minimally biased average of correlation values
#'
#' This function computes a minimally biased average of correlation values.
#' This is needed because simple averaging of correlations is negatively biased,
#' and the often used z-transformation method of averaging correlations is positively biased.
#' The algorithm was developed by Olkin & Pratt (1958).
#'
#' @param r a vector containing correlation values
#' @param n a single value or vector containing sample sizes
#' @param wts Character. How should the correlations be weighted?
#' \code{none} leads to no weighting, \code{n} weights by sample size, \code{df} weights by sample size minus one.
#' @param type Character. Determines which averaging algorithm to use, with "OP5" being the most accurate.
#' @param na.rm Logical. Should missing values be removed?
#'
#' @return An average correlation.
#' @name cormean
#' @export
#'
#' @references
#' Olkin, I., & Pratt, J. (1958). Unbiased estimation of certain correlation coefficients.
#' The Annals of Mathematical Statistics, 29. https://doi.org/10.1214/aoms/1177706717
#'
#' Shieh, G. (2010). Estimation of the simple correlation coefficient. Behavior Research Methods,
#' 42(4), 906-917. https://doi.org/10.3758/BRM.42.4.906
#'
#' @examples
#' cormean(c(0,.3,.5),c(30,30,60))
cormean<-function(r,n,wts=c("none","n","df"),type=c("OP5","OPK","OP2"),na.rm=F){
  type<-match.arg(type)
  wts<-match.arg(wts)

  if(na.rm){
    missing<-which(is.na(r) | is.na(n))
    if(length(missing)>0){
      r<-r[-missing]
      n<-n[-missing]
    }
  }
  weight<-list(rep(1,times=length(n)),n,n-1)[[1+(wts=="n")+2*(wts=="df")]]
  if(length(r)!=length(n)){
    stop("Length of r and n not equal!")
  }

  if(type=="OP5"){
    sizevec<-unique(n)
    gammalist<-sapply(sizevec,function(nr) (gamma(.5+1:5)^2 * gamma(nr/2-1))/
                        (gamma(.5)^2 * gamma(nr/2-1+1:5)))
    rmean<-weighted.mean(x= sapply(seq_along(r),
                                   function(i)
                                     r[i]*(1+ sum(gammalist[,match(n[i],sizevec)] *
                                                    (1-r[i]^2)^(1:5)/factorial(1:5)))),
                         w= weight)
  }else if(type=="OPK"){
    rmean<-weighted.mean(x= r*(1+(1-r^2)/(2*(n-(9*sqrt(2)-7)/2))),
                         w= weight)
  }else if(type=="OP2"){
    rmean<-weighted.mean(x= r*(1+ (1-r^2)/(2*(n-2)) +
                                 (9*(1-r^2)^2)/(8*n*(n-2))),
                         w= weight)
  }
  return(rmean)
}

#' Partial correlation
#' Compute the correlation between x and y while controlling for z.
#' @param x,y,z x and y will be correlated while controlling for z
#' @param use optional character indicating how to handle missing values (see \link{cor})
#' @export
#' @examples
#' partial.cor(mtcars$mpg,mtcars$cyl,mtcars$disp)
partial.cor<-function(x,y,z,use=c("complete.obs","everything")){
  use<-match.arg(use)
  if(use=="complete.obs"){
    key<- !is.na(x) & !is.na(y) & !is.na(z)
    x<-x[key]
    y<-y[key]
    z<-z[key]
  }
  xy<-cor(x,y)
  xz<-cor(x,z)
  yz<-cor(y,z)
  return((xy-xz*yz)/sqrt((1-xz^2)*(1-yz^2)))
}

#ref: https://www.tse-fr.eu/sites/default/files/medias/stories/SEMIN_09_10/STATISTIQUE/croux.pdf
# devlin, 1975
cor.influence<-function(x,y){
  x<-x-mean(x)
  y<-y-mean(y)
  x*y-(x^2+y^2)/2*cor(x,y)
}


#' Multiple correlation
#' Computes the \href{https://en.wikipedia.org/wiki/Multiple_correlation}{multiple correlation coefficient}
#' of variables in \code{ymat} with the variable \code{x}
#' @param x Either a matrix of variables whose multiple correlation with each other is to be estimated; or a vector of which the multiple correlation with variables in \code{ymat} is to be estimated
#' @param ymat a matrix or data.frame of variables of which the multiple correlation with \code{x} is to be estimated
#' @param use optional character indicating how to handle missing values (see \link{cor})
#'
#' @return The multiple correlation coefficient
#' @export
#' @seealso https://www.personality-project.org/r/book/chapter5.pdf
#'
#' @examples
#' multiple.cor(mtcars[,1],mtcars[,2:4])
multiple.cor<-function(x,ymat,use="everything"){
  if(missing(ymat)){
    cv<-cor(x,use=use)
    corvec<-numeric(ncol(x))
    for(i in seq_along(corvec)){
      gfvec<-cv[(1:nrow(cv))[-i],i]
      dcm<-cv[(1:nrow(cv))[-i],(1:ncol(cv))[-i]]
      rsq<-t(gfvec) %*% solve(dcm) %*% gfvec
      corvec[i]<-sqrt(as.vector(rsq))
    }
    names(corvec)<-colnames(cv)
    return(corvec)
  }else{
    cv<-cor(cbind(x,ymat),use=use)
    gfvec<-cv[2:nrow(cv),1]
    dcm<-cv[2:nrow(cv),2:ncol(cv)]
    rsq<-t(gfvec) %*% solve(dcm) %*% gfvec
    return(sqrt(as.vector(rsq)))
  }
}

#negative reliability in split-half aat occurs when the subtracted components correlate too positively with each other



#' Covariance matrix computation with multiple imputation
#'
#' This function computes a covariance matrix from data with some values missing at random.
#' The code was written by Eric from StackExchange. https://stats.stackexchange.com/questions/182718/ml-covariance-estimation-from-expectation-maximization-with-missing-data
#' @param dat_missing a matrix with missing values
#' @param iters the number of iterations to perform to estimate missing values
#' @references Beale, E. M. L., & Little, R. J. A.. (1975). Missing Values in Multivariate Analysis. Journal of the Royal Statistical Society. Series B (methodological), 37(1), 129–145.
#' @export
#' @examples
#' # make data with missing values
#' missing_mtcars <- mtcars
#' for(i in 1:20){
#'   missing_mtcars[sample(1:nrow(mtcars),1),sample(1:ncol(mtcars),1)]<-NA
#' }
#' covmat<-covEM(as.matrix(missing_mtcars))$sigma
#' calpha(covmat)
covEM<-function(dat_missing,iters=1000){
  if(!anyNA(dat_missing)){
    return(list(sigma=cov(dat_missing),data=dat_missing))
  }

  n <- nrow(dat_missing)
  nvar <- ncol(dat_missing)
  is_na <- apply(dat_missing,2,is.na) # index if NAs

  dat_impute <- dat_missing # data matrix for imputation
  # set initial estimates to means from available data
  for(i in 1:ncol(dat_impute)){
    dat_impute[is_na[,i],i] <- colMeans(dat_missing,na.rm = TRUE)[i]
  }

  # starting values for EM
  means <- colMeans(dat_impute)
  # NOTE: multiplying by (nrow-1)/(nrow) to get ML estimate
  sigma <- cov(dat_impute)*(nrow(dat_impute)-1)/nrow(dat_impute)

  # carry out EM over 100 iterations
  for(j in 1:iters){
    bias <- matrix(0,nvar,nvar)
    for(i in 1:n){
      row_dat <- dat_missing[i,]
      miss <- which(is.na(row_dat))
      if(length(miss)>0){
        bias[miss,miss] <- bias[miss,miss] + sigma[miss,miss] -
          sigma[miss,-miss] %*% solve(sigma[-miss,-miss]) %*% sigma[-miss,miss]
        dat_impute[i,miss] <- means[miss] +
          (sigma[miss,-miss] %*% solve(sigma[-miss,-miss])) %*%
          (row_dat[-miss]-means[-miss])
      }
    }
    # get updated means and covariance matrix
    means <- colMeans(dat_impute)
    biased_sigma <- cov(dat_impute)*(n-1)/n
    # correct for bias in covariance matrix
    sigma <- biased_sigma + bias/n
  }
  return(list(sigma=sigma,data=dat_impute))
}


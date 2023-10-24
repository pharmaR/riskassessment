
#' Compute psychological experiment reliability
#' @description This function can be used to compute an exact reliability score for a psychological task whose results involve a difference score.
#' The resulting intraclass correlation coefficient is equivalent to the average all possible split-half reliability scores.
#' It ranges from -1 to 1, with -1 implying that all variance in the data is explained by within-subjects variability,
#' 1 implying that all variance is explained by between-subjects variability,
#' and 0 implying that within-subjects and between-subjects variability contribute equally to the total variance in the sample.
#' @param ds a long-format data.frame
#' @param subjvar name of the subject variable
#' @param formula a formula predicting the participant's reaction time using trial-level variables such as movement direction and stimulus category
#' @param aatterm a string denoting the term in the formula that contains the participant's approach bias
#'
#' @return a qreliability object, containing the reliability coefficient,
#' and a data.frame with participants' bias scores and score variance.
#'
#' Please note that the valence of the bias scores may or may not correspond with
#' approach and avoidance. If you plan to use these scores in your analyses,
#' always verify that they are in the right direction by correlating them with
#' independently calculated bias scores, for example using \code{aat_compute()}.
#'
#' @export
#' @author Sercan Kahveci
#' @examples
#' # Double-difference score reliability
#' q_reliability(ds=erotica,subjvar="subject",
#'               formula= RT ~ is_pull * is_target, aatterm = "is_pull:is_target")
#'
#' # Single-difference reliability for target stimuli
#' q_reliability(ds=erotica[erotica$is_target ==1,],subjvar="subject",
#'               formula= RT ~ is_pull, aatterm = "is_pull")
#'
#' # Reliability of the mean reaction time of approaching target stimuli (no difference score)
#' q_reliability(ds=erotica[erotica$is_target ==1 & erotica$is_pull ==1,],subjvar="subject",
#'               formula= RT ~ 1, aatterm = "1")
#'
q_reliability<-function(ds,subjvar,formula,aatterm=NA){
  # argument checks
  cols<-c(subjvar,as.character(attr(terms(formula),"variables"))[-1])
  stopifnot(all(cols %in% colnames(ds)))
  ds<-ds[apply(!is.na(ds[,cols]),MARGIN=1,FUN=all),]
  if(aatterm=="1"){ aatterm<-NA }

  # functional part
  coefs<-data.frame(pp=unique(ds[[subjvar]]),ab=NA,var=NA)
  for(u in 1:nrow(coefs)){
    iterset<-ds[ds[[subjvar]]==coefs[u,]$pp,]
    mod<-lm(formula,data=iterset)
    coefs[u,]$ab <- -coef(mod)[ifelse(is.na(aatterm),length(coef(mod)),aatterm)]
    coefs[u,]$var <- (diag(vcov(mod)))[ifelse(is.na(aatterm),length(coef(mod)),aatterm)] # squared standard error
  }

  bv<-var(coefs$ab,na.rm=TRUE)
  wv<-mean(coefs$var,na.rm=TRUE)
  q<-(bv-wv)/(bv)

  return(structure(list(q=q,coefs=coefs),class="qreliability"))
}

#' @rdname q_reliability
#' @param splitvars Vector of column names over which to split the data
#' to compute difference scores. This can be used to compute the
#' reliability of single, double, or even triple difference scores.
#' @param rtvar Column name of the variable containing reaction times
#' @param dscore If true, reliability will be computed for a difference score
#' that is divided by the subject's standard deviation (as in D-scores)
#' @param na.rm If true, remove rows with missing values from the data
#' @export
#' @examples
#' q_reliability2(ds=erotica,subjvar="subject",
#'               splitvars=c("is_pull", "is_target"),rtvar="RT")
q_reliability2<-function(ds,subjvar,splitvars,rtvar,dscore=F,na.rm=F){
  #remove missing
  if(na.rm){
    ds<-ds[,c(subjvar,rtvar,splitvars)]
    ds<-ds[rowSums(is.na(ds))<1,]
  }

  #divide RTs by person-specific SD to make it possible to compute D-score by simply
  # doing a double mean difference
  if(dscore){
    sds<-tapply(ds[[rtvar]],ds[[subjvar]],sd)
    ds[[rtvar]]<-ds[[rtvar]]/sds[as.character(ds[[subjvar]])]
  }

  #scores
  sc<-tapply(X=ds[[rtvar]],
             INDEX = ds[,c(splitvars,subjvar)],
             FUN=mean)
  if(!all(dim(sc)[-length(dim(sc))]==2)){
    stop("Not all split variables consist of only 2 values.")
  }
  while(length(dim(sc))>1){
    sc<-arrextract(sc,1,1)-arrextract(sc,1,2)
  }

  #variances
  variances<-tapply(X=ds[[rtvar]],
                    INDEX = ds[,c(splitvars,subjvar)],
                    FUN=function(x){var(x)/length(x)}) %>%
    apply(X=.,MARGIN=length(dim(.)),sum)

  #remove missing
  unmissing<-which(!is.na(variances) & !is.na(sc))
  sc<-sc[unmissing]
  variances<-variances[unmissing]

  #rel
  bv<-var(sc)
  wv<-mean(variances)
  rel<-(bv-wv)/(bv)

  #output
  return(structure(list(q=rel,coefs=data.frame(pp=names(sc),
                                               bias=sc,
                                               var=variances)),
                   class="qreliability"))
}

#borrowed from stackoverflow
arrextract <- function(A, .dim, .value){
  idx.list <- lapply(dim(A), seq_len)
  idx.list[[.dim]] <- .value
  do.call(`[`, c(list(A), idx.list))
}

#' @export
#' @rdname q_reliability
#' @param x a \code{qreliability} object
#' @param ... Other arguments passed to the generic \code{print} and \code{plot} functions.
print.qreliability<-function(x,...){
  cat("q = ",x$q,"\n",sep="")
}

#' @export
#' @rdname q_reliability
#' @param x a \code{qreliability} object
#' @param ... Other arguments passed to the generic \code{print} and \code{plot} functions.
plot.qreliability<-function(x,...){
  bv<-var(x$coefs$ab,na.rm=TRUE) / nrow(x$coefs)*1.96 *2
  wv<-mean(x$coefs$var,na.rm=TRUE) / nrow(x$coefs)*1.96 *2
  plotset<-data.frame(x=mean(x$coefs$ab) + cos(0:100 / 100 * 2*pi)*bv * 1/2*sqrt(2) - sin(0:100 / 100 * 2*pi)*wv * 1/2*sqrt(2),
                      y=mean(x$coefs$ab) + cos(0:100 / 100 * 2*pi)*bv * 1/2*sqrt(2) + sin(0:100 / 100 * 2*pi)*wv * 1/2*sqrt(2))
  plot(plotset$x,plotset$y,type="l",main=paste0("Reliability\n","q = ",round(x$q,digits=2)),xlab="Participants' scores",ylab="Participants' scores")
  points(x$coefs$ab,x$coefs$ab)
  dispval<-(bv+wv)/100
  plotset<-data.frame(xstart=c(x$coefs$ab+dispval,x$coefs$ab-dispval),
                      ystart=c(x$coefs$ab-dispval,x$coefs$ab+dispval),
                      xend=c(x$coefs$ab+sqrt(x$coefs$var) *1/2*sqrt(2),
                             x$coefs$ab-sqrt(x$coefs$var) *1/2*sqrt(2)),
                      yend=c(x$coefs$ab-sqrt(x$coefs$var) *1/2*sqrt(2),
                             x$coefs$ab+sqrt(x$coefs$var) *1/2*sqrt(2)))
  segments(plotset$xstart,plotset$ystart,plotset$xend,plotset$yend)
}

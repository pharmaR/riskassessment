# splithalf engine ####

#multicore splithalf
#' @title Compute the bootstrapped split-half reliability for approach-avoidance task data
#' @description Compute bootstrapped split-half reliability for approach-avoidance task data.
#' @param ds a longformat data.frame
#' @param subjvar Quoted name of the participant identifier column
#' @param pullvar Quoted name of the column indicating pull trials.
#' Pull trials should either be represented by 1, or by the second level of a factor.
#' @param targetvar Name of the column indicating trials featuring the target stimulus.
#' Target stimuli should either be represented by 1, or by the second level of a factor.
#' @param rtvar Name of the reaction time column.
#' @param stratvars Names of additional variables to stratify splits by.
#' @param iters Total number of desired iterations. At least 6000 are recommended for reasonable estimates.
#' @param algorithm Function (without brackets or quotes) to be used to compute AAT scores. See \link{Algorithms} for a list of usable algorithms.
#' @param trialdropfunc Function (without brackets or quotes) to be used to exclude outlying trials in each half.
#' The way you handle outliers for the reliability computation should mimic the way you do it in your regular analyses.
#' It is recommended to exclude outlying trials when computing AAT scores using the mean double-dfference scores and regression scoring approaches,
#' but not when using d-scores or median double-difference scores.
#' \itemize{
#' \item \code{prune_nothing} excludes no trials (default)
#' \item \code{trial_prune_grubbs} applies a Grubbs' test to the data, removing one outlier at a time until the test is no longer significant.
#' \item \code{trial_prune_3SD} excludes trials deviating more than 3SD from the mean per participant.
#' \item \code{trial_prune_SD_dropcases} removes trials deviating more than a specific number of standard deviations from the participant's mean,
#' and removes participants with an excessive percentage of outliers.
#' Required arguments:
#' \itemize{
#' \item \code{trialsd} - trials deviating more than \code{trialsd} standard deviations from the participant's mean are excluded (optional; default is 3)
#' \item \code{maxoutliers} - participants with a higher percentage of outliers are removed from the data. (optional; default is .15)
#' }
#' \item \code{trial_recode_SD} recodes outlying reaction times to the nearest non-outlying value,
#' with outliers defined as reaction times deviating more than a certain number of standard deviations from the participant's mean. Required argument:
#' \itemize{
#' \item \code{trialsd} - trials deviating more than this many standard deviations from the mean are classified as outliers.
#' }
#' \item \code{trial_prune_percent_subject} and \code{trial_prune_percent_sample} remove trials below and/or above certain percentiles,
#' on a subject-by-subject basis or sample-wide, respectively. The following arguments are available:
#' \itemize{
#' \item \code{lowerpercent} and \code{uppperpercent} (optional; defaults are .01 and .99).
#' }
#' }
#' @param errortrialfunc Function (without brackets or quotes) to apply to an error trial.
#'
#' \itemize{
#' \item \code{prune_nothing} removes no errors (default).
#' \item \code{error_replace_blockmeanplus} replaces error trial reaction times with the block mean, plus an arbitrary extra quantity.
#' If used, the following additional arguments are required:
#' \itemize{
#' \item \code{blockvar} - Quoted name of the block variable (mandatory)
#' \item \code{errorvar} - Quoted name of the error variable, where errors are 1 or TRUE and correct trials are 0 or FALSE (mandatory)
#' \item \code{errorbonus} - Amount to add to the reaction time of error trials. Default is 0.6 (recommended by \code{Greenwald, Nosek, & Banaji, 2003})
#' }
#' \item \code{error_prune_dropcases} removes errors and drops participants if they have more errors than a given percentage. The following arguments are available:
#' \itemize{
#' \item \code{errorvar} - Quoted name of the error variable, where errors are 1 or TRUE and correct trials are 0 or FALSE (mandatory)
#' \item \code{maxerrors} - participants with a higher percentage of errors are excluded from the dataset. Default is .15.
#' }
#' }
#' @param casedropfunc Function (without brackets or quotes) to be used to exclude outlying participant scores in each half.
#' The way you handle outliers here should mimic the way you do it in your regular analyses.
#' \itemize{
#' \item \code{prune_nothing} excludes no participants (default)
#' \item \code{case_prune_3SD} excludes participants deviating more than 3SD from the sample mean.
#' }
#' @param plot Create a scatterplot of the AAT scores computed from each half of the data from the last iteration.
#' This is highly recommended, as it helps to identify outliers that can inflate or diminish the reliability.
#' @param include.raw logical indicating whether raw split-half data should be included in the output object.
#' @param parallel If TRUE (default), will use parallel computing to compute results faster.
#' If a doParallel backend has not been registered beforehand,
#' this function will register a cluster and stop it after finishing, which takes some extra time.
#' @param ... Other arguments, to be passed on to the algorithm or outlier rejection functions (see arguments above)
#'
#' @return A list, containing the mean bootstrapped split-half reliability, bootstrapped 95% confidence intervals,
#' a list of data.frames used over each iteration, and a vector containing the split-half reliability of each iteration.
#'
#' @author Sercan Kahveci
#' @seealso \link{q_reliability}
#' @examples
#' split <- aat_splithalf(ds=erotica[erotica$is_irrelevant==0,],
#'                        subjvar="subject", pullvar="is_pull", targetvar="is_target",
#'                        rtvar="RT", stratvars="stimuluscode", iters=10,
#'                        trialdropfunc="trial_prune_3SD",
#'                        casedropfunc="case_prune_3SD", algorithm="aat_dscore",
#'                        plot=FALSE, parallel=FALSE)
#'
#' print(split)
#' #Mean reliability: 0.521959
#' #Spearman-Brown-corrected r: 0.6859041
#' #95%CI: [0.4167018, 0.6172474]
#'
#' plot(split)
#'
#' \donttest{
#' #Regression Splithalf
#' aat_splithalf(ds=erotica[erotica$is_irrelevant==0,],
#'               subjvar="subject", pullvar="is_pull", targetvar="is_target",
#'               rtvar="RT", iters=10, trialdropfunc="trial_prune_3SD",
#'               casedropfunc="case_prune_3SD", algorithm="aat_regression",
#'               formula = RT ~ is_pull * is_target, aatterm = "is_pull:is_target",
#'               plot=FALSE, parallel=FALSE)
#' #Mean reliability: 0.5313939
#' #Spearman-Brown-corrected r: 0.6940003
#' #95%CI: [0.2687186, 0.6749176]
#' }
#' @export
aat_splithalf<-function(ds,subjvar,pullvar,targetvar=NULL,rtvar,stratvars=NULL,iters,
                        algorithm=c("aat_doublemeandiff","aat_doublemediandiff",
                                    "aat_dscore","aat_dscore_multiblock",
                                    "aat_regression","aat_standardregression",
                                    "aat_singlemeandiff","aat_singlemediandiff"),
                        trialdropfunc=c("prune_nothing","trial_prune_3SD","trial_prune_3MAD",
                                        "trial_prune_SD_dropcases",
                                        "trial_recode_SD","trial_prune_percent_subject",
                                        "trial_prune_percent_sample","trial_prune_grubbs"),
                        errortrialfunc=c("prune_nothing","error_replace_blockmeanplus",
                                         "error_prune_dropcases"),
                        casedropfunc=c("prune_nothing","case_prune_3SD"),
                        plot=TRUE,include.raw=FALSE,parallel=TRUE,...){
  packs<-c("magrittr","dplyr","AATtools")

  #Handle arguments
  args<-list(...)
  algorithm<-match.arg(algorithm)
  if(!(algorithm %in% c("aat_singlemeandiff","aat_singlemediandiff",
                        "aat_regression","aat_standardregression")) & is.null(targetvar)){
    stop("Argument targetvar missing but required for algorithm!")
  }
  trialdropfunc<-match.arg(trialdropfunc)
  casedropfunc<-match.arg(casedropfunc)
  errortrialfunc<-match.arg(errortrialfunc)
  errorpenalizefunc<-ifelse(errortrialfunc=="error_replace_blockmeanplus",errortrialfunc,"prune_nothing")
  errorremovefunc<-ifelse(errortrialfunc=="error_replace_blockmeanplus","prune_nothing",errortrialfunc)
  if(errortrialfunc=="error_replace_blockmeanplus"){
    stopifnot(!is.null(args$blockvar),!is.null(args$errorvar))
    if(is.null(args$errorbonus)){ args$errorbonus<- 0.6 }
    if(is.null(args$blockvar)){ args$blockvar<- 0 }
    if(is.null(args$errorvar)){ args$errorvar<- 0 }
  }

  stopifnot(!(algorithm=="aat_dscore_multiblock" & is.null(args$blockvar)))
  if(algorithm %in% c("aat_regression","aat_standardregression")){
    if(!("formula" %in% names(args))){
      args$formula<-as.formula(paste0(rtvar,"~",pullvar,"*",targetvar))
      warning("No formula provided. Defaulting to formula ",form2char(args$formula))
    }else if(is.character(args$formula)){
      args$formula<-as.formula(args$formula)
    }
    if(!("aatterm" %in% names(args))){
      args$aatterm<-paste0(pullvar,":",targetvar)
      warning("No AAT-term provided. Defaulting to AAT-term ",args$aatterm)
    }
  }
  ds<-do.call(aat_preparedata,c(list(ds=ds,subjvar=subjvar,pullvar=pullvar,targetvar=targetvar,
                                     rtvar=rtvar,stratvars=stratvars),args))

  #Prepare the cluster
  if(parallel){
    `%dofunc%` <- `%dopar%`
    hasCluster<-getDoParRegistered()
    if(!hasCluster){
      cluster<-makeCluster(getOption("AATtools.workers"))
      registerDoParallel(cluster)
      on.exit(unregisterDoParallel(cluster))
    }
  }else{
    `%dofunc%` <- `%do%`
  }

  #splithalf loop
  results<-
    foreach(iter = seq_len(iters), .packages=packs) %dofunc% {
      #Split data
      # if(is.null(targetvar)){
      #   iterds<-ds%>%group_by(!! sym(subjvar), !! sym(pullvar))%>%
      #     mutate(key=sample(n())%%2)%>%ungroup()
      # }else{
      #   # iterds<-ds%>%group_by(!! sym(subjvar), !! sym(pullvar), !! sym(targetvar))%>%
      #   #   mutate(key=sample(n())%%2)%>%ungroup()
      #
      #   h<-tapply(seq_len(nrow(ds)),ds[c(subjvar,pullvar,targetvar)],
      #             function(x){sample(x,size=round(length(x)/2))})%>%unlist()
      #   iterds<-ds
      #   iterds$key<-0
      #   iterds$key[h]<-1
      # }

      #Split data
      iterds<-ds
      iterds$key<-datasplitter(iterds[,c(subjvar,pullvar,targetvar,stratvars)])

      #Handle error removal
      iterds<-do.call(errorremovefunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))
      #Handle outlying trials
      iterds<-do.call(trialdropfunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))
      #Handle error penalization
      iterds<-do.call(errorpenalizefunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))
      #intermediate prune of empty cases
      iterds<-drop_empty_cases(iterds,subjvar)

      # abds<-do.call(algorithm,c(list(iterds=iterds,subjvar=subjvar,pullvar=pullvar,
      #                                targetvar=targetvar,rtvar=rtvar),args))

      #Compute AB
      half0set<-iterds[which(iterds$key==0),]
      half1set<-iterds[which(iterds$key==1),]
      abds<-merge(
        do.call(algorithm,c(list(ds=half0set,subjvar=subjvar,pullvar=pullvar,
                                 targetvar=targetvar,rtvar=rtvar),args)),
        do.call(algorithm,c(list(ds=half1set,subjvar=subjvar,pullvar=pullvar,
                                 targetvar=targetvar,rtvar=rtvar),args)),
        by=subjvar,suffixes=c("half0","half1"))

      #Remove outlying participants
      abds<-do.call(casedropfunc,list(ds=abds))

      #Compute reliability
      currcorr<-cor(abds$abhalf0,abds$abhalf1,use="complete.obs")
      frcorr<-FlanaganRulonStandard(abds$abhalf0,abds$abhalf1)
      rjcorr<-RajuStandard(abds$abhalf0,abds$abhalf1,mean(iterds$key))

      #produce output
      out<-list(corr=currcorr,frcorr=frcorr,rjcorr=rjcorr,abds=abds)
      if(include.raw){out$rawdata<-iterds}
      out
    }

  #extract coefs from output
  rjcors<-sapply(results,FUN=function(x){x$rjcorr}) %>% lim(-.9999,.9999)
  cors<-sapply(results,FUN=function(x){x$corr})
  sbcors<-SpearmanBrown(lim(cors,-.9999,.9999),fix.negative="none") %>% lim(-.9999,.9999)
  frcorrs<-sapply(results,FUN=function(x){x$frcorr})  %>% lim(-.9999,.9999)

  #get  sample sizes (for averaging and significance testing)
  counts<-sapply(results,function(x){ sum(!is.na(x$abds$abhalf0) & !is.na(x$abds$abhalf1)) })
  avg_n<-mean(counts)

  #sort the cors
  ordering<-order(rjcors)
  rjcors<-rjcors[ordering]
  cors<-cors[ordering]
  sbcors<-sbcors[ordering]
  frcorrs<-frcorrs[ordering]
  counts<-counts[ordering]

  #assemble output
  output<-list(uncorrected=list(r=cormean(cors,counts),
                                lowerci=quantile(cors,probs=.025),
                                upperci=quantile(cors,probs=.975),
                                pval=r2p(cormean(cors,counts),avg_n),
                                itercors=cors),
               spearmanbrown=list(r=cormean(sbcors,counts),
                                  lowerci=quantile(sbcors,probs=.025),
                                  upperci=quantile(sbcors,probs=.975),
                                  pval=r2p(cormean(sbcors,counts),avg_n),
                                  itercors=sbcors),
               flanaganrulon=list(r=cormean(frcorrs,counts),
                                  lowerci=quantile(x=frcorrs,probs=.025),
                                  upperci=quantile(x=frcorrs,probs=.975),
                                  pval=r2p(cormean(frcorrs,counts),avg_n),
                                  itercors=frcorrs),
               raju=list(r=cormean(rjcors,counts),
                         lowerci=quantile(x=rjcors,probs=.025),
                         upperci=quantile(x=rjcors,probs=.975),
                         pval=r2p(cormean(rjcors,counts),avg_n),
                         itercors=rjcors),
               avg_n=avg_n,
               ordering=ordering,
               parameters=c(list(ds=ds,
                                 subjvar=subjvar,
                                 pullvar=pullvar,
                                 targetvar=targetvar,
                                 rtvar=rtvar,
                                 iters=iters,
                                 algorithm=algorithm,
                                 trialdropfunc=trialdropfunc,
                                 errortrialfunc=errortrialfunc,
                                 casedropfunc=casedropfunc),
                            args),
               iterdata=lapply(results,function(x){ x$abds })[ordering]) %>%
    structure(class = "aat_splithalf")

  #include raw data if asked to (disabled by default, takes a lot of space)
  if(include.raw){
    output$rawiterdata<-lapply(results,function(x){ x$rawdata })[ordering]
  }

  #plot if asked to (default)
  if(plot){ plot(output) }

  #return output
  return(output)
}

#' @param coef Optional character argument,
#' indicating which reliability coefficient should be printed.
#' Defaults to Raju's beta.
#' @details The calculated split-half coefficients are described in Warrens (2016).
#' @references Warrens, M. J. (2016). A comparison of reliability coefficients for
#' psychometric tests that consist of two parts.
#' Advances in Data Analysis and Classification, 10(1), 71-84.
#' @export
#' @rdname aat_splithalf
print.aat_splithalf<-function(x,coef=c("SpearmanBrown","Raju","FlanaganRulon"),...){
  coef<-match.arg(coef)
  if(coef=="Raju"){
    coefstr<-paste0("\nFull-length reliability (Raju's beta):\n",
                    "beta (",format(x$avg_n),") = ",mf(x$raju$r),
                    ", 95%CI [", mf(x$raju$lowerci), ", ", mf(x$raju$upperci),"]",
                    ", p = ",mf(x$raju$pval,digits=3),"\n")
  }else if(coef=="FlanaganRulon"){
    coefstr<-paste0("\nFull-length reliability (Flanagan-Rulon coefficient):\n",
                    "FR (",format(x$avg_n),") = ",mf(x$flanaganrulon$r),
                    ", 95%CI [", mf(x$flanaganrulon$lowerci), ", ", mf(x$flanaganrulon$upperci),"]",
                    ", p = ",mf(x$flanaganrulon$pval,digits=3),"\n")
  }else if(coef=="SpearmanBrown"){
    coefstr<-paste0("\nFull-length reliability (Spearman-Brown coefficient):\n",
                    "SB (",format(x$avg_n),") = ",mf(x$spearmanbrown$r),
                    ", 95%CI [", mf(x$spearmanbrown$lowerci), ", ", mf(x$spearmanbrown$upperci),"]",
                    ", p = ",mf(x$spearmanbrown$pval,digits=3),"\n")
  }
  cat(coefstr,
      "\nUncorrected, average split-half correlation:\n",
      "r (",format(x$avg_n),") = ",mf(x$uncorrected$r),
      ", 95%CI [", mf(x$uncorrected$lowerci), ", ", mf(x$uncorrected$upperci),"]",
      ", p = ",mf(x$uncorrected$pval,digits=3),"\n",
      sep="")
}

#' @title Plot split-half scatterplots
#'
#' @param x an \code{aat_splithalf} object
#' @param type Character argument indicating which iteration should be chosen. Must be an abbreviation of
#' \code{"median"} (default), \code{"minimum"}, \code{"maximum"}, or \code{"random"}.
#'
#' @export
#' @rdname aat_splithalf
plot.aat_splithalf<-function(x,type=c("median","minimum","maximum","random"),...){
  type<-match.arg(type)
  if(type=="median"){
    title<-"Split-half Scatterplot for Iteration with Median Reliability"
    idx<-ceiling(x$parameters$iters/2)
  }else if(type=="minimum"){
    title<-"Split-half Scatterplot for Iteration with the Lowest Reliability"
    idx<-1
  }else if(type=="maximum"){
    title<-"Split-half Scatterplot for Iteration with the Highest Reliability"
    idx<-x$parameters$iters
  }else if(type=="random"){
    title<-"Split-half Scatterplot for Random Iteration"
    idx<-sample(1:x$parameters$iters,1)
  }
  abds<-x$iterdata[[idx]]
  plot(abds$abhalf0,abds$abhalf1,pch=20,main=
         paste0(title,"\n(Uncorrected r = ", round(x$uncorrected$itercors[idx],digits=2),")"),
       xlab="Half 1 computed bias",ylab="Half 2 computed bias")
  text(abds$abhalf0,abds$abhalf1,abds[,1],cex= 0.7, pos=3, offset=0.3)
}








#' Compute stimulus-specific bias scores
#' Computes mean single-difference scores (push - pull) for each stimulus.
#'
#' @param ds the \code{data.frame} to use
#' @param subjvar Name of the subject-identifying variable
#' @param stimvar Name of the stimulus-identifying variable
#' @param pullvar Name of the movement-direction identifying variable
#' @param targetvar Optional. Name of the stimulus-category identifying variable
#' @param rtvar Name of the reaction-time identifying variable
#' @param aggfunc The function with which to aggregate the RTs before computing difference scores. Defaults to mean but can be changed to median.
#' @param iters If there are missing values (which is almost inevitable) then
#' multiple imputation will be used to complete the covariance matrix - this argument sets
#' the number of multiple imputations to be used.
#'
#' @return Exports a \code{list} containing
#' a \code{data.frame} with stimulus-specific bias scores, indicated in the column names,
#' a covariance matrix of that same data, and
#' a \code{data.frame} indicating to which stimulus category each stimulus belongs.
#' @export
#'
#' @examples
#' ds<-aat_simulate(biasfx_jitter=40,nstims=16)
#' ds$stim<-paste0(ds$stim,"-",ds$is_target)
#' aat_stimulusscores(ds,"subj","stim","is_pull","is_target","rt")
aat_stimulusscores<-function(ds,subjvar,stimvar,pullvar,targetvar=NULL,rtvar,aggfunc=c("mean","median"),iters=5){
  ds<-aat_preparedata(ds,subjvar=subjvar,pullvar=pullvar,stimvar=stimvar,targetvar=targetvar,rtvar=rtvar)

  pps<-unique(ds[[subjvar]])
  stims<-unique(ds[[stimvar]])

  if(!is.null(targetvar)){
    stimcats<-distinct(ds[c(stimvar,targetvar)]) %>% setNames(c("stim","cat"))
  }else{
    stimcats<-data.frame(stim=stims,cat=0,stringsAsFactors=F)
  }

  aggfunc<-match.arg(aggfunc)
  if(aggfunc=="median"){
    scorefunc<-aat_singlemediandiff
  }else{
    scorefunc<-aat_singlemeandiff
  }

  biases<-list()
  for(u in seq_along(pps)){
    biases[[u]]<-
      do.call(scorefunc,list(ds=ds[ds[[subjvar]]==pps[u],],
                                      subjvar=stimvar,pullvar=pullvar,rtvar=rtvar)) %>%
      setNames(c(stimvar,paste0("",pps[u]))) #subject-
  }
  biasset<-Reduce(function(x,y){merge(x,y,by=stimvar,all=T)},x=biases[-1],init=biases[1])
  biasmat<-t(as.matrix(biasset[,-1]))
  colnames(biasmat)<-biasset[[1]]

  unmissing<-covEM(biasmat,iters)
  covmat<-unmissing$sigma
  rownames(covmat)<-colnames(covmat)
  dataset<-unmissing$data

  out<-list(data=dataset,covmat=covmat,stimcats=stimcats)
  return(out)
}


#' Compute a dataset's reliability from its covariance matrix
#'
#' This function computes mean single-difference scores (push minus pull) for individual stimuli,
#' and computes the reliability from that information.
#' Missing values are dealt with using multiple imputation.
#'
#' When only one stimulus category is indicated, one of the commonly known reliability algorithms
#' provided with the \code{algorithm} argument is used.
#' When two stimulus categories are indicated, this function uses Lord's (1963) algorithm to
#' compute the reliability of a double mean difference score, using the algorithms in \code{algorithm}
#' to estimate the reliability of indiviau lstimulus categories.
#'
#' When one wants to compute the reliability of a double median difference score or D-score,
#' \code{aat_splithalf()} is recommended instead.
#'
#' @param ds the \code{data.frame} to use
#' @param subjvar Name of the subject-identifying variable
#' @param stimvar Name of the stimulus-identifying variable
#' @param pullvar Name of the movement-direction identifying variable
#' @param targetvar Optional. Name of the stimulus-category identifying variable
#' @param rtvar Name of the reaction-time identifying variable
#' @param aggfunc The function with which to aggregate the RTs before computing difference scores. Defaults to mean but can be changed to median.
#' @param algorithm The reliability formula to use. Defaults to Cronbach's alpha, but Guttman's Lambda-2 is recommended instead.
#' @param iters If there are missing values (which is almost inevitable) then
#' multiple imputation will be used to complete the covariance matrix - this option sets
#' the number of multiple imputations to be used.
#'
#' @return Returns an \code{aat_covreliability} object containing the reliability value
#' as well as the dataset and covariance matrix with replaced missing values. When
#' the argument \code{targetvar} is provided, the output also contains the reliability of the
#' individual stimulus categories and their intercorrelation.
#'
#' @export
#'
#' @references
#' Lord, F.Y. (1963), "Elementary Models for Measuring Change",
#' in Problems in Measuring Change, C.W. Harris, ed.. Madison. Wisconsin:
#' University of Wisconsin.
#'
#' @examples
#' #We generate a dataset with 16 stimuli in each category
#' ds<-aat_simulate(biasfx_jitter=40,nstims=16)
#' ds$stim<-paste0(ds$stim,"-",ds$is_target)
#'
#' # If Lord's formula and
#' # bootstrapped splithalf measure something similar,
#' # then the outcomes should be close to each other.
#' aat_covreliability(ds=ds,subjvar="subj",stimvar="stim",pullvar="is_pull",
#'                            targetvar="is_target",rtvar="rt")
#' aat_splithalf(ds=ds,subjvar="subj",pullvar="is_pull",targetvar="is_target",rtvar="rt",
#'               algorithm="aat_doublemeandiff",iters=100,plot=FALSE)
#'
#' #Testing reliability for single-difference scores
#' ds<-ds[ds$is_target==1,]
#' aat_covreliability(ds=ds,subjvar="subj",stimvar="stim",pullvar="is_pull",rtvar="rt")
aat_covreliability<-function(ds,subjvar,stimvar,pullvar,targetvar=NULL,rtvar,aggfunc=c("mean","median"),
                             algorithm=c("calpha","lambda2","lambda4"),iters=5){
  algorithm<-match.arg(algorithm)
  aggfunc<-match.arg(aggfunc)
  sc<-aat_stimulusscores(ds,subjvar=subjvar,stimvar=stimvar,pullvar=pullvar,targetvar=targetvar,
                         rtvar=rtvar,aggfunc=aggfunc,iters=iters)

  if(!is.null(targetvar)){
    dia<-diag(sc$covmat)
    firstcat <-which(names(dia) %in% sc$stimcats$stim[sc$stimcats$cat==0])
    secondcat<-which(names(dia) %in% sc$stimcats$stim[sc$stimcats$cat==1])
    n1<-length(firstcat )
    n2<-length(secondcat)

    r11<-do.call(algorithm,list(covmat=sc$covmat[firstcat, firstcat ]))
    r22<-do.call(algorithm,list(covmat=sc$covmat[secondcat,secondcat]))
    # r12<-cor(x=rowSums(sc$dataset[,firstcat]),
    #          y=rowSums(sc$dataset[,secondcat]))
    r12<-sum(sc$covmat[firstcat,secondcat])/sqrt(sum(sc$covmat[firstcat,firstcat])*sum(sc$covmat[secondcat,secondcat]))
    s1<-sqrt(sum(sc$covmat[firstcat,firstcat]))/n1
    s2<-sqrt(sum(sc$covmat[secondcat,secondcat]))/n2
    rel<-(s1^2*r11+s2^2*r22-2*s1*s2*r12)/
      (s1^2+s2^2-2*s1*s2*r12)
  }else{
    rel<-do.call(algorithm,list(covmat=sc$covmat))
  }

  out<-structure(list(rel=rel,data=sc$data,covmat=sc$covmat,algorithm=algorithm),
                 class="aat_covreliability")
  if(!is.null(targetvar)){
    out$components<-list(r11=r11,r22=r22,r12=r12,n1=n1,n2=n2,s1=s1,s2=s2)
  }
  return(out)
}

#' @export
#' @describeIn aat_covreliability Print an \code{aat_covreliability} object
print.aat_covreliability<-function(x,...){
  cat(sep="","r = ",mf(x$rel),
      "\nBased on ",ncol(x$data)," valid stimuli, ",
      nrow(x$data)," valid participants, and the ",
      x$algorithm," algorithm.\n")
  if(any("components"==names(x))){
    cat(sep="",
        "Reliability of stimulus category 1: r = ",mf(x$components$r11),", n = ",x$components$n1,", sd = ",mf(x$components$s1),"\n",
        "Reliability of stimulus category 2: r = ",mf(x$components$r22),", n = ",x$components$n2,", sd = ",mf(x$components$s2),"\n",
        "Category intercorrelation: r = ",mf(x$components$r12),"\n")
  }
}


#' @rdname aat_covreliability
#' @param holdout What should be removed from the data for computation of jackknife statistics?
#' "both" computes reliability when stimuli and participants are separately removed,
#' while "cross" computes  reliability when stimuli and participants are simultaneously removed.
#' @description This function computes the reliability when stimuli and participants are removed,
#' allowing for the diagnosis of potential sources of unreliability within the data.
#' @export
#' @return \code{aat_covreliability_jackknife()} returns an \code{aat_covreliability_jackknife} object,
#' containing jackknife reliability statistics. If argument \code{holdout} was set to "cross",
#' then these statistics are provided in a matrix where rows represent participants and columns represent stimuli.
#' Otherwise, they are provided in \code{data.frame}s where the stimulus or participant is represented in a column
#' alongside the associated reliability value.
#' @examples
#' hh<-aat_simulate()
#' test<-aat_covreliability_jackknife(ds=hh,subjvar="subj",stimvar="stim",pullvar="is_pull",
#'                                    targetvar="is_target",rtvar="rt",holdout="cross")
#' print(test)
#' plot(test)
aat_covreliability_jackknife<-function(ds,subjvar,stimvar,pullvar,targetvar=NULL,rtvar,
                                       algorithm=c("calpha","lambda2","lambda4"),iters=5,
                                       holdout=c("both","participant","stimulus","cross")){
  algorithm<-match.arg(algorithm)
  sc<-aat_stimulusscores(ds,subjvar=subjvar,stimvar=stimvar,pullvar=pullvar,targetvar=targetvar,
                         rtvar=rtvar,iters=iters)
  cat1<-sc$stimcats$stim[sc$stimcats$cat==0]
  cat2<-sc$stimcats$stim[sc$stimcats$cat==1]

  #declare reliability computation functions
  if(!is.null(targetvar)){
    relfinder<-function(psc){
      dia<-diag(psc)
      firstcat <-which(names(dia) %in% cat1)
      secondcat<-which(names(dia) %in% cat2)
      n1<-length(firstcat)
      n2<-length(secondcat)
      r11<-do.call(algorithm,list(covmat=psc[firstcat, firstcat ]))
      r22<-do.call(algorithm,list(covmat=psc[secondcat,secondcat]))
      r12<-sum(psc[firstcat,secondcat])/sqrt(sum(psc[firstcat,firstcat])*sum(psc[secondcat,secondcat]))
      s1<-sqrt(sum(psc[firstcat,firstcat]))/n1
      s2<-sqrt(sum(psc[secondcat,secondcat]))/n2
      rel<-(s1^2*r11+s2^2*r22-2*s1*s2*r12)/
           (s1^2+s2^2-2*s1*s2*r12)
      return(rel)
    }
  }else{
    relfinder<-function(psc){
      rel<-do.call(algorithm,list(covmat=psc))
      return(rel)
    }
  }

  output<-list(rel=relfinder(sc$covmat))

  pps<-sort(unique(ds[[subjvar]]))
  stims<-sort(unique(ds[[stimvar]]))


  if(any(c("both","participant")==holdout)){
    # Run jackknife over participants
    ppset<-data.frame(pp=pps,rel=NA)
    for(i in seq_along(ppset$pp)){
      ppset$rel[i]<-relfinder(cov(sc$data[rownames(sc$data)!=ppset$pp[i],]))
    }
    output$pps<-ppset
  }

  if(any(c("both","stimulus")==holdout)){
    #Run jackknife over stimuli
    stimset<-data.frame(stim=stims,rel=NA)
    for(i in seq_along(stimset$stim)){
      stimset$rel[i]<-relfinder(sc$covmat[rownames(sc$covmat) != stimset$stim[i], colnames(sc$covmat) != stimset$stim[i]])
    }
    output$stims<-stimset
  }

  if("cross"==holdout){
    #run jackknife over stimuli and participants simultaneously
    relmat<-matrix(NA,nrow=length(pps),ncol=length(stims))
    rownames(relmat)<-pps
    colnames(relmat)<-stims
    for(i in seq_len(nrow(relmat))){
      itercov<-cov(sc$data[rownames(sc$data) != rownames(relmat)[i],])
      for(j in seq_len(ncol(relmat))){
        relmat[i,j]<-relfinder(itercov[rownames(itercov) != colnames(relmat)[j],colnames(itercov) != colnames(relmat)[j]])
      }
    }
    output$cross<-relmat
  }

  output<-structure(c(output,list(data=sc$data,covmat=sc$covmat,algorithm=algorithm,holdout=holdout)),
                 class="aat_covreliability_jackknife")
  return(output)
}

#' @export
#' @describeIn aat_covreliability Print an \code{aat_covreliability_jackknife} object
#' @param x Object to be printed
#' @param ... Ignored
print.aat_covreliability_jackknife<-function(x, ...){
  cat("Reliability: r = ",mf(x$rel),"\n",sep="")
  if(any("pps"==names(x))){
    cmax<-which.max(x$pps$rel)
    cat("Maximum achieveable reliability is with removal of participant ",as.character(x$pps$pp[cmax]),
        ": r = ",mf(x$pps$rel[cmax]),"\n",sep="")
  }
  if(any("stims"==names(x))){
    cmax<-which.max(x$stims$rel)
    cat("Maximum achieveable reliability is with removal of stimulus ",as.character(x$stims$stim[cmax]),
        ": r = ",mf(x$stims$rel[cmax]),"\n",sep="")
  }
  if(any("cross"==names(x))){
    cmax<-which(x$cross==max(x$cross),arr.ind=T)
    cat("Maximum achieveable reliability is with removal of stimulus ",colnames(x$cross)[cmax[2]],
        " and participant ",rownames(x$cross)[cmax[1]],
        ": r = ",mf(x$cross[cmax[1],cmax[2]]),"\n",sep="")
  }
}

#' @export
#' @describeIn aat_covreliability Plot an \code{aat_covreliability_jackknife} object
plot.aat_covreliability_jackknife<-function(x, ...){
  prev.mfrow<-par("mfrow")
  ncols<-sum(c("pps", "stims","cross") %in% names(x))
  par(mfrow=c(1,ncols))

  if(any("pps"==names(x))){
    ord<-order(x$pps$rel)
    plot(range(x$pps$rel), range(ord), bty = 'n', type = 'n',main="Participants",
         xlab="Jackknife reliability",ylab="Rank")
    abline(v=x$rel,col="#00000055")
    text(x=x$pps$rel[ord],y=seq_along(x$pps$rel),label=as.character(x$pps$pp[ord]),cex=.7)

  }

  if(any("stims"==names(x))){
    ord<-order(x$stims$rel)
    plot(range(x$stims$rel), range(ord), bty = 'n', type = 'n',main="Stimuli",
         xlab="Jackknife reliability",ylab="Rank")
    abline(v=x$rel,col="#00000055")
    text(x=x$stims$rel[ord],y=seq_along(x$stims$rel),label=as.character(x$stims$stim[ord]),cex=.7)
  }

  if(any("cross"==names(x))){
    image(t(x$cross), xlab="Stimuli",ylab="Participants",axes=FALSE,main="Jackknife reliability")
    axis(2,at=(seq_len(nrow(x$cross))-1)/(nrow(x$cross)-1),labels=rownames(x$cross))
    axis(1,at=(seq_len(ncol(x$cross))-1)/(ncol(x$cross)-1),labels=colnames(x$cross))
  }
  par(mfrow=prev.mfrow)
}



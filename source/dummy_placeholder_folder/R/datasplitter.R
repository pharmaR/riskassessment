balancedrandombinary<-function(n){
  keys<-rep(c(0,1),floor(n/2))
  if(n%%2){
    keys<-c(keys,NA)
  }
  keys[sample.int(length(keys))]
}

splitsweep<-function(currsplitset){
  h<-tapply(seq_len(nrow(currsplitset)),currsplitset,function(x){
    cbind(x,balancedrandombinary(length(x)))
  },simplify=F)
  h<-do.call(rbind,h)

  currkey<-numeric(nrow(h))
  currkey[h[,1]]<-h[,2]
  currkey
}

datasplitter<-function(splitset){
  validcols<-ncol(splitset)
  key<-splitsweep(splitset)
  while(anyNA(key) & validcols>0){
    whichna<-is.na(key)
    key[whichna]<-splitsweep(as.data.frame(splitset[whichna,1:validcols]))
    validcols<-validcols-1
  }
  key[is.na(key)]<-sample( (seq_len(sum(is.na(key)))+sample(0:1,1)) %%2)
  key
}

#load data
rr<-array(NA,dim = c(108,200))
maxL<-array(NA,dim = c(108,200))
rENTR<-array(NA,dim = c(108,200))
ENTR<-array(NA,dim = c(108,200))
L<-array(NA,dim = c(108,200))

for (i in 1:108){
  'rr_downs'
  'rr_downs200'
  'rr_downs100'
  t<- paste0('~/rr_downs70',i,'.RData')
  #t<- paste0('~/arezoo/ansx',i,'.RData')  
  #t<-paste0('~/Dropbox/crqa/ansx',i,'.RData')
  load(t)
  for (j in 1:200){
    if ((length(ansx1[[j]]) > 1) & !is.na(ansx1[[j]]) ){
    
    rr[i,j] <- ansx1[[j]][["RR"]]
    maxL[i,j] <- ansx1[[j]][["maxL"]]
    rENTR[i,j] <- ansx1[[j]][["rENTR"]]
    ENTR[i,j] <- ansx1[[j]][["ENTR"]]
    L[i,j] <- ansx1[[j]][["L"]]
    
  }
}

}
save(rr,maxL,rENTR,ENTR,L,file='crqa_down70.RData')

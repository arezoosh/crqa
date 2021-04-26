library(tseriesChaos)
library(zoo)
library(akima)
library(signal)
library(gdata)
load("~/arezoo/dynamic.RData")
keep(xe_block,xm_block,sure = TRUE)
which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(TRUE,diff(x)<=0,FALSE))>0)
    }else {
      which(diff(diff(x)<=0)>0)
    }    
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)
    }
    
  }
}
load("C:/Users/sahar/Downloads/bandit_visual_eye_mouse_dynamic.RData")
keep(xe_block_bandit,xm_block_bandit,sure = TRUE)
label <- array(NA,dim = c(108,200))
delay <- array(NA,dim = c(200))
embed_dim <- array(NA,dim = c(200))
xm_block<-xm0_block
xe_block<-xe4_block
rm(xe4_block,xm0_block)
for (i in 31:108){
  for (j in 33:200){
    xm<-(xm_block[[i]][[j]][[1]])
    xe<-(xe_block[[i]][[j]][[1]])
#label[i,j]<-bandit_label[[i]][[1]][j]
#0 exploit, 1 explore

cutlength_x<-min(length(xm),length(xe))


if ((cutlength_x > 10) & (sum(diff(xm))!=0)){

min_m<-which.peaks(mutual(xm[1:cutlength_x],lag.max = 50,plot = FALSE),decreasing = TRUE)
if (length(min_m)==0){
  min_m<-which.peaks(mutual(xm[1:cutlength_x],lag.max = round(cutlength_x/2),plot = FALSE),decreasing = TRUE)
}
min_e<-which.peaks(mutual(xe[1:cutlength_x],lag.max = 50,plot = FALSE),decreasing = TRUE)
if (length(min_e)==0){
  min_e<-which.peaks(mutual(xe[1:cutlength_x],lag.max = round(cutlength_x/2),plot = FALSE),decreasing = TRUE)
}

delay[j]<-max(min_m[1],min_e[1],na.rm = TRUE)
fm_out<-(false.nearest(xm[1:cutlength_x],m=3,d=delay[j],t=0))
embed_m<-which.peaks(fm_out[1,],decreasing = TRUE)
if (length(embed_m)==0){
  embed_m<-which.min(fm_out[1,])
    }
fe_out<-(false.nearest(xe[1:cutlength_x],m=3,d=delay[j],t=0))
embed_e<-which.peaks(fe_out[1,],decreasing = TRUE)
if (length(embed_e)==0){
  embed_e<-which.min(fe_out[1,])
}
if (length(embed_e) & length(embed_m)==0){
  embed_e<-1
  embed_m<-1
}
embed_dim[j]<-max(embed_e,embed_m,na.rm = TRUE)
} 
  
  }
  save(delay,embed_dim,file=paste0('down_ansx',i,'.RData'))
  embed_dim<-array(NA,dim = 200)
  delay<-array(NA,dim = 200)
}
#reading all separate files and find mean
d<-array(NA,dim = c(32,200))
embed<-array(NA,dim = c(32,200))
for (i in 1:32){
  
  t<- paste0('~/var_ansx',i,'.RData')
  #t<- paste0('~/arezoo/ansx',i,'.RData')  
  #t<-paste0('~/Dropbox/crqa/ansx',i,'.RData')
  load(t)
  namd <- paste("delay", i, sep = "")
  assign(namd, delay)
  d[i,]<-delay
  
  nameb <- paste("embed_dim", i, sep = "")
  assign(nameb, embed_dim)
  embed[i,]<-embed_dim
  rm(ansx1)
}
mean(colMeans(embed,na.rm = TRUE))
mean(colMeans(d,na.rm = TRUE))
par(mfrow=c(1,2))
hist(d,xlab = 'delay',main = 'Histogram of delays')
hist(embed,xlab = 'embedding dimensions',main = 'Histogram of embedding dimension')

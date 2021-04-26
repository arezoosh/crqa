#load("C:/Users/sahar/Downloads/bandit_visual_eye_mouse_dynamic.RData")
load("C:/Users/sahar/Downloads/arezoo.RData")
#MFDXA
library(signal)
library(crqa)
ansx1<-array(NA,dim = 200)
#ansx2<-array(NA,dim = c(108,200))
n_trial <- 200
xm_block<-xm0_block
xe_block<-xe4_block
rm(xe4_block,xm0_block)
#xm_block<-xm_block_visual
#xe_block<-xe_block_visual

for (i in 25:108) { #number of participants
  for (j in 1:n_trial) { # number of trials
    xm<-(xm_block[[i]][[j]][[1]])
    xe<-(xe_block[[i]][[j]][[1]])
    cutlength_x <- min(length(xm),length(xe))
    if (cutlength_x > 10000){
      cutlength_x <- 10000
    }

    if ( cutlength_x < 10 ){
      ansx1[j] <- list(0)

    } else {
      ansx1[j] <- list(crqa(xe[1:cutlength_x],xm[1:cutlength_x],
                                       delay = 68, embed = 3,rescale = 0, 
                                       radius = 70, normalize = 0,
                                       mindiagline = 2, minvertline = 2,
                                       tw = 0, whiteline = FALSE, recpt = FALSE,side = 'both'))
          }
 
  }
  save(ansx1,file=paste0('rr_downs70',i,'.RData'))
  ansx1<-array(NA,dim = 200)


  }         
                             
                             

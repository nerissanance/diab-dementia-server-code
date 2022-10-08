
#https://www.mireilleschnitzer.com/collaborative-longitudinal-tmle.html

#####################################################################
#Mini helper functions
#####################################################################

datagen_xx<-function(n,seed=sample(1:100000,size=1)){

  set.seed(seed)
  In0=rnorm(n, 0, 1)
  L0=rnorm(n, 0, 1)
  R0=rnorm(n, 0, 1)
  In1=rnorm(n, 0, 1)
  #A1
  p_A1=plogis(In0+L0)
  A1=rbinom(n, 1, p_A1)
  #R1
  R1=rnorm(n,0.5*R0,1)
  #L1
  m_L1=0.5*L0+0.5*A1
  L1=rnorm(n,mean=m_L1,sd=1)
  #A2
  p_A2=plogis(A1+In1+In0+L1)
  A2=rbinom(n,1,p_A2)
  #Y
  p_Y=plogis(-2+0.5*A1+0.5*A2-L0-L1+0.5*R0+0.5*R1)
  Y=rbinom(n, 1, p_Y)

  truth<-  0.2858638

  #
  X=data.frame(In0, L0, In1, R0, A1, R1, L1 ,A2,Y)

  return(X)

}

#given predictions, does update
#returns Q2pred_star and e2
update2<-function(g1pred,g2pred,Q2pred,e2=NA,X){
  H2<-(X$A2==a2&X$A1==a1)/(g1pred*g2pred)
  Q2pred[Q2pred<=0]<-.Machine$double.neg.eps
  Q2pred[Q2pred>=1]<-1-.Machine$double.neg.eps
  Offset=qlogis(Q2pred)
  if (is.na(e2)){
    e2<-coef(glm(Y~-1+H2,subset=(A2==a2&A1==a1),offset=Offset,family=binomial(),data=X))
  }
  Q2pred_star<-plogis( Offset + e2/(g1pred*g2pred) )
  return(list(Q2pred_star=Q2pred_star,e2=e2))
}
#given predictions does Q1 update
#returns Q1pred_star and e1
update1<-function(g1pred,Q1pred,Q2pred,e1=NA,X){
  H1<-(X$A1==a1)/(g1pred)
  Q1pred[Q1pred<=0]<-.Machine$double.neg.eps
  Q1pred[Q1pred>=1]<-1-.Machine$double.neg.eps
  Offset=qlogis(Q1pred)
  if (is.na(e1)){
    e1<-coef(glm(Q2pred~-1+H1,subset=(A1==a1),offset=Offset,family=binomial(),data=X))
  }
  Q1pred_star<-plogis( Offset + e1/(g1pred) )
  return(list(Q1pred_star=Q1pred_star,e1=e1))
}

#fit the g2 model from just terms to be included and the data
fit_g2<-function(terms2,X){
  g2mod<-glm.fit(y=X$A2,x=cbind(1,X$A1,X[,terms2]),family=binomial())
  g2pred<-plogis(as.matrix(cbind(1,a1,X[,terms2]))%*%coef(g2mod))*a2+(1-plogis(as.matrix(cbind(1,a1,X[,terms2]))%*%coef(g2mod)))*(1-a2)
  return(list(g2mod=g2mod,g2pred=g2pred))
}
#fit the g2 model from just terms to be included and the data
fit_g1<-function(terms1,X){
  g1mod<-glm.fit(y=X$A1,x=cbind(1,X[,terms1]),family=binomial())
  g1pred<-plogis(as.matrix(cbind(1,X[,terms1]))%*%coef(g1mod))*a1+(1-plogis(as.matrix(cbind(1,X[,terms1]))%*%coef(g1mod)))*(1-a1)
  return(list(g1mod=g1mod,g1pred=g1pred))
}
#calculate the loss in Q2 or Q1 from generic fit and data
#Q2pred and Q1pred are vectors
get_loss2<-function(Q2pred, X){
  Q2pred[Q2pred<=0]<-.Machine$double.neg.eps
  Q2pred[Q2pred>=1]<-1-.Machine$double.neg.eps
  loss2<- -1*mean((X$A2==a2&X$A1==a1)*(X$Y*log(Q2pred)+(1-X$Y)*log(1-Q2pred)))
  return(loss2)
}
get_loss1<-function(Q1pred, Q2pred, X){
  Q1pred[Q1pred<=0]<-.Machine$double.neg.eps
  Q1pred[Q1pred>=1]<-1-.Machine$double.neg.eps
  loss1<- -1*mean((X$A1==a1)*(Q2pred*log(Q1pred)+(1-Q2pred)*log(1-Q1pred)))
  return(loss1)
}
get_loss_tot<-function(Q1pred,Q2pred,Q2pred_orig, X){
  losst<-get_loss2(Q2pred,X)+get_loss1(Q1pred,Q2pred_orig,X)
  return(losst)
}
get_loss_totCV<-function(Q1pred,Q2pred,Q2pred_orig, g1pred,g2pred, X){
  est<-mean(Q1pred)
  EIF<-(X$A2==a2)*(X$A1==a1)*(X$Y-Q2pred)/(g1pred*g2pred) + (X$A1==a1)*(Q2pred_orig-Q1pred)/g1pred + (Q1pred-est)
  losst<-get_loss2(Q2pred,X)+get_loss1(Q1pred,Q2pred_orig,X)+var(EIF)/length(X$Y)
  return(losst)
}
#fit Q2 model from just the formula (of form Y~A1+A2+covariates), the data, and the covariable matrix (excluding intercept, A1 and A2)
get_Q2<-function(Q2form, Q2cov_mat, X){
  Q2mod<-glm(Q2form,family=binomial(),data=X)
  n1<-length(X$Y)
  newdatapred<-cbind(rep(1,n1),A1=a1,A2=a2,Q2cov_mat)
  Q2pred<-predict(Q2mod,type="response",newdata=as.data.frame(newdatapred))
  return(list(Q2mod=Q2mod, Q2pred=Q2pred))
}
#fit Q1 model from just the formula (of form Q2pred~A1+covariates), the fit of Q2, the data, and the covariable matrix (excluding intercept, A1 and A2)
get_Q1<-function(Q1form, Q2pred, Q1cov_mat, X){
  Q1mod<-glm(Q1form,family=binomial(),data=cbind(Q2pred,X))
  n1<-length(X$Y)
  newdatapred<-cbind(rep(1,n1),A1=a1,Q1cov_mat)
  Q1pred<-predict(Q1mod,type="response",newdata=as.data.frame(newdatapred))
  return(list(Q1mod=Q1mod, Q1pred=Q1pred))
}

#given terms and Q2pred, what is the loss? Gives full details about resulting fit
test_add<-function(terms1, terms2, Q1pred, Q2pred, Q2pred_orig, X){

  #fit g models and get predictions
  infog1<-fit_g1(terms1,X)
  g1mod<-infog1$g1mod
  g1pred<-infog1$g1pred
  infog2<-fit_g2(terms2,X)
  g2mod<-infog2$g2mod
  g2pred<-infog2$g2pred

  #do update
  updateinfo2<-update2(g1pred=g1pred,g2pred=g2pred,Q2pred=Q2pred,X=X)
  e2<-updateinfo2$e2
  Q2pred_star<-as.vector(updateinfo2$Q2pred_star)

  updateinfo1<-update1(g1pred=g1pred,Q1pred=Q1pred,Q2pred=Q2pred_orig,X=X)
  e1<-updateinfo1$e1
  Q1pred_star<-as.vector(updateinfo1$Q1pred_star)

  #get loss with updated Q_stars
  loss<-get_loss_tot(Q1pred=Q1pred_star,Q2pred=Q2pred_star,Q2pred_orig=Q2pred_orig, X)

  return(list(loss=loss,g1mod=g1mod,g2mod=g2mod,g1pred=g1pred,g2pred=g2pred,e=c(e1,e2),Q1pred_star=Q1pred_star,Q2pred_star=Q2pred_star))
}


##################################################################

#############################################
#Q: test every possible addition to g2 and g1
#select the best
#return new list of covariates, loss2, and new Q1pred_star, Q2pred_star
#one of the remaining lists is non-null
selectQ<-function(terms1,terms2,remaining1,remaining2,Q1pred, Q2pred,Q2pred_orig, X){

  modopt1<-list()
  if(length(remaining1)!=0){
    loss2_1<-rep(Inf,length(remaining1))
    for ( l in 1:length(remaining1) ){
      modopt1[[l]]<-test_add(terms1=c(terms1,remaining1[l]),terms2=terms2,Q1pred=Q1pred,Q2pred=Q2pred,Q2pred_orig=Q2pred_orig,X=X)
      loss2_1[l]<-modopt1[[l]]$loss
    }
  }else{loss2_1<-NULL}

  modopt2<-list()
  if(length(remaining2)!=0){
    loss2_2<-rep(Inf,length(remaining2))
    for ( l in 1:length(remaining2) ){
      modopt2[[l]]<-test_add(terms1=terms1,terms2=c(terms2,remaining2[l]),Q1pred=Q1pred,Q2pred=Q2pred,Q2pred_orig=Q2pred_orig,X=X)
      loss2_2[l]<-modopt2[[l]]$loss
    }
  }else{loss2_2<-NULL}

  loss_opt<-min(loss2_1,loss2_2)
  wm<-which.min(c(loss2_1,loss2_2))
  name_var<-c(remaining1,remaining2)[wm]
  g1_select<-F
  g2_select<-F
  if (wm>length(remaining1)){ #variable selected into the 2nd model (g2)
    g2_select<-T;terms2<-c(terms2,name_var); remaining2<-remaining2[remaining2!=name_var]; Q2pred_star<-modopt2[[wm-length(remaining1)]]$Q2pred_star; Q1pred_star<-modopt2[[wm-length(remaining1)]]$Q1pred_star
    e<-modopt2[[wm-length(remaining1)]]$e; g1mod<-modopt2[[wm-length(remaining1)]]$g1mod; g2mod<-modopt2[[wm-length(remaining1)]]$g2mod;
  }else{ #variable selected into the 1st model (g1)
    g1_select<-T;terms1<-c(terms1,name_var); remaining1<-remaining1[remaining1!=name_var]; Q2pred_star<-modopt1[[wm]]$Q2pred_star; Q1pred_star<-modopt1[[wm]]$Q1pred_star
    e<-modopt1[[wm]]$e; g1mod<-modopt1[[wm]]$g1mod; g2mod<-modopt1[[wm]]$g2mod;
  }

  return(list(name_var=name_var,g1_select=g1_select,g2_select=g2_select,terms1=terms1,terms2=terms2,remaining1=remaining1,remaining2=remaining2,loss_opt=loss_opt,Q1pred_star=Q1pred_star,Q2pred_star=Q2pred_star,e=e,g1mod=g1mod,g2mod=g2mod))
}
#####################################################################

#Build Q#############################################################
#stop is between 1 and (length(remaining1)+length(remaining2)+1)

buildQ<-function(terms1,terms2,remaining1,remaining2,Q1pred,Q2pred,stop=(length(remaining1)+length(remaining2)+1),X){

  modsQ<-list() #stores each model in the sequence
  update<-rep(0,stop) #indicates where the updates (to both Q1 and Q2) were done

  num_vars<-length(remaining1)+length(remaining2)


  #First current Q1 and Q2 are the initial ones without update
  Q2pred_c<-Q2pred_orig<-Q2pred
  Q1pred_c<-Q1pred

  #The first candidate has the update with the empty (or forced in) g models
  g2_fit=fit_g2(terms2,X)
  g1_fit=fit_g1(terms1,X)
  g2pred=g2_fit$g2pred
  g1pred=g1_fit$g1pred
  g2mod=g2_fit$g2mod
  g1mod=g1_fit$g1mod
  up2<-update2(g1pred=g1pred,g2pred=g2pred,Q2pred=Q2pred,X=X)
  Q2pred_star<-up2$Q2pred_star
  e2<-up2$e2
  up1<-update1(g1pred=g1pred, Q2pred=Q2pred_orig, Q1pred=Q1pred,X=X)
  Q1pred_star<-up1$Q1pred_star
  e1<-up1$e1

  first_losst<-get_loss_tot(Q1pred=as.vector(Q1pred_star),Q2pred=as.vector(Q2pred_star),Q2pred_orig=as.vector(Q2pred_orig), X) #Note that the Q2star and orig are the same at the first step (so this also saves the original Q2pred_orig that is used in the loss function)
  modsQ[[1]]<-list(Q2pred2=Q2pred_star,Q1pred=Q1pred_star,terms1=terms1,terms2=terms2,loss=first_losst,update=0,g1mod=g1mod,g2mod=g2mod,e=c(e1,e2))

  if (stop>1){

    loss_seq<-c(first_losst,rep(Inf,stop-1)) #vector of in-bag loss values
    for (k in 2:stop){

      #identify optimal covariate addition
      sel<-selectQ(terms1=terms1,terms2=terms2,remaining1,remaining2,Q1pred=Q1pred_c,Q2pred=Q2pred_c,Q2pred_orig=Q2pred_orig,X=X)
      loss_seq[k]<-sel$loss_opt

      #check if improvement
      if (loss_seq[k]<loss_seq[k-1]){ #update the g models and keep the updated Q if needed later
        remaining1<-sel$remaining1
        remaining2<-sel$remaining2
        terms1<-sel$terms1
        terms2<-sel$terms2
        e<-sel$e
        g1mod<-sel$g1mod
        g2mod<-sel$g2mod
        Q2pred_star<-sel$Q2pred_star #current best update
        Q1pred_star<-sel$Q1pred_star #current best update
      } else{ #update the current Q to be the last optimal Qstar
        Q2pred_c<-modsQ[[k-1]]$Q2pred
        Q1pred_c<-modsQ[[k-1]]$Q1pred
        update[k]<-1 #indicates that an update was performed
        #then select the next term
        sel<-selectQ(terms1=terms1,terms2=terms2,remaining1,remaining2,Q1pred=Q1pred_c,Q2pred=Q2pred_c,Q2pred_orig=Q2pred_orig,X=X)
        loss_seq[k]<-sel$loss_opt
        remaining1<-sel$remaining1
        remaining2<-sel$remaining2
        terms1<-sel$terms1
        terms2<-sel$terms2
        e<-sel$e
        g1mod<-sel$g1mod
        g2mod<-sel$g2mod
        Q2pred_star<-sel$Q2pred_star
        Q1pred_star<-sel$Q1pred_star
      }
      modsQ[[k]]<-list(Q1pred=Q1pred_star,Q2pred=Q2pred_star,terms1=terms1,terms2=terms2,loss=loss_seq[k],update=update[k],g1mod=g1mod,g2mod=g2mod,e=e)

    } #end loop

  } else{ loss_seq=first_losst } #end if stop>1

  return(list( modsQ=modsQ, loss=loss_seq[stop], Q1pred_star=Q1pred_star,Q2pred_star=Q2pred_star,terms1=terms1, terms2=terms2, remaining1=remaining1, remaining2=remaining2, g2mod=g2mod, g1mod=g1mod))
} #end buildQ2 function

####################################################

####################################################
#Q cross-validation
#For every variable addition, calculate the CV error of resulting model
CVQ<-function(R,terms1,terms2,remaining1,remaining2,X, Q1formula, Q2formula, Q1cov_mat, Q2cov_mat){

  ssize<-length(X$Y)

  J<-length(c(remaining1,remaining2))

  lossval<-matrix(nrow=R,ncol=(J+1))

  #Randomly shuffle the data
  shuffle<-sample(ssize)
  Xs<-as.data.frame(as.matrix(X[shuffle,]))
  Q2cov_mats<-NULL
  Q1cov_mats<-NULL
  if (!is.null(Q2cov_mat)){
    Q2cov_mats<-as.data.frame(as.matrix(Q2cov_mat[shuffle,]))
  }
  if (!is.null(Q1cov_mat)){
    Q1cov_mats<-as.data.frame(as.matrix(Q1cov_mat[shuffle,]))
    names(Q1cov_mats)<-names(Q1cov_mat)
  }

  #Create R equally sized folds
  folds <- cut(seq(1,length(Xs$Y)),breaks=R,labels=FALSE)


  #Perform R fold cross validation
  for(k in 1:R){
    #Segement your data by fold using the which() function
    testIndexes <- which(folds==k,arr.ind=TRUE)

    #trainData <- dat1[-testIndexes, ]
    X1<-Xs[-testIndexes,]
    n1<-dim(X1)[1]
    #testData <- dat1[testIndexes, ]
    X2<-Xs[testIndexes,]
    n2<-dim(X2)[1]
    Q2cov_mat1=NULL
    Q2cov_mat2=NULL
    Q1cov_mat1=NULL
    Q1cov_mat2=NULL
    if (!is.null(Q2cov_mat)){
      Q2cov_mat1<-Q2cov_mats[-testIndexes,] #train
      Q2cov_mat2<-Q2cov_mats[testIndexes,] #test
    }
    if (!is.null(Q1cov_mat)){
      Q1cov_mat1<-as.data.frame(as.matrix(Q1cov_mats[-testIndexes,]))
      names(Q1cov_mat1)<-names(Q1cov_mat)
      Q1cov_mat2<-as.data.frame(as.matrix(Q1cov_mats[testIndexes,]))
      names(Q1cov_mat2)<-names(Q1cov_mat)
    }

    Q2train_info<-get_Q2(Q2form=Q2formula, Q2cov_mat=Q2cov_mat1, X=X1)
    Q2modtrain<-Q2train_info$Q2mod
    Q2predtrain<-Q2train_info$Q2pred

    Q1train_info<-get_Q1(Q1form=Q1formula, Q2pred=Q2predtrain, Q1cov_mat=Q1cov_mat1, X=X1)
    Q1modtrain<-Q1train_info$Q1mod
    Q1predtrain<-Q1train_info$Q1pred

    #run model on train Data
    res<-buildQ(terms1=terms1,terms2=terms2,remaining1=remaining1,remaining2=remaining2,Q2pred=Q2predtrain,Q1pred=Q1predtrain, stop=J+1,X=X1)

    #get current Q1predtest and Q2predtest on test set
    Q2predtest<-plogis( as.matrix(cbind(rep(1,n2),A1=a1,A2=a2,Q2cov_mat2))%*%coef(Q2modtrain) )
    Q2predt_c<-Q2pred_star_test<-Q2predtest

    Q1predtest<-plogis( as.matrix(cbind(rep(1,n2),A1=a1,Q1cov_mat2))%*%coef(Q1modtrain) )
    Q1predt_c<-Q1pred_star_test<-Q1predtest

    for (t in 1:(J+1)){ #loop for each variable in order
      Q2predt_last<-Q2pred_star_test
      Q1predt_last<-Q1pred_star_test
      if (res$modsQ[[t]]$update==0){ #if no update, then only need g1mod, g2mod and e2 to get Q2pred_star
        terms1_train<-(res$modsQ)[[t]]$terms1
        terms2_train<-(res$modsQ)[[t]]$terms2
        g1mod_train<-(res$modsQ)[[t]]$g1mod
        g2mod_train<-(res$modsQ)[[t]]$g2mod
        #get g1 and g2 preds
        g1pred_test<-plogis(as.matrix(cbind(rep(1,n2),X2[,terms1_train]))%*%coef(g1mod_train))*a1+(1-plogis(as.matrix(cbind(rep(1,n2),X2[,terms1_train]))%*%coef(g1mod_train)))*(1-a1)
        g2pred_test<-plogis(as.matrix(cbind(rep(1,n2),a1,X2[,terms2_train]))%*%coef(g2mod_train))*a2+(1-plogis(as.matrix(cbind(rep(1,n2),a1,X2[,terms2_train]))%*%coef(g2mod_train)))*(1-a2)
        e_train<-(res$modsQ)[[t]]$e
        Q2pred_star_test<-update2(g1pred=g1pred_test, g2pred=g2pred_test, Q2pred=Q2predt_c, e2=e_train[2], X=X2)$Q2pred_star #test data to get CV risk
        Q1pred_star_test<-update1(g1pred=g1pred_test, Q1pred=Q1predt_c, e1=e_train[1], X=X2)$Q1pred_star #test data to get CV risk
      } else {
        Q2predt_c<-Q2predt_last #replace the current Q2pred with the last updated one. Then update
        Q1predt_c<-Q1predt_last #replace the current Q1pred with the last updated one. Then update
        terms1_train<-(res$modsQ)[[t]]$terms1
        terms2_train<-(res$modsQ)[[t]]$terms2
        g1mod_train<-(res$modsQ)[[t]]$g1mod
        g2mod_train<-(res$modsQ)[[t]]$g2mod
        #get g1 and g2 preds
        g1pred_test<-plogis(as.matrix(cbind(1,X2[,terms1_train]))%*%coef(g1mod_train))*a1+(1-plogis(as.matrix(cbind(1,X2[,terms1_train]))%*%coef(g1mod_train)))*(1-a1)
        g2pred_test<-plogis(as.matrix(cbind(1,a1,X2[,terms2_train]))%*%coef(g2mod_train))*a2+(1-plogis(as.matrix(cbind(1,a1,X2[,terms2_train]))%*%coef(g2mod_train)))*(1-a2)
        e_train<-(res$modsQ)[[t]]$e
        Q2pred_star_test<-update2(g1pred=g1pred_test, g2pred=g2pred_test, Q2pred=Q2predt_c, e2=e_train[2], X=X2)$Q2pred_star #test data to get CV risk
        Q1pred_star_test<-update1(g1pred=g1pred_test, Q1pred=Q1predt_c, Q2pred=Q2predt_c, e1=e_train[1], X=X2)$Q1pred_star #test data to get CV risk
      }
      #Test error with test Data
      lossval[k,t]<-get_loss_totCV(Q1pred=Q1pred_star_test, Q2pred=Q2pred_star_test,Q2pred_orig=Q2predtest,g1pred=g1pred_test,g2pred=g2pred_test,X=X2)
    }
  }

  kloss<-colMeans(lossval)


  return(kloss)
}

##################################################
#estimate the variance of the final estimator
get_var<-function(Q1pred,Q2pred,Q2pred_orig,g1pred,g2pred,est,X){
  EIF<-(X$A2==a2)*(X$A1==a1)*(X$Y-Q2pred)/(g1pred*g2pred) + (X$A1==a1)*(Q2pred_orig-Q1pred)/g1pred + (Q1pred-est)
  var<-var(EIF)/length(X$Y)
  return(list(var=var,EIF=EIF))
}

get_var_ltmle<-function(Q2form_ltmle, Q1form_ltmle,g1fit,g2fit,X){

  ltmleres <- ltmle(data=X, Anodes=c("A1","A2"),
                    Lnodes=c("In1","L1", "R1"), Ynodes=c("Y"), abar=c(1, 1),
                    Qform=c(In1=Q1form_ltmle, Y=Q2form_ltmle), gform=cbind(g1fit,g2fit),
                    gbound=c(0,1),SL.library=NULL,variance.method = "tmle")
  return((summary(ltmleres)$treatment$std.dev)^2)

}


####################################################
#Run C-TMLE

CTMLE_total<-function(fullX, terms_remaining1, terms_remaining2, terms_selected1=NULL, terms_selected2=NULL, Qcorr){

  #Initial Q

  X=fullX

  if (Qcorr==T){
    Q2form<-"Y~A2+A1+L0+L1+R0+R1" ; Q2form_ltmle<-"Q.kplus1~A2+A1+L0+L1+R0+R1"
    Q1form<-"Q2pred~A1+L0+R0" ; Q1form_ltmle<-"Q.kplus1~A1+L0+R0"

    Q2cov_mat<-as.data.frame(cbind(L0=X$L0,L1=X$L1,R0=X$R0,R1=X$R1))
    Q1cov_mat<-as.data.frame(cbind(L0=X$L0,R0=X$R0))

    #Q2form<-"Y~A2+A1+L0+L1" ; Q2form_ltmle<-"Q.kplus1~A2+A1+L0+L1"
    #Q1form<-"Q2pred~A1+L0"; Q1form_ltmle<-"Q.kplus1~A1+L0"

    #Q2cov_mat<-as.data.frame(cbind(L0=X$L0,L1=X$L1))
    #Q1cov_mat<-as.data.frame(cbind(L0=X$L0))


  } else {
    Q2form<-"Y~A2+A1"  ; Q2form_ltmle<-"Q.kplus1~A2+A1"
    Q1form<-"Q2pred~A1" ; Q1form_ltmle<-"Q.kplus1~A1"
    Q2cov_mat<-NULL
    Q1cov_mat<-NULL
  }

  Q2pred<-get_Q2(Q2form=Q2form,Q2cov_mat=Q2cov_mat,X=X)$Q2pred
  Q1pred<-get_Q1(Q1form=Q1form, Q2pred=Q2pred, Q1cov_mat=Q1cov_mat, X=X)$Q1pred

  #select stop
  kloss<-CVQ(5,terms1=terms_selected1,terms2=terms_selected2,remaining1=terms_remaining1,remaining2=terms_remaining2,X=X,Q1formula=Q1form,Q2formula=Q2form,Q1cov_mat=Q1cov_mat,Q2cov_mat=Q2cov_mat)
  stop<-which.min(kloss)

  Qresult<-buildQ(terms1=terms_selected1,terms2=terms_selected2,remaining1=terms_remaining1,remaining2=terms_remaining2,Q2pred=Q2pred,Q1pred=Q1pred,stop=stop,X)

  Q2pred_star<-Qresult$Q2pred_star
  terms_selected11<-Qresult$terms1
  terms_selected21<-Qresult$terms2
  terms_remaining11<-Qresult$remaining1
  terms_remaining21<-Qresult$remaining2
  g1pred<-fit_g1(terms_selected11,X)$g1pred
  g2pred=fit_g2(terms_selected21,X)$g2pred

  Q1pred<-get_Q1(Q1form=Q1form, Q2pred=Q2pred_star, Q1cov_mat=Q1cov_mat, X=X)$Q1pred

  #use selected g1mod to update Q1
  Q1pred_star<-update1(g1pred=g1pred, Q1pred=Q1pred, Q2pred=Q2pred_star, X=X)$Q1pred_star

  est<-mean(Q1pred_star)
  var<-get_var(Q1pred=Q1pred_star,Q2pred=Q2pred_star,Q2pred_orig=Q2pred_star,g1pred=g1pred,g2pred=g2pred,est=est,X=X)$var
  var2<-get_var_ltmle(Q2form_ltmle, Q1form_ltmle,g1pred,g2pred,X)


  return(list(est=est,var=var,var2=var2,terms21=terms_selected21,terms11=terms_selected11,terms12=terms_selected11))#return
} #end CTMLE function






#do this once
seeds<-sample(1:1000000,size=5000)
#write(seeds,file="seeds.txt",ncol=1)
#seeds<-read.table("seeds.txt",header=F)

a1=1; a2=1


for (i in 1:1000){

  seed<-seeds[i,1]
  X<-datagen_xx(1000,seed)
  terms_remaining1=c("In0","L0","R0")
  terms_remaining2=c("In0","L0","R0","In1", "L1", "R1")
  terms_selected1=NULL
  terms_selected2=NULL

  res<-CTMLE_total(fullX=X, terms_remaining1, terms_remaining2, terms_selected1=NULL, terms_selected2=NULL, Qcorr=T)

  #construct indicators for which covariates were retained
  indg11<-rep(NA,1)
  for (k in 1:length(terms_remaining1)){
    indg11[k]<-(terms_remaining1[k] %in% res$terms11)
  }
  indg21<-rep(NA,1)
  for (k in 1:length(terms_remaining2)){
    indg21[k]<-(terms_remaining2[k] %in% res$terms21)
  }
  indg12<-rep(NA,1)
  for (k in 1:length(terms_remaining1)){
    indg12[k]<-(terms_remaining1[k] %in% res$terms12)
  }

  resvect<-c(i,res$est,res$var,res$var2,indg11,indg21)

  write(resvect,"LCTMLE_1000_total_Qcor.txt",ncol=450,append=T)
}

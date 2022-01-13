### Function to predicted average VE in the new setting based on CoR data in the current setting, based on infection status and immune response data
## in vaccine arm of the current efficacy trial (possibly with subsampling of immune response), 
## disease prevalence in placebo recipients of the current efficacy trial, and immune response data in the new setting
## Assumptions: 1) risk of disease conditional on immune response follows a linear logistic model;
################2) placebo risk does not depend on vaccine-induced immune response;
################3) VE as a function of immune response is constant across settings
 
### input:
###
### dat.cor.vac: correlates data sampled from vaccine arm in the current efficiency trial, it needs to include the following variables:
  # Y: binary indicator for disease or not
  # S: immune correlate value
  # weights: sampling weights for measuring S
  
### prev.cur: disease prevalence in placebo arm of the current setting

### S.new: immunogenicity for S in vaccine arm measured in the new setting (where VE prediction is of interest)

### weights.new: sampling weight for S.new in the new setting, default to 1 for random sample if not provided by user

### output:
### VE.new: predicted average VE in the new setting

predVE<-function(dat.cor.vacc, prev.cur, S.new, weights.new=rep(1,length(S.new))){
    fit=glm(Y~S,data=dat.cor.vacc,weights=weights,family=binomial(link=logit))    
    ve.out=1-predict(fit,newdata=data.frame(S=S.new),type='response')/prev.cur
    sum(ve.out*weights.new)/sum(weights.new)
}

###




n<-10000
beta<-c(-2,-0.8)
set.seed(1)
S<-rnorm(n,2,1)
R<-1/(1+exp(-beta[1]-beta[2]*S))
Y<-rbinom(n,1,R)

prev.cur<-0.05

### 
prob.S<-ifelse(Y==1,1,0.05)
delta<-rbinom(n,1,prob.S)

dat.cor.vacc<-data.frame(Y=Y[delta==1],S=S[delta==1],weights=1/prob.S[delta==1])
S.new<-rnorm(100,2.5,1)


predVE(dat.cor.vacc=dat.cor.vacc,prev.cur=prev.cur,S.new=S.new)

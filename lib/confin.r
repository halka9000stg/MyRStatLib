smean="mean"
svar="var"
slec =paste(svar,smean,sep="&&")

confin.version=function(){"0.2.1"}
confin.ver=function(){confin.version()}
confin.n=function(){8.3}
confin=function(vec,y,kind){
  if(kind=="mean"){
    confin.mean(vec,y)
  }
  if(kind=="var"){
    confin.var(vec,y)
  }
  confin.n()
}

# (y*100)% interval estimation for a population mean of (vec), with normal-distribution.
# population variance known
confin.mean_kn=function(vec,y,pop_var){
  len=length(vec)
  mn=mean(vec)
  vr=var(vec)
  alpha=1-y
  al_hf=alpha/2
  normval_hf=qnorm(al_hf)
  lower=mn-normval_hf*sqrt(pop_var/len)
  upper=mn+normval_hf*sqrt(pop_var/len)
  c(lower,upper)
}

# (y*100)% interval estimation for a population mean of (vec), with t-distribution.
# population variance unknown
confin.mean=function(vec,y){
  len=length(vec)
  mn=mean(vec)
  vr=var(vec)
  dof=len-1
  alpha=1-y
  al_hf=alpha/2
  tval_hf=qt(al_hf,dof)
  lower=mn-tval_hf*sqrt(vr/len)
  upper=mn+tval_hf*sqrt(vr/len)
  c(lower,upper)
}

# (y*100)% interval estimation for a population variance of (vec), with chi-distribution.
confin.var=function(vec,y){
  len=length(vec)
  mn=mean(vec)
  vr=var(vec)
  dof=len-1
  alpha=1-y
  al_hf=alpha/2
  al_hf_m=1-al_hf
  chival_hf=qchisq(al_hf,dof)
  chival_hf_m=qchisq(al_hf_m,dof)
  lower=(dof*vr)/chival_hf
  upper=(dof*vr)/chival_hf_m
  c(lower,upper)
}
confin.mean.iv=function(vec,y){
  confin.mean(vec,y)[1]-confin.mean(vec,y)[2]
}
confin.var.iv=function(vec,y){
  confin.var(vec,y)[1]-confin.var(vec,y)[2]
}
confin.iv=function(vec,y,kind){
  confin(vec,y,kind)[1]-confin(vec,y,kind)[2]
}

# (y*100)% interval estimation for a population mean difference of (vec), with normal-distribution.
# var known
confin.mean_df_kn=function(vec,y){}

# (y*100)% interval estimation for a population mean difference of (vec), with mearged variance.
# var1=var2=var,but unknown
confin.mean_df_eq=function(vec,y){}

# (y*100)% interval estimation for a population mean difference of (vec), with ?-distribution.
#var1!=var2,and unknown
confin.mean_df=function(vec,y){}

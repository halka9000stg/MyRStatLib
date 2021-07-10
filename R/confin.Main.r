
confin=function(vec,y,kind){
  if(kind=="mean"){
    confin.mean(vec,y)
  }
  if(kind=="var"){
    confin.var(vec,y)
  }
  confin.n()
}
confin.names=function(x){
  names(x)=c("lower","upper")
  x
}
confin.names.proto=function(x,proto){
  names(x)=proto
  x
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
  res = confin.names(c(lower,upper))
  res
}
confin.mean_kn.data=function(vec,y,pop_var){
  len=length(vec)
  mn=mean(vec)
  vr=var(vec)
  alpha=1-y
  al_hf=alpha/2
  normval_hf=qnorm(al_hf)
  lower=mn-normval_hf*sqrt(pop_var/len)
  upper=mn+normval_hf*sqrt(pop_var/len)
  res = confin.names.proto(c(len,mn,vr,alpha,al_hf,normval_hf),confin.mean_kn.data.returns())
  res
}
confin.mean_kn.data.returns=function(){
  c("len","mn","vr","alpha","al_hf","normval_hf")
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
  res = confin.names(c(lower,upper))
  res
}
confin.mean.data=function(vec,y){
  len=length(vec)
  mn=mean(vec)
  vr=var(vec)
  dof=len-1
  alpha=1-y
  al_hf=alpha/2
  tval_hf=qt(al_hf,dof)
  lower=mn-tval_hf*sqrt(vr/len)
  upper=mn+tval_hf*sqrt(vr/len)
  res = confin.names.proto(c(len,mn,vr,dof,alpha,al_hf,tval_hf),confin.mean.data.returns())
  res
}
confin.mean.data.returns=function(){
  c("ln","mn","vr","dof","alpha","al_hf","tval_hf")
}
confin.mean2=function(len,mn,vr,y){
  dof=len-1
  alpha=1-y
  al_hf=alpha/2
  tval_hf=qt(al_hf,dof)
  lower=mn-tval_hf*sqrt(vr/len)
  upper=mn+tval_hf*sqrt(vr/len)
  res = confin.names(c(lower,upper))
  res
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
  res = confin.names(c(lower,upper))
  res
}
confin.var.data=function(vec,y){
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
  res = confin.names.proto(c(len,mn,dof,alpha,al_hf,al_hf_m,chival_hf,chival_hf_m),confin.var.data.returns())
  res
}
confin.var.data.returns=function(){
  c("len","mn","dof","alpha","al_hf","al_hf_m","chival_hf","chival_hf_m")
}


# (y*100)% interval estimation for a population mean difference of (vec), with normal-distribution.
# var known
confin.mean_df_kn=function(vec,y){}

# (y*100)% interval estimation for a population mean difference of (vec), with t-distribution, and mearged variance.
# var1=var2=var,but unknown
confin.mean_df_eq=function(x_len,x_mean,x_var,y_len,y_mean,y_var,y){
  x_dof=x_len-1
  y_dof=y_len-1
  alpha=1-y
  tval=qt(alpha,x_dof+y_dof)
  mgd_var=(x_dof*x_var+y_dof*y_var)/(x_dof+y_dof)
  lower=(x_mean-y_mean)-tval*(x_dof+y_dof)*sqrt(mgd_var*((1/x_len)+(1/y_len)))
  upper=(x_mean-y_mean)+tval*(x_dof+y_dof)*sqrt(mgd_var*((1/x_len)+(1/y_len)))
  res = confin.names(c(lower,upper))
  res
}
confin.mean_df_eq.data=function(x_len,x_mean,x_var,y_len,y_mean,y_var,y){
  x_dof=x_len-1
  y_dof=y_len-1
  alpha=1-y
  tval=qt(alpha,x_dof+y_dof)
  mgd_var=(x_dof*x_var+y_dof*y_var)/(x_dof+y_dof)
  lower=(x_mean-y_mean)-tval*(x_dof+y_dof)*sqrt(mgd_var*((1/x_len)+(1/y_len)))
  upper=(x_mean-y_mean)+tval*(x_dof+y_dof)*sqrt(mgd_var*((1/x_len)+(1/y_len)))
  res = confin.names.proto(c(x_dof,y_dof,alpha,tval,mgd_var),confin.mean_df_eq.data.returns())
  res
}
confin.mean_df_eq.data.returns=function(){
  c("x_dof","y_dof","alpha","tval","mgd_var")
}
# (y*100)% interval estimation for a population mean difference of (vec), with ?-distribution.
#var1!=var2,and unknown
confin.mean_df=function(vec,y){}

confin.eq_pop_var=function(x_vec,y_vec,y){
  x_len=length(x_vec)
  y_len=length(y_vec)
  x_mean=mean(x_vec)
  y_mean=mean(y_vec)
  x_var=var(x_vec)
  y_var=var(y_vec)
  x_dof=x_len-1
  y_dof=y_len-1
  alpha=1-y
  al_hf=alpha/2
  fval_hf=qf(al_hf,x_dof,y_dof)
  fval_hf_m=1/fval_hf
  lower=fval_hf_m*(x_var/y_var)
  upper=fval_hf*(x_var/y_var)
  res = confin.names(c(lower,upper))
  res
}
confin.eq_pop_var.data=function(x_vec,y_vec,y){
  x_len=length(x_vec)
  y_len=length(y_vec)
  x_mean=mean(x_vec)
  y_mean=mean(y_vec)
  x_var=var(x_vec)
  y_var=var(y_vec)
  x_dof=x_len-1
  y_dof=y_len-1
  alpha=1-y
  al_hf=alpha/2
  fval_hf=qf(al_hf,x_dof,y_dof)
  fval_hf_m=1/fval_hf
  lower=fval_hf_m*(x_var/y_var)
  upper=fval_hf*(x_var/y_var)
  res = confin.names.proto(c(x_len,y_len,x_mean,y_mean,x_var,y_var,x_dof,y_dof,alpha,al_hf,fval_hf,fval_hf_m),confin.eq_pop_var.data.returns())
  res
}
confin.eq_pop_var.data.returns=function(){
  c("x_len","y_len","x_mean","y_mean","x_var","y_var","x_dof","y_dof","alpha","al_hf","fval_hf","fval_hf_m")
}
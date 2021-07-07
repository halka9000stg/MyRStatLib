#³‹K•ª•z•`‰æ
seiki = function(t,cl,x0,x1,l0,l1){
  #•½‹Ï,•ªU,F
  ln = nrow(t)
  for(i in 1:ln){
    if(i==1){
      d=F
    }else{
      d=T
    }
    curve(add=d,dnorm(x,t[i,1],t[i,2]),xlim=c(x0,x1),type="l",col=cl[i])
  }
  title("³‹K•ª•z")
  vc=numeric(ln)
  for(j in 1:ln){
    vc[j]=paste("N(",t[j,1],",",t[j,2],")")
  }
  legend(l0,l1,vc,lty=1,col=cl)
  abline(v=t[,1],col="gray",lty=2)
  for(k in 1:ln){
    mtext(text=paste("ƒÊ=",t[k,1],sep=""),at=t[k,1],col="gray")
  }
}
seiki2 = function(vec,cl,x0,x1,l0,l1){
  ln1=length(vec)/2
  mat=matrix(vec,nrow=ln1,ncol=2)
  mat
  seiki(mat,cl,x0,x1,l0,l1)
}
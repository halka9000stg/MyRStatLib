sct=function(){
  plot(sin,xlim=c(0,5),ylim=c(-1,1),ylab="",col=5,lty=3)
  par(new=T)
  plot(cos,xlim=c(0,5),ylim=c(-1,1),ylab="",col=1,lty=4)
  par(new=T)
  plot(tan,xlim=c(0,5),ylim=c(-1,1),ylab="",col=6,lty=4)
  lines(c(0,5),c(0,0))
  text(0.5,-0.7,"( ; ?É÷? )")
  title("sin,cos,tanîg")
}
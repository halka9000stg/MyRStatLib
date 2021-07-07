pis <- c(pi,2*pi,3*pi,4*pi,5*pi,6*pi,7*pi,8*pi)
pismat <- matrix(pis,nrow=2,ncol=4,byrow=T)
pismat
pismata <- trunc(pismat)
pismata
pismatb <- pismat - pismata
pismatb
pismata + pismatb

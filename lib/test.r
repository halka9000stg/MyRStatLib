test = function(y,x){
  #母集団N(0,1)中の標本xが信頼区間y%の範囲にあるか
  #標本数n
  n=1000
  Data = rnorm(n,0,1)
  mu_lower = - y*sqrt(1/n)
  mu_upper = y*sqrt(1/n)
  if(mu_lower < x && x < mu_upper){
    TRUE
  }else{
    FALSE
  }
}
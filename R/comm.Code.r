#符号のn次エントロピー
hent = function(source) {
  limit()
}

#r元シャノンファノ符号の符号長Lの下限(下限＋1＝上限)
# Vec source  情報源
# int r  r元符号
hent.shan_fano = function(source,r){
  len=length(source)
  ans=c()
  for (i in 1:len) {
    s = source[i]
    ans[i]=-1*p(s)*log(p(s),base = r)
  }
  sum(ans)
}
dirnamee = "./R/"
dirs = dir(pattern = ".r", dirnamee)
sourceall = function(adr,dne){
  for(x in 1:length(adr)){
    source(paste(dne,adr[x],sep = ""),encoding = "utf-8")
  }
}
sourceall(dirs,dirnamee)
rm(sourceall)
rm(dirs)
#stdpkg=c()
pkgi = function(x) Map(install.packages,x) 
#pkgi(stdpkg)
maplist = function(len, sta=0, end,fn,l_pre="",l_suf=""){
  #length, start, end, function, label prefix, label suffix
  if(missing(len)){
    if(missing(sta)&&missing(end)){
      warning("Either argment 'len' or 'end' is required")
    }
  }else if(missing(fun)){
    warning("Argument 'fun' is required")
  }
  vec = unlist(Map(fn,1:len))
  genname=function(x){
    paste(l_pre,x,l_suf,sep = "")
  }
  thename = unlist(Map(genname,1:len))
  names(vec) = thename
  vec
  }
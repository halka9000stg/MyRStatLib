dirs = dir(pattern = ".r", "./R")
sourceall = function(){
  for(x in 1:length(dirs)){
    source(paste("./R/",dirs[x],sep = ""),encoding = "utf-8")
  }
}
sourceall()
rm(sourceall)
rm(dirs)
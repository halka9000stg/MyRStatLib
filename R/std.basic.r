# Deviation value
std.deviations = function(vec){
  len = length(vec)
  mn = mean(vec)
  vr = var(vec)
  devs = rep(1,len)
  for(i in 1:len){
    devs[i] = ((10*(vec[i]-mn))/vr)+50
  }
  devs
}
std.deviations.takeval = function(vec,i){
  devs = deviation(vec)
  devs[i]
}

# manage-al
std.man.getsd = function(){
  dirr = "./myg_data/"
  dirs = dir(pattern = ".r", dirr)
  for(x in 1:length(dirs)){
    source(paste(dirr,dirs[x],sep = ""),encoding = "utf-8")
  }
  rm(dirr)
  rm(dirs)
}
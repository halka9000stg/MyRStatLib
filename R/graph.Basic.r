graph.link=function(node){(node*(node-1))/2}
graph.patFromStr=function(str){
  lines=unlist(strsplit(str,";"))
  for (lns in 1:length(lines)) {
    lines[lns]=unlist(strsplit(str,", "))
    lnsc=lines[lns]
    for(lnj in 1:length(lines[lns])){
      lnsc[lnj]=gsub(" ","",lnsc[lnj])
    }
    lnsc[lns]
  }
  do.call(rbind,as.list(lines))
}
c()
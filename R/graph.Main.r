graph.node.min=function(a,b){
  ifelse(a>b,b,a)
}

# ワーシャルフロイド法：最短経路問題
graph.spp.wfa = function(matA){
  ln = nrow(matA)
  d=matA
  for (xk in 1:ln) {
    for(xi in 1:ln){
      for(xj in 1:ln){
        d[xi,xj]=graph.node.min(d[xi,xj],d[xi,xk]+d[xk,xj])
      }
    }
  }
  d
}
# ダイクストラ法：最短経路問題
graph.spp.daijk=function(matA,s,t){
  #matrix matA, start s, end t
  replaceNA = function(vec,to){
    for (i in 1:length(vec)) {
      if(is.na(vec[i])){
        vec[i]=to
      }
    }
    vec
  }
  takeMin=function(vin){
    vin[-which(as.character(retMin(vin)))]
  }
  retMin=function(vec){
    vec=unlist(Map(as.integer,vec))
    l=length(vec)
    print(length(vec))
    vec = replaceNA(vec,Inf)
    min=Inf
    minid=0
    for(gi in 1:vec:length(vec)){
      if(!is.na(vec[gi]) && !is.nan(vec[gi]) && vec[gi]<min){
        minid=gi
        min=vec[gi]
      }
    }
    minid
  }
  w = function(i,j){
    matA[i,j]
  }
  
  ln = nrow(matA)
  v=Map(x=1:ln,f=as.character)
  q=v
  d=rep(Inf,ln)
  for (xi in 1:ln) {
    if(xi==s){
      d[xi]=0
    }
  }
  pre=numeric()
  pre[s]=s
  while(length(q)>0){
    #d(i)が最小となるq(i)を除去
    q=takeMin(q)
    if(as.integer(xi)==t){
      names(pre)=1:length(pre)
      return(pre)
    }else{
      for(eu in u[xi]){
        if(d[eu]>d[xi]+w(xi,eu)){
          d[eu]=d[xi]+w(xi,eu)
          pre[eu]=xi
        }
      }
    }
  }
}
#ダイクストラ法
dijkstra <- function(matrix, nstart){
  adjacency_matrix <- matrix
  adjacency_matrix[which(adjacency_matrix==0)] <- Inf #コストの設定されていないノードへの到達コストを∞にする
  diag(adjacency_matrix) <- 0 #対角成分すなわち自身へと回帰する部分のコストを0にする
  n <- nrow(matrix) #ノード数
  arrival_cost <- rep(Inf, n) #各ノードの到達コストを表すベクトル
  arrival_cost[nstart] <- 0 #スタート地点を0に設定（ややこしいのは各ノードをラベルで扱っているため）
  unknown_node <- 1:n #未確定ノードを表すベクトル
  unknown_node <- unknown_node[-nstart] #スタート地点の削除
  selected_node <- nstart #最初に選択されるノードはスタート地点
  ans_list=1:n
  for (i in 1:n) {
    ans_list[i]=c(Inf)
  }
  
  while(length(unknown_node) > 0){
    #イテレータは行列の列指定で使うのでjにしてあります。
    for(j in 1:n)
      #ここのfor文で中括弧を打つとなぜか通らなくなる（理由は不明、わかる方是非教えてください）
      arrival_cost[j]<-min(arrival_cost[j],arrival_cost[selected_node]+adjacency_matrix[selected_node,j])
    
    #未確定ノードの中から最も到達コストの低いノードを選択する
    selected_node<-unknown_node[which(arrival_cost[unknown_node]==min(arrival_cost[unknown_node]))[1]]
    
    #選択されたノードを確定とし、未確定ノードから削除する
    unknown_node <- unknown_node[-which(unknown_node == selected_node)]
    al=ans_list[j]
    al[length(al)+1]=selected_node
    
    
  }
  #各ノードへの距離が入ったベクトルを出す。
  list(arrival_cost,ans_list)
}
# プリム：最小全域木
# クラスカル：最小全域木
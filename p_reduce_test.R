library(TDA)
library(myfs)
library(phacm)
library(tidyverse)
library(rgl)
library(seephacm)
library(magrittr)

#anu<-anulusUnif(100)
plot(anu)
text(anu, labels = 1:100)
text(anu[c(1, cell_p[which(cell_p[, 1]==1), 2]), ], labels = c(1, cell_p[which(cell_p[, 1]==1), 2]))
text(anu[c(12, cell_p[which(cell_p[, 1]==12), 2]), ], labels = c(12, cell_p[which(cell_p[, 1]==12), 2]))
text(anu[c(12, diffts), ], labels = c(12, diffts))

cho<-choose(100*0.4, 2)
anu_dist<-dist(anu)

#dist関数確認
ts<-matrix(1:8, 4, 2)
ts_dist<-dist(ts)

cell_p<-c(0, 0)
cell_s<-0
i_p<-1
i_s<-1

thre<-sort(anu_dist)[cho]

for (j in 1:nrow(anu)) {
  i_j<-i_p
  
  for (k in 1:nrow(anu)) {
    
    if(as.matrix(anu_dist)[j, k] < thre && j!=k ){
      
      cell_p<-rbind(cell_p, c(j, k))
      
      i_p<-i_p+1
      
    }
    
  }
  #debugText(i_j, i_p)
  
  if(i_j==i_p){
    
    cell_s<-c(cell_s, j)
    
  }
  
}

cell_p<-cell_p[-1, ]
ip<-nrow(cell_p)

poly<-list(c(0,0))

for (i in 1:ip) {
  
}

cnct_ts<-connect(1, cell_p, 1:100)

points(anu[cnct_ts[[1]], ], col=2, pch=16)
points(anu[cnct_ts[[2]], ], col=3, pch=16)
for (p in 1:length(cnct_ts)) {
  points(anu[cnct_ts[[p]], ], col=rainbow(length(cnct_ts))[p], pch=16)
}

#cell_set関数テスト
cellset<-cell_set(anu, 0.3)
cnct_ts2<-connect(1, cellset[[1]], all = 1:100)
oldpar<-par(no.readonly = T)
for (p in 1:length(cnct_ts2)) {
  debugText(p)
  points(rbind(c(0, 0),anu[cnct_ts2[[p]], ]), col=rainbow(length(cnct_ts2))[p], pch=16)
}
par(oldpar)
points(anu[cnct_ts2[[1]], ], col=2, pch=16)

#reduce_points関数テスト
anu_red<-reduce_points(x = anu, conect = cnct_ts)

anu_red2<-reduce_points(x = anu, conect = cnct_ts2)

#PD計算
anu_diag<-ripsDiag(anu, maxdimension = 1, maxscale = 3)
plot(anu_diag[[1]])

anu_red_diag<-ripsDiag(anu_red, maxdimension = 1, maxscale = 3)
plot(anu_red_diag[[1]])

#トーラスで試す
torus1000<-torusUnif(1000, 1, 2.5)
figurePlot3d(torus1000)
torus1000_dist<-dist(torus1000)

cellset<-cell_set(torus1000, 0.5)
cnct_torus<-connect(1, cellset[[1]], all = 1:nrow(torus1000))
torus1000_red<-reduce_points(x = torus1000, conect = cnct_torus)

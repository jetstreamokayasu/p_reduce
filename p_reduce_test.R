library(TDA)
library(myfs)
library(phacm)
library(tidyverse)
library(rgl)
library(seephacm)
library(magrittr)

anu<-anulusUnif(100)
plot(anu, pch=16)
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
plot(anu_red2)
for (p in 1:length(cnct_ts2)) {
  debugText(p)
  points(rbind(c(0, 0), anu_red2[p,]), col=rainbow(length(anu_red2[, 1]))[p], pch=16)
}

#PD計算
anu_diag<-ripsDiag(anu, maxdimension = 1, maxscale = 3)
plot(anu_diag[[1]])

anu_red_diag<-ripsDiag(anu_red2, maxdimension = 1, maxscale = 3)
plot(anu_red_diag[[1]])

#トーラスで試す
torus1000<-torusUnif(1000, 1, 2.5)
figurePlot3d(torus1000)
torus1000_dist<-dist(torus1000)
rgl.snapshot("./data/torus1000_1.png")

#cellset<-cell_set(torus1000, 0.5)
#cnct_torus<-connect(1, cellset[[1]], all = 1:nrow(torus1000))
#torus1000_red<-reduce_points(x = torus1000, conect = cnct_torus)

torus_cell<-cell_set2(torus1000, 0.5)
cnct_torus<-connect2(1, torus_cell,  all = 1:nrow(torus1000))
torus1000_red<-reduce_points(x = torus1000, conect = cnct_torus)
figurePlot3d(torus1000_red)
torus1000_red_diag<-ripsDiag(torus1000_red, maxdimension = 2, maxscale = 3)
plot(torus1000_red_diag[[1]])

anu_cell<-cell_set2(anu, 0.4)
anu_cnct3<-connect2(1, anu_cell, all = 1:100)
anu_red3<-reduce_points(anu, anu_cnct3)
plot(anu_red3)
anu_red3_diag<-ripsDiag(anu_red2, maxdimension = 1, maxscale = 3)
plot(anu_red3_diag[[1]])

#トーラス試し2
torus_cell2<-cell_set2(torus1000, 0.3)
cnct_torus2<-connect2(1, torus_cell2,  all = 1:nrow(torus1000))
torus1000_red2<-reduce_points(x = torus1000, conect = cnct_torus2)
figurePlot3d(torus1000_red2)
rgl.snapshot("./data/torus1000_red.png")
torus1000_red_diag2<-ripsDiag(torus1000_red2, maxdimension = 2, maxscale = 3)
plot(torus1000_red_diag2[[1]])

torus1000_red_pd<-compute_pd(torus1000_red2, maxdimension = 2, maxscale = 3)
autoplot(torus1000_red_pd)

#アニュラス試し2
anu2<-anulusUnif(500)
plot(anu2)

anu2_cell<-cell_set2(anu2, 0.3)
anu2_cnct<-connect2(1, anu2_cell, all = 1:nrow(anu2))
anu2_red<-reduce_points(anu2, anu2_cnct)
plot(anu2_red)

anu2_diag<-ripsDiag(anu2, maxdimension = 1, maxscale = 3)
plot(anu2_diag[[1]])

anu_red3_diag<-ripsDiag(anu_red2, maxdimension = 1, maxscale = 3)
plot(anu_red3_diag[[1]])

anu2_cell2<-cell_set2(anu2, 0.2)
anu2_cnct2<-connect2(1, anu2_cell2, all = 1:nrow(anu2))
anu2_red2<-reduce_points(anu2, anu2_cnct2)
plot(anu2_red2)

anu2_red2_diag<-ripsDiag(anu2_red2, maxdimension = 1, maxscale = 3)
plot(anu2_red2_diag[[1]])

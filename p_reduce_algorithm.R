#ポイント削減のアルゴリズムの改良実験
##時間がかかりすぎるので

#100点アニュラスで実験
anu_dist_mat<-as.matrix(anu_dist)

simplex_pair<-which(anu_dist_mat < 1 & anu_dist_mat != 0, arr.ind = T)
mixplex<-1:nrow(anu) %>% as.list

for (i in 1:nrow(simplex_pair)) {
  n <- simplex_pair[i, 1]
  pair <- simplex_pair[i, 2]
  mixplex[[n]] <- c(mixplex[[n]], pair)
}

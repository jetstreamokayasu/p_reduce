connect<-function(i, cell_p, all, cnct=list(c(0, 0))){
  
  if(i==1){
    
    cnct[[1]]<-c(i, cell_p[which(cell_p[, 1]==i), 2])
    #debugText(cnct)
  }
  else{
    
    diff<-setdiff(c(i, cell_p[which(cell_p[, 1]==i), 2]), unlist(cnct))
    #debugText(diff)
    cnct<-c(cnct, list(diff))
    
    }

  #debugText(i, cnct)
  
  diffset<-setdiff(all, unlist(cnct))
  
  if(length(diffset) != 0){
    
    j<-diffset[1]
    #debugText(j)
    cnct<-connect(j, cell_p, all, cnct)
    
  }
  
  return(cnct)
  
}

cell_set<-function(x, thresh){
  
  cell_p<-c(0, 0)
  cell_s<-0
  i_p<-1
  i_s<-1
  x_dist<-dist(x)
  
  for (j in 1:nrow(x)) {

    i_j<-i_p
    
    for (k in 1:nrow(x)) {
      
      if(as.matrix(x_dist)[j, k] <= thresh && j!=k ){
        debugText(j, k)
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
  
  return(list(cell_p=cell_p, cell_s=cell_s))
  
}

reduce_points<-function(x, conect){
  reduc<-0
  for (i in 1:length(conect)) {
    if(length(conect[[i]])!=1){
      debugText(conect[[i]])
      mean_cnct<-apply(x[conect[[i]], ], 2, mean)
      cnct_dist<-rbind(mean_cnct, x[conect[[i]], ]) %>% 
                 dist()  %>% 
                 as.matrix() %>% 
                .[1, -1] %>% 
                 as.vector()
      
      remain<-conect[[i]][which.min(cnct_dist)]
      debugText(setdiff(conect[[i]], remain), remain)
      if(length(reduc)==1){reduc<-setdiff(conect[[i]], remain)}
      else{reduc<-c(reduc, setdiff(conect[[i]], remain))}

    }
  }
  y<-x[-reduc, ]
  
  return(y)
  
}

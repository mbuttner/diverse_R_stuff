order_by_reference <- function(unsorted, reference, reference2=NULL){
  if(is.null(dim(unsorted))){
    result <- sapply(reference, function(x,y){which(y == x)},unsorted)
  }else{
    if(is.null(reference2)){
      stop('The unsorted data has several rows or columns, but reference2 is not defined.')
    }
    result <- sapply(reference, function(ref1,y,ref2){which(y[,1] == ref1)[order_by_reference(y[which(y[,1] == ref1),2], ref2)]},unsorted, reference2)
  }

  return(as.vector(result))
}

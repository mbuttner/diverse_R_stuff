#merge two data sets whose information should be combined
#input: dataset 1, dataset 2

heatmap_with_triangles <- function(dataset1, dataset2){

  library(data.table)

  cols.ds1 <- colnames(dataset1)
  cols.ds2 <- colnames(dataset2)
  if (sum(cols.ds1[1:2] %in% cols.ds2[1:2])<2){
    stop('The first two columns do not match by name. Abort.')
  }

  merge.ds <- merge(dataset1, dataset2, by=cols.ds1[1:2])
  melt.ds <- melt(merge.ds, id.vars=c(cols.ds1[1:2]))
  melt.ds <- melt.ds[order(melt.ds[[cols.ds1[1]]], melt.ds[[cols.ds1[2]]]),]

  levels.x <- droplevels(unique(melt.ds[[cols.ds1[1]]]))
  levels.y <- droplevels(unique(melt.ds[[cols.ds1[2]]]))

  if(2*length(levels.x)*length(levels.y) != dim(melt.ds)[1]){
    stop('Incomplete pairs.')
  }

  groups <- rep(1:(2*length(levels.x)*length(levels.y)), each=3)

  coords.x <- 0:(length(levels.x)-1)
  coords.y <- 0:(length(levels.y)-1)

  square.coords <- expand.grid(coords.y, coords.x)
  triangle.coords <- apply(square.coords, 1, function(x){create_triangles_from_square(x[1], x[2])})
  triangle.coords <- melt(triangle.coords, id.vars=c('x','y'))
  triangle.coords$group <- groups
  triangle.coords[[cols.ds1[1]]] <- rep(melt.ds[[cols.ds1[1]]], each=3)
  triangle.coords[[cols.ds1[2]]] <- rep(melt.ds[[cols.ds1[2]]], each=3)
  triangle.coords[['variable']] <- rep(melt.ds[['variable']], each=3)
  triangle.coords[['value']] <- rep(melt.ds[['value']], each=3)

  return(triangle.coords)
}


create_triangles_from_square <- function(x_min,  y_min, x_max=NULL, y_max=NULL){
  if(is.null(x_max)){
    x_max = x_min +1
  }
  if(is.null(y_max)){
    y_max=y_min+1
  }
  x.upper <- c(x_min, x_min, x_max)
  x.lower <- c(x_min, x_max, x_max)
  y.upper <- c(y_min, y_max, y_max)
  y.lower <- c(y_min, y_min, y_max)

  df <- data.frame(x=c(x.upper, x.lower), y=c(y.upper,y.lower))
  return(df)
}

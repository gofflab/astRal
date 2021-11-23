# utils.R - Utility functions in suport of astRal\

# Check if class is a sparseMatrix from Matrix package (Borrowed from monocle3)
is_sparse_matrix <- function(x){
  class(x) %in% c("dgCMatrix", "dgTMatrix")
}


#' myColoRamp utility
#'
#' @param values A vector of expression values to map to palette
#' @param palette A color palette
#'
#' @return
#' @export
#' @import viridis
# #' @examples
#'
myColorRamp <- function(values,palette=viridis(255)) {
  values[is.na(values)]<-0
  #print(values)
  if(min(values)<0){
    values<-values+abs(min(values))
  }
  v <- (values - min(values))/diff(range(values))
  #v <- values/diff(range(values))
  #print(v)
  x <- colorRamp(palette)(v)
  #print(x)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

myColorRampTricycle <- function(values,palette=c("#2E22EA", "#9E3DFB", "#F86BE2", "#FCCE7B", "#C4E416", "#4BBA0F", "#447D87", "#2C24E9")) {
  values[is.na(values)]<-0
  #print(values)
  if(min(values)<0){
    values<-values+abs(min(values))
  }
  v <- (values - min(values))/diff(range(values))
  #v <- values/diff(range(values))
  #print(v)
  x <- colorRamp(palette)(v)
  #print(x)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

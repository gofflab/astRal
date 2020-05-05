# utils.R - Utility functions in suport of astRal\

# Check if class is a sparseMatrix from Matrix package (Borrowed from monocle3)
is_sparse_matrix <- function(x){
  class(x) %in% c("dgCMatrix", "dgTMatrix")
}

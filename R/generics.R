#' Generic to extract clusters from SpatialExperiment object
#'
#' @param x A SpatialExperiment object.
#' @param reduction_method Reduced dimension to extract clusters for.
#' @export
setGeneric("clusters", function(x, reduction_method = "UMAP")
  standardGeneric("clusters"))

#' Method to extract clusters from CDS object
#' @param x A cell_data_set object.
#' @param reduction_method Reduced dimension to extract clusters for.
#'
#' @export
setMethod("clusters", "SpatialExperiment",
          function(x, reduction_method = "UMAP") {
            value <- x@clusters[[
              reduction_method]]$clusters[colnames(exprs(x))]
            if (is.null(value)) {
              stop(paste0("No clusters calculated for reduction_method = ",
                          reduction_method, ". Please first run ",
                          "cluster_cells with reduction_method = ",
                          reduction_method, "."))
            }
            return(value)
          })

# Set of wrappers for easy transition from monocle.

#' Generic to access SpatialExperiment count matrix
#' @param x A SpatialExperiment object.
#'
#' @export
setGeneric("exprs", function(x) standardGeneric("exprs"))

#' Method to access SpatialExperiment count matrix
#' @param x A SpatialExperiment object.
#'
#' @export
setMethod("exprs", "SpatialExperiment", function(x) {
  value <- assays(x)$counts
  return(value)
})

#' Generic to access SpatialExperiment colData table
#' @param x A SpatialExperiment object.
#'
#' @export
setGeneric("pData", function(x) standardGeneric("pData"))

#' Generic to set SpatialExperiment colData table
#' @param x A SpatialExperiment object.
#' @param value A data frame to set to colData table.
#'
#' @export
setGeneric("pData<-", function(x, value) standardGeneric("pData<-"))

#' Method to access SpatialExperiment colData table
#' @param x A SpatialExperiment object.
#'
#' @export
setMethod("pData", "SpatialExperiment", function(x) {
  value <- colData(x)
  return(value)
})

#' Method to set SpatialExperiment colData table
#' @param x A SpatialExperiment object.
#' @param value A data frame to set to colData table.
#'
#' @export
#' @importClassesFrom S4Vectors List
setReplaceMethod("pData", "SpatialExperiment", function(x, value) {
  colData(x) <- value
  methods::validObject(x)
  return(x)
})


#' Generic to access SpatialExperiment rowData table
#' @param x A SpatialExperiment object.
#'
#' @export
setGeneric("fData", function(x) standardGeneric("fData"))

#' Generic to set SpatialExperiment rowData table
#' @param x A SpatialExperiment object.
#' @param value A data frame to set to colData table.
#'
#' @export
setGeneric("fData<-", function(x, value) standardGeneric("fData<-"))

#' Generic to access SpatialExperiment rowData table
#' @param x A SpatialExperiment object.
#'
#' @export
setMethod("fData", "SpatialExperiment", function(x) {
  value <- rowData(x)
  return(value)
})

#' Method to set SpatialExperiment rowData table
#' @param x A SpatialExperiment object.
#' @param value A data frame to set to colData table.
#'
#' @export
#' @importClassesFrom S4Vectors List
setReplaceMethod("fData", "SpatialExperiment", function(x, value) {
  rowData(x) <- value
  methods::validObject(x)
  return(x)
})

#' The SpatialExperiment class
#'
#' @field images SimpleList of images associated with each slide or assay
#' @field clusters SimpleList of cluster information for different
#'   dimensionality reduction.
#' @name SpatialExperiment
#' @rdname SpatialExperiment
#' @aliases SpatialExperiment-class
#' @exportClass SpatialExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment colData rowData
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @importFrom SingleCellExperiment reducedDim<- reducedDim reducedDims<-
#' @importFrom SingleCellExperiment reducedDims
#' @importFrom SummarizedExperiment Assays colData<- rowData<- assays assays<-
#' @importFrom S4Vectors metadata metadata<- SimpleList
setClass("SpatialExperiment",
         slots = c(
           images = "SimpleList",
           spatialMap = "SimpleList",
           clusters = "SimpleList"
         ),
         contains="SingleCellExperiment",
)

#' Create a new SpatialExperiment object.
#'
#' @param expression_data expression data matrix for an experiment, can be a
#'   sparseMatrix.
#' @param barcode_metadata data frame containing attributes of individual pixels/voxels/barcodes,
#'  where \code{row.names(barcode_metadata) = colnames(expression_data)}.
#' @param gene_metadata data frame containing attributes of features
#'   (e.g. genes), where
#'   \code{row.names(gene_metadata) = row.names(expression_data)}.
#' @return a new SpatialExperiment object
#' @export
#' @examples
#'
#'
NewSpatialExperiment <- function(expression_data,
                              barcode_metadata = NULL,
                              gene_metadata = NULL) {

  assertthat::assert_that(class(expression_data) == "matrix" ||
                            is_sparse_matrix(expression_data),
                          msg = paste("Argument expression_data must be a",
                                      "matrix - either sparse from the",
                                      "Matrix package or dense"))
  if (!is.null(barcode_metadata)) {
    assertthat::assert_that(nrow(barcode_metadata) == ncol(expression_data),
                            msg = paste("barcode_metadata must be NULL or have",
                                        "the same number of rows as columns",
                                        "in expression_data"))
    assertthat::assert_that(!is.null(row.names(barcode_metadata)) &
                              all(row.names(barcode_metadata) == colnames(expression_data)),
                            msg = paste("row.names of barcode_metadata must be equal to colnames of",
                                        "expression_data"))
  }

  if (!is.null(gene_metadata)) {
    assertthat::assert_that(nrow(gene_metadata) == nrow(expression_data),
                            msg = paste("gene_metadata must be NULL or have",
                                        "the same number of rows as rows",
                                        "in expression_data"))
    assertthat::assert_that(!is.null(row.names(gene_metadata)) & all(
      row.names(gene_metadata) == row.names(expression_data)),
      msg = paste("row.names of gene_metadata must be equal to row.names of",
                  "expression_data"))
  }

  if (is.null(barcode_metadata)) {
    barcode_metadata <- data.frame(barcode = colnames(expression_data),
                                row.names = colnames(expression_data))
  }

  if(!('gene_short_name' %in% colnames(gene_metadata))) {
    warning(paste("Warning: gene_metadata must contain a column verbatim",
                  "named 'gene_short_name' for certain functions."))
  }

  sce <- SingleCellExperiment(list(counts=methods::as(expression_data, "dgCMatrix")),
                              rowData = gene_metadata,
                              colData = barcode_metadata)

  spat <- methods::new("SpatialExperiment",
                      assays = SummarizedExperiment::Assays(
                        list(counts=methods::as(expression_data, "dgCMatrix"))),
                      colData = colData(sce),
                      int_elementMetadata = int_elementMetadata(sce),
                      int_colData = int_colData(sce),
                      int_metadata = int_metadata(sce),
                      metadata = metadata(sce),
                      NAMES = NULL,
                      elementMetadata = elementMetadata(sce)[,0],
                      rowRanges = rowRanges(sce))

  metadata(spat)$SE_version <- "0.1.0"
  clusters <- stats::setNames(SimpleList(), character(0))
  #spat <- estimate_size_factors(spat)
  spat
}


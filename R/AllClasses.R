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
#' @importFrom SingleCellExperiment reducedDims LinearEmbeddingMatrix
#' @importFrom SummarizedExperiment Assays colData<- rowData<- assays assays<-
#' @import sf
# #' @importFrom sp SpatialPoints SpatialPointsDataFrame
#' @importFrom S4Vectors metadata metadata<- SimpleList
setClass("SpatialExperiment",
         slots = c(
           images = "SimpleList",
           spatialMap = "sf",
           clusters = "SimpleList",
           embeddings = "SimpleList" #simpleList
           #dimReductions = "SimpleList"

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
                              spatial_coords,
                              barcode_metadata = NULL,
                              gene_metadata = NULL) {

  assertthat::assert_that(class(expression_data) == "matrix" ||
                            is_sparse_matrix(expression_data),
                          msg = paste("Argument expression_data must be a",
                                      "matrix - either sparse from the",
                                      "Matrix package or dense"))

  #assert that spatial_coords is a data.frame, matrix, or SpatialPixelsDataFrame object
  assertthat::assert_that(any(class(spatial_coords) %in% c("data.frame","matrix","sf")),
                          msg = paste("Argument spatial_coords must be a",
                                      "matrix, data.frame,",
                                      "or object of class sf"))

  #assert that columns in expression data and rows in spatial_coords are equal
  assertthat::assert_that(
                          dim(expression_data)[2] == dim(spatial_coords)[1],
                          msg = "Rows in spatial_coords must match coumns in expression_data")

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
    barcode_metadata <- data.frame(spatial_feature_id = colnames(expression_data),
                                row.names = colnames(expression_data))
  }

  if (is.null(gene_metadata)) {
    gene_metadata <- data.frame(feature_id = rownames(expression_data),
                                   row.names = make.names(rownames(expression_data),unique=TRUE)) #TODO: enforce unique row names up front and or enforce gene_metadata!=NULL. Hacky but works for now
  }

  if(!('gene_short_name' %in% colnames(gene_metadata))) {
    warning(paste("Warning: gene_metadata must contain a column verbatim",
                  "named 'gene_short_name' for certain functions."))
  }

  #TODO: Only borrowing this approach until we can create new SpatialExperiment directly...
  sce <- SingleCellExperiment(list(counts=methods::as(expression_data, "dgCMatrix")),
                              rowData = gene_metadata,
                              colData = barcode_metadata)

  #TODO: make sure that non 'sf' objects passed to 'spatial_coords' are appropriately converted to class 'sf' and pass validation.
  #spatialMap <- SpatialPointsDataFrame(spatial_coords,data.frame(barcode=colnames(sce)))
  spatialMap <- spatial_coords

  spat <- methods::new("SpatialExperiment",
                      assays = SummarizedExperiment::Assays(
                        list(counts=methods::as(expression_data, "dgCMatrix"))),
                      colData = colData(sce),
                      spatialMap = spatialMap,
                      int_elementMetadata = int_elementMetadata(sce),
                      int_colData = int_colData(sce),
                      int_metadata = int_metadata(sce),
                      metadata = metadata(sce),
                      NAMES = NULL,
                      elementMetadata = elementMetadata(sce)[,0],
                      rowRanges = rowRanges(sce))

  metadata(spat)$SE_version <- "0.1.1"
  #clusters <- stats::setNames(SimpleList(), character(0))
  #spat <- estimate_size_factors(spat)
  spat
}

# Dimensionality reduction methods for paired spatial and transcriptional datasets

#' Title
#'
#' @param se A SpatialExperiment object
#' @param method Pattern discovery method.  Must be one of c("NMF").  [default: "NMF"]
#' @param rank Where applicable, the number of dimensions to reduce into (e.g. 'k') [default: 20]
#' @param verbose A logical indicating verbose output. [default: FALSE]
#' @param ...
#'
#' @return
#' @importFrom RcppML nmf
#' @export
#'
reduce_dimension_spatial<-function(se,
                    method=c("NMF"),
                    rank = 20, #arbitrary
                    verbose=TRUE,
                    ...){
  extra_arguments <- list(...)

  if(method=="NMF"){
    se.nmf<-nmf(as.matrix(log10(exprs(se))),k=rank)
    se.lem<-LinearEmbeddingMatrix(sampleFactors=t(se.nmf$h),
                                  featureLoadings=se.nmf$w,
                                  metadata = list("method"="NMF")
                                  )
    se@embeddings[[method]]<-se.lem
  }
  return(se)
}

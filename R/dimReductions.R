# Dimensionality reduction methods for paired spatial and transcriptional datasets

#' Title
#'
#' @param se A SpatialExperiment object
#' @param method Pattern discovery method.  Must be one of c("NNMF").  [default: "NNMF"]
#' @param rank Where applicable, the number of dimensions to reduce into (e.g. 'k') [default: 20]
#' @param verbose A logical indicating verbose output. [default: FALSE]
#' @param ...
#'
#' @return
#' @importFrom NNLM nnmf
#' @export
#'
reduce_dimension_spatial<-function(se,
                    method=c("NNMF"),
                    rank = 20, #arbitrary
                    verbose=TRUE,
                    ...){
  extra_arguments <- list(...)

  if(method=="NNMF"){
    se.nmf<-nnmf(as.matrix(log10(exprs(se))),k=rank)
    se.lem<-LinearEmbeddingMatrix(sampleFactors=t(se.nmf$H),
                                  featureLoadings=se.nmf$W,
                                  metadata = list("method"="NNMF")
                                  )
    se@embeddings[[method]]<-se.lem
  }
  return(se)
}

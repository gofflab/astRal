# Visualizations for Astral projections

#' Title
#'
#' @param se A SpatialExperiment Object
#' @param color_by A column of pData(se) used to color points (default:NULL)
#' @param shape_by A column of pData(se) used as shape aesthetic for points (default:NULL)
#' @param plot_images A logical value to determine whether or not section images should be plotted (default: FALSE)
#'
#' @return
#' @export
#' @import ggplot2
# #' @examples
#'
spatial_plot<-function(se,color_by=NULL,shape_by=NULL,plot_images=FALSE){
  coords<-coordinates(se@spatialMap)
  colnames(coords)<-c("x","y")
  tmp<-as.data.frame(cbind(coords,pData(se)))
  p <- ggplot(tmp,aes(x=x,y=y))

    if(!is.null(color_by)){
      p<- p + geom_point(aes_string(color=color_by))

    } else {
      p<- p + geom_point()
    }

    p<- p + monocle:::monocle_theme_opts()
    p
}


#xlim<-c(1,max(st_bbox(aba_ccf)))
#ylim<-xlim
#zlim<-ylim

#aspect_ratio<-aba_array_dims()/aba_array_dims()[1]



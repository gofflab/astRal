# Visualizations for Astral projections

#' 2D spatial plot
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
  coords<-sf::st_coordinates(se@spatialMap)
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


#' 3D spatial plot
#'
#' @param se A SpatialExperiment Object
#' @param color_by A column of pData(se) used to color points (default:NULL)
#' @param shape_by A column of pData(se) used as shape aesthetic for points (default:NULL)
#' @param plot_images A logical value to determine whether or not section images should be plotted (default: FALSE)
#'
#' @return
#' @export
#' @import rgl
#' @import sf
# #' @examples
#'
spatial_plot_3d<-function(se,color_by=NULL,shape_by=NULL,plot_images=FALSE){
  coords<-sf::st_coordinates(se@spatialMap)
  assertthat::assert_that(dim(coords)[2]==3
                          , msg = "SpatialMap must have 3 dimensional coordinates")

  colnames(coords)<-c("x","y","z")

  #tmp<-as.data.frame(cbind(coords,pData(se)))
  # p <- ggplot(tmp,aes(x=x,y=y,z=z))
  #
  if(!is.null(color_by)){
    expr_values<-as.numeric(exprs(se[color_by,]))
    plot_coords_idx<-expr_values>0
    coords<-coords[plot_coords_idx,]
    expr_values<-expr_values[plot_coords_idx]
    col = myColorRamp(palette=plasma(100),as.numeric(expr_values))
  } else {
    col="black"
  }

  xlim<-c(1,max(sf::st_bbox(se@spatialMap)))
  ylim<-xlim
  zlim<-ylim

  aspect_ratio<-aba_array_dims()/aba_array_dims()[1]
  rgl::plot3d(coords,aspect_ratio=aspect_ratio,col=col,xlim=xlim,ylim=ylim,zlim=zlim,box=F,axes=F,xlab="",ylab="",zlab="",size=3,main=color_by,alpha=(expr_values-min(expr_values))/(max(expr_values)-min(expr_values)))
}




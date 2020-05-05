# ABA Spatial

# CCF (200uM) Annotation Dimensions
#' Title
#'
#' @param resolution Resolution of the returned array dimensions ("200um" or "25um")
#'
#' @return
#' @export
#'
# #' @examples
aba_array_dims<-function(resolution=200){
  if(resolution==200){
    return(c(67, 41, 58))
  }else if(resolution==25){
    return(c(528, 320, 456))
  }
}

#' Get CCFv3 Annotation
#'
#' @param age One of c("Adult","E11","E13","E15","E18","P4","P14","P28","P56")
#' @param make_array Logical argument whether to return the result as a 3D array instead of an object of class 'sf'.
#' @param array_spacing numerical value indicating spacing between array point (usually 25 or 200) in uM.
#'
#' @importFrom plyr adply
#' @importFrom sfheaders sf_point
#' @return
#' @export
ccf_annotation<-function(age="Adult",make_array=FALSE,array_spacing=200){
  vol_dims<-aba_array_dims(array_spacing)
  print(vol_dims)
  baseFile <- "http://download.alleninstitute.org/informatics-archive/current-release/mouse_annotation/"
  if(age=="Adult"){
    file = paste0(baseFile,"P56_Mouse","_gridAnnotation.zip")
  }else if(age=="E11"){
    file = paste0(baseFile,"E11pt5","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="E13"){
    file = paste0(baseFile,"E13pt5","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="E15"){
    file = paste0(baseFile,"E15pt5","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="E18"){
    file = paste0(baseFile,"E18pt5","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="P4"){
    file = paste0(baseFile,"P4","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="P14"){
    file = paste0(baseFile,"P14","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="P28"){
    file = paste0(baseFile,"P28","_DevMouse2012_gridAnnotation.zip")
  }else if(age=="P56"){
    file = paste0(baseFile,"P56","_DevMouse2012_gridAnnotation.zip")
  }else{
    stop("Invalid age param")
  }

  # Download specific grid annotation
  tmp <- tempfile()
  download.file(file,tmp)
  raw_file <- unz(tmp, "gridAnnotation.raw", "rb")
  vol_raw <- readBin(raw_file, "integer", size = 4, n = prod(vol_dims))

  # Cleanup
  close(raw_file)
  file.remove(tmp)

  # Return results
  if(make_array){
    return(array(vol_raw, dim = vol_dims))
  } else {
    res <- plyr::adply(array(vol_raw, dim = vol_dims),1:3)
    colnames(res)<-c("X","Y","Z","Annot")

    #multiply by array_spacing value
    indx <- sapply(res, is.factor)
    res[indx] <- lapply(res[indx], function(x) as.numeric(as.character(x)))
    res[,1:3]<-res[,1:3]*array_spacing

    #convert to sfg
    res <- sf_point(res,x="X",y="Y",z="Z",keep=TRUE)
    return(res)
  }
}

#' Get Ontology from Allen
#'
#' @param species
#'
#' @return
#' @export
get_ontology<-function(species="mouse"){
  if(species=="mouse"){
    ont <-1
  }else if(species=="human"){
    ont <-2
  }else{
    stop("Species not in c('mouse','human')")
  }
  file <- paste0("http://api.brain-map.org/api/v2/structure_graph_download/",ont,".json")

  # Download ontology JSON
  tmp <- tempfile()
  download.file(file,tmp)
  ontology <- jsonlite::fromJSON(tmp)[["msg"]]
  return(ontology)
}

flatten_ontology <- function(ontology, ontology_df = NULL) {

  l <- ontology

  if(is.null(ontology_df)) {
    ontology_df <- data.frame(l[names(l) != "children"])[0,]
    ontology_df$n_children <- numeric()
  }

  if("children" %in% names(l)) {

    child_df <- data.frame(l[names(l) != "children"])

    n_children_of_children <- purrr::map_dbl(l$children,
                                             function(x) {
                                               if("children" %in% names(x)) {
                                                 length(x$children)
                                               } else {
                                                 0
                                               }
                                             })

    child_df$n_children <- n_children_of_children

    ontology_df <- rbind(ontology_df, child_df)

    for(i in 1:length(l$children)) {

      child_list <- l$children[[i]]

      ontology_df <- flatten_mba_ontology(child_list, ontology_df)
    }
  }

  return(ontology_df)
}



#Sagittal Energy Matrix
#ccf_v3_coords_file<-system.file("extdata","p56coord_dev_v3_left.RData",package="astRal")
#struct_file<-system.file("extdata","p56_dev_structureID_left.RData",package="astRal")
#group_file<-system.file("extdata","group.RData",package="astRal")
#load(ccf_v3_coords_file)
#load(struct_file)
#load(group_file)

#readBin(raw_file, "double", size = 4, n = vol_dims[1]*vol_dims[2]*vol_dims[3])

#
# library(cocoframer)
# library(purrr)
# library(viridisLite) # optional - nice color palettes
# library(dplyr)
# library(reshape2)
#
# # Get structure ontology annotations
# ga <- get_ccf_grid_annotation()
#
# ontology <- get_mba_ontology()
# ontology_df <- flatten_mba_ontology(ontology)
#
# # build a 3d array of ontology structure acronyms - easier to deal with than IDs
# oa <- array(ontology_df$acronym[match(ga, ontology_df$id)], dim = dim(ga))
#
# # Get ISH data
# Slc17a7_ids <- get_gene_aba_ish_ids("Slc17a7")


###
# API query examples
##########
# All sagittal ISH SectionDataSets from E11.5 Developing Mouse
#http://api.brain-map.org/api/v2/data/query.xml?criteria=model::SectionDataSet,rma::criteria,[failed$eq%27false%27],products[id$eq%273%27],specimen(donor(age[name$in%27E11.5%27])),plane_of_section[name$eq%27sagittal%27],rma::include,genes,specimen(donor(age)),plane_of_section





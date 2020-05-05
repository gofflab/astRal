# ## get annotation
# get.annot <- function(dir, file, bit) {
#   size <- get.size(dir, file)
#   ## read as 32 bit floating point
#   if(bit==32) {
#     dat <- readBin(paste(dir, "/", file, ".raw", sep=""), "integer", n=prod(size), size=4, endian="little") }
#   if(bit==8) {
#     dat <- readBin(paste(dir, "/", file, ".raw", sep=""), what = "integer", n=prod(size), size = 1, endian="little") }
#   return(dat)
# }
#
# ######
# ## Main
# ## Repeat for E11.5, E13.5, E15.5, E16.5, E18.5, P4, P14, P28, P56 ######
# mouse <- 'P56_dev'
# annot <- get.annot('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_DevMouse2012_annotation', file='annotation', 32)
# annotsize <- get.size('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_DevMouse2012_annotation',file='annotation')
# annot3D <- array(annot, dim=annotsize)
# ## Volume
# vol <- get.annot('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_atlasVolume', file='atlasVolume', 8)
# volsize <- get.size('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_atlasVolume', file='atlasVolume')
# vol3D <- array(vol, dim=volsize)
#
# ## Grid annotation
# gannot <- get.annot('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_DevMouse2012_gridAnnotation', file='gridAnnotation', 32)
# gannotsize <- get.size('C:/Users/25149/Documents/JHU/Rotation/?½??ļ???/GetUpdatedData/p56_dev_annotation_volume/P56_DevMouse2012_gridAnnotation', file='gridAnnotation')
# gannot3D <- array(gannot, dim=gannotsize)
# save(annot3D, vol3D, gannot3D, file=paste("RData/", mouse, ".annot.RData", sep=""))

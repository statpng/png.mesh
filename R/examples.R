#' @export png.data.nose
png.data.nose <- function(){
  library(Morpho)
  data(nose)
  mesh <- shortnose.mesh
  landmark <- shortnose.lm
  
  list(mesh=mesh, landmark=landmark)
}

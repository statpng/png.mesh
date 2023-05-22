#' @export png.plt2stl
png.plt2stl <- function(path){
  # path <- "/Volumes/T7/1.Mesh3d/temp1/voltmap.plt/"
  ListFiles <- list.files(path, "plt", full.names = TRUE)
  FileNames <- gsub(".plt", "", map_chr(ListFiles, ~strsplit(.x, "/")[[1]] %>% {.[length(.)]}))
  MESH <- png.read_plts( ListFiles, nmax=Inf, center=TRUE )
  MESH %>% map2( FileNames, ~ Rvcg::vcgStlWrite(.x, filename=paste0(gsub(".plt","",.y)) ) )
}



#' @export png.coord2mesh
png.coord2mesh <- function(nodes, edges, value=NULL, type="triangle"){
  nodes <- nodes[,1:3]
  edges <- edges[,1:3]
  
  data.table::setnames(nodes, c("x", "y", "z"))
  data.table::setnames(edges, c("x", "y", "z"))
  
  if(type == "triangle"){
    mesh <- rgl::tmesh3d(vertices = t(nodes[,1:3]), indices = t(edges))
  } else {
    mesh <- rgl::mesh3d(vertices = t(nodes[,1:3]), quads = t(edges))
  }
  
  if(!is.null(value)){
    mesh$value <- as.matrix(value)
  }
  
  mesh
  
}




#' @export png.df2mesh
png.df2mesh <- function(df, type="matrix"){
  df <- as.matrix(df[,1:3])
  colnames(df) <- letters[24:26]
  
  # devtools::install_github("stla/SurfaceReconstruction")
  mesh <- SurfaceReconstruction::AFSreconstruction( df )
  
  return(mesh)
}



#' @export png.mesh2df
png.mesh2df <- function(mesh, type="matrix"){
  if(type=="matrix"){
    out <- as.matrix( t(mesh$vb[1:3,]) )
  } else {
    out <- as.data.frame( t(mesh$vb[1:3,]) )
  }
  colnames(out) <- letters[24:26]
  
  return(out)
}




#' @export png.cuvia2mesh
png.cuvia2mesh <- function(fit.cuvia, type="triangle"){
  node <- fit.cuvia$node[,1:4]
  edge <- fit.cuvia$edge[,1:3]
  
  data.table::setnames(node, c("x", "y", "z", "value"))
  data.table::setnames(edge, c("x", "y", "z"))
  
  if(type == "triangle"){
    mesh <- rgl::tmesh3d(vertices = t(node[,1:3]), indices = t(edge))
  } else {
    mesh <- rgl::mesh3d(vertices = t(node[,1:3]), quads = t(edge))
  }
  
  mesh$value <- as.matrix(node[,4])
  
  mesh
  
}




#' @export png.cuvia2stl
png.cuvia2stl <- function(path, level=1){
  library(dplyr)
  path %>% 
    png.cuvia( level=level ) %>% 
    png.cuvia2mesh() %>% 
    Rvcg::vcgStlWrite(filename= paste0( "cuv2stl_lev", level, "_", strsplit(path,"/")[[1]] %>% {.[length(.)]} %>% gsub(".cuv", "", .) ) )
}

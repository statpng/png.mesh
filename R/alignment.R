#' @export png.plotly.add_landmark
png.plotly.add_landmark <- function(p, landmark, size = 4) {
  for (i in 1:nrow(landmark)) {
    x <- landmark[i, 1]
    y <- landmark[i, 2]
    z <- landmark[i, 3]
    
    params <- list(symbol = 3, size = size, color = "red")
    p <- plotly::add_markers(p, x, y, z, marker = params)
  }
  
  return(p)
}


#' @export png.align_landmark
png.align_landmark <- function(mesh, landmark) {
  # example: png.landmark(shortnose.mesh, shortnose.lm)
  
  df <- png.mesh2df(mesh)
  
  out <- NULL
  for (i in 1:nrow(landmark)) {
    sum_abs <- apply(df, 1, function(x) sum(abs(x - landmark[i,])))
    out[[i]] <- list(min = min(sum_abs), wh.min = which.min(sum_abs))
  }
  
  landmark_new <- df[map_int(out, "wh.min"),]
  return(landmark_new)
}



#' @export png.compare_icp
png.compare_icp <- function(mesh.org, mesh.target, landmark.org, landmark.target, use.icp=TRUE, radius=c(1,1,1), iterations=100, seed=1234){
  # Example
  
  library(rgl)
  
  n.org <- nrow(landmark.org)
  n.target <- nrow(landmark.target)
  n.min <- min( n.org, n.target )
  
  if (n.org != n.target) {
    set.seed(seed)
    landmark.org <- landmark.org[sample(n.org, n.min), ]
    set.seed(seed)
    landmark.target <- landmark.target[sample(n.target, n.min), ]
  }
  
  if(use.icp){
    icp <- Morpho::icpmat(landmark.org, landmark.target, iterations = iterations)
  }
  mesh.rot <- rotmesh.onto(mesh.org, landmark.org, icp, scale = TRUE, reflection = TRUE)
  
  open3d()
  mesh.org %>% shade3d(col = "grey90")
  spheres3d(landmark.org, col = "red", radius = radius[1])
  
  open3d()
  mesh.target %>% shade3d(col = "blue")
  spheres3d(landmark.target, col = "red", radius = radius[2])
  
  open3d()
  mesh.rot$mesh %>% shade3d(col = "green")
  mesh.rot$yrot %>% spheres3d(col = "red", radius = radius[3])
  
  return( list(mesh.rot=mesh.rot$mesh, landmark.rot=mesh.rot$yrot, Q=mesh.rot$trafo) )
}





#' @export png.mesh3d.change_columns
png.mesh3d.change_columns <- function(mesh, newcol = 1:3) {
  mesh$vb <- mesh$vb[c(newcol, 4),]
  mesh$it <- mesh$it[newcol,]
  mesh$normals <- mesh$normals[c(newcol, 4),]
  return(mesh)
}


#' @export png.create_rotmat3d
png.create_rotmat3d <- function(angle = 0) {
  rotation_angle_radians <- angle * (pi / 180)
  Q <- matrix(c(cos(rotation_angle_radians), -sin(rotation_angle_radians), 0,
                sin(rotation_angle_radians), cos(rotation_angle_radians), 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  return(Q)
}


#' @export png.mesh3d.rotate
png.mesh3d.rotate <- function(mesh, angle = 0) {
  Q <- png.create_rotmat3d(angle)
  mesh$vb[1:3,] <- t( t(mesh$vb[1:3,]) %*% Q )
  return(mesh)
}


#' @export trans_3d_2d
trans_3d_2d <- function(data, theta=135, phi=60) {
  pmat <- plot3D::perspbox(z=diag(2), plot=F, theta=theta, phi=phi)
  
  data <- as.data.frame(data)
  XY <- plot3D::trans3D(
    x = data$x,
    y = data$y,
    z = data$z,
    pmat = pmat) %>%
    data.frame()
  
  data$x <- XY$x
  data$y <- XY$y
  
  return(data[, c('x', 'y')])
}


#' @export png.mesh3d.scatter2d
png.mesh3d.scatter2d <- function(mesh, theta=135, phi=60, alpha=0.1){
  library(plot3D)
  library(ggplot2)
  
  if (inherits(mesh, "mesh3d")) {
    df <- as.matrix(t(mesh$vb[1:3,]))
  }
  colnames(df) <- c("x", "y", "z")
  
  
  trans_3d_2d(df[,1:3], theta=theta, phi=phi) %>% 
    ggplot(aes(x, y, color=x)) + geom_point(alpha=alpha)  + 
    coord_flip() + 
    theme(legend.position="none") + 
    scale_y_reverse()
  
}



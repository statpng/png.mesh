#' @export png.plotly.scatter3d
png.plotly.scatter3d <- function(mesh, size = 2) {
  if (inherits(mesh, "mesh3d")) {
    df <- t(mesh$vb[1:3,])
  } else {
    df <- mesh
  }
  colnames(df) <- c("x", "y", "z")
  
  p <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', data = as.data.frame(df), 
               marker = list(symbol = 3, size = size, color = "grey70",
                             line = list(color="grey10", width=1) ) )
  
  return(p)
}



#' @export png.mesh3d.plot
png.mesh3d.plot <- function(mesh, type="wire", landmark = NULL, radius = 0.1) {
  library(SurfaceReconstruction)
  library(rgl)
  
  if (!inherits(mesh, "mesh3d")) {
    mesh <- SurfaceReconstruction::AFSreconstruction(as.matrix(mesh[, 1:3]))
  }
  
  open3d()
  if( type == "shade" ){
    shade3d(mesh, color = "grey70")
  } else if( type == "wire" ){
    wire3d(mesh, color = "grey70")
  }
  
  
  if (!is.null(landmark)) {
    spheres3d(landmark, col = "red", radius = radius)
  }
  
}






#' @export png.flat.plot1
png.flat.plot1 <- function(df, col_name = "LAT", type="wide", size=0.5, ...){
  
  library(ggplot2)
  if(type == "long"){
    df %>% filter(measure == col_name) %>% 
      ggplot(aes(x,y,col=value)) +
      geom_point(size = size, alpha=1, ...) +
      # scale_color_gradient2(low="red", high="blue", mid="yellow", na.value="grey90",
      # midpoint=mean(df_value, na.rm=TRUE)) + 
      scale_color_gradientn(name=col_name,
                            colours = ( RColorBrewer::brewer.pal(5,c("Spectral","RdYlBu","RdYlGn")[1]) %>% rev ), 
                            na.value="grey90") +
      xlab("")+ylab("")+
      theme_classic()
  } else {
    df %>% 
      ggplot(aes(x,y,col=get(col_name))) +
      geom_point(size = size, alpha=1) +
      # scale_color_gradient2(low="red", high="blue", mid="yellow", na.value="grey90",
      # midpoint=mean(df_value, na.rm=TRUE)) + 
      scale_color_gradientn(name=col_name,
                            colours = ( RColorBrewer::brewer.pal(5,c("Spectral","RdYlBu","RdYlGn")[1]) %>% rev ), 
                            na.value="grey90") +
      xlab("")+ylab("")+
      theme_classic()
  }
}





#' @export png.flat.plot2
png.flat.plot2 <- function(nodes_long, file=NULL, ...){
  if(FALSE){
    nodes_long <- nodes_raw_long
  }
  
  library(cowplot)
  nodes_long %>% 
    group_split(measure) %>% 
    map(
      ~ggplot(., aes(x, y, color = value)) + 
        geom_point(size = 0.5, alpha=1) +
        scale_color_gradientn(name="Intensity",
                              colours = RColorBrewer::brewer.pal(5,c("Spectral","RdYlBu","RdYlGn")[1]) %>% rev, 
                              na.value="grey90") +
        facet_wrap(~ measure, labeller = function(x) label_both(x, multi_line = FALSE)) + 
        xlab("")+ylab("")+
        theme_classic() #+ 
      # scale_shape_manual(values=c(24,25,2,3,4,6,8)[1:5])
    ) %>% 
    plot_grid(plotlist = ., align = 'hv', ncol = 2)
  
  if(!is.null(title)){
    ggsave(filename=file, ...)
  }
}



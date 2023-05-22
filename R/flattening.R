#' @export png.flat.remove_outlier
png.flat.remove_outlier <- function(df, radius=0.5, type="missing"){
  
  # df <- df_flat
  nodes <- df$nodes[,1:3]
  edges <- df$edges[,1:3]
  x <- nodes[,1]
  y <- nodes[,2]
  z <- 0 # nodes[,3]
  outliers <- which(x^2 + y^2 + z^2 > 0.5^2+1e-4)
  # nonoutliers <- (1:nrow(nodes))[-outliers]
  nodes_filtered <- df$nodes
  nodes_filtered[outliers,] <- NA
  edges_filtered <- edges %>% filter_all( all_vars(!. %in% outliers) )
  
  edges %>% dim %>% print
  edges_filtered %>% dim %>% print
  
  
  if(type == "na.omit"){
    n_nodes <- nrow(nodes_filtered)
    
    new_idx_for_edges <- sapply(1:n_nodes, function(x) x-sum(outliers<x))
    
    n_edges <- nrow(edges_filtered)
    edges_vec <- unlist(edges_filtered)
    edges_filtered <- matrix(new_idx_for_edges[edges_vec], nrow=n_edges, ncol=3, byrow=FALSE) %>% as.data.frame
    nodes_filtered <- nodes_filtered %>% na.omit %>% as.data.frame
  }
  
  list(nodes=nodes_filtered, edges=edges_filtered)
}







# Inverse Distance Weighting
#' @export interpolate_data_IDW
interpolate_data_IDW <- function(person1_data, person2_data, value_column, grid_resolution = 100) {
  library(sp)
  library(gstat)
  
  radius <- 0.5
  # Create a common grid
  x_range <- seq(-radius, radius, length.out = grid_resolution)
  y_range <- seq(-radius, radius, length.out = grid_resolution)
  grid <- expand.grid(x = x_range, y = y_range)
  
  # Filter the grid to keep only points within the circle x^2 + y^2 <= 1
  grid <- grid[grid$x^2 + grid$y^2 <= radius^2, ]
  
  # Define the spatial data
  coordinates(person1_data) <- ~x + y
  coordinates(person2_data) <- ~x + y
  coordinates(grid) <- ~x + y
  
  # Perform IDW interpolation for each person's data
  idw1 <- idw(as.formula(paste(value_column, "~ 1")), person1_data, grid)
  idw2 <- idw(as.formula(paste(value_column, "~ 1")), person2_data, grid)
  
  # Convert the results to data frames
  idw1_df <- data.frame(coordinates(idw1), z=0, value = idw1@data$var1.pred)
  idw2_df <- data.frame(coordinates(idw2), z=0, value = idw2@data$var1.pred)
  
  return(list(person1 = idw1_df, person2 = idw2_df))
}


# K-nearest neighbours
#' @export interpolate_data_KNN
interpolate_data_KNN <- function(person1_data, person2_data, value_column, k = 5, grid_resolution = 50, is.factor=FALSE) {
  if(FALSE){
    person1_data=df_new
    person2_data=df_new
    value_column="auto_region"
    k = 3
    grid_resolution = 50
    is.factor=TRUE
  }
  
  library(sp)
  library(FNN)
  
  radius <- 0.5
  # Create a common grid
  x_range <- seq(-radius, radius, length.out = grid_resolution)
  y_range <- seq(-radius, radius, length.out = grid_resolution)
  grid <- expand.grid(x = x_range, y = y_range)
  
  # Filter the grid to keep only points within the circle x^2 + y^2 <= 1
  grid <- grid[grid$x^2 + grid$y^2 <= radius^2, ]
  
  # Define the spatial data
  coordinates(person1_data) <- ~x + y
  coordinates(person2_data) <- ~x + y
  coordinates(grid) <- ~x + y
  
  # Perform kNN interpolation for each person's data
  if( !is.factor ){
    knn1 <- knn.reg(train = person1_data@coords[, c("x", "y")], test = grid %>% as.data.frame, y = person1_data[[value_column]], k = k)
    knn2 <- knn.reg(train = person2_data@coords[, c("x", "y")], test = grid %>% as.data.frame, y = person2_data[[value_column]], k = k)
    
    # Convert the results to data frames
    knn1_df <- data.frame(coordinates(grid), z=0, value = knn1$pred)
    knn2_df <- data.frame(coordinates(grid), z=0, value = knn2$pred)
    
  } else {
    knn1 <- knn(train = person1_data@coords[, c("x", "y")], test = grid %>% as.data.frame, cl = person1_data[[value_column]], k = k)
    knn2 <- knn(train = person2_data@coords[, c("x", "y")], test = grid %>% as.data.frame, cl = person2_data[[value_column]], k = k)
    
    # Convert the results to data frames
    knn1_df <- data.frame(coordinates(grid), z=0, value = knn1)
    knn2_df <- data.frame(coordinates(grid), z=0, value = knn2)
    
  }
  
  
  
  return(list(person1 = knn1_df, person2 = knn2_df))
}







#' @export png.flat.interpolate
png.flat.interpolate <- function(df_nodes, method="KNN", k=3, col_factor=NULL, long=FALSE, grid_resolution=100){
  if(FALSE){
    df_nodes <- data_flat$flat$nodes
    method="KNN"
    k=3
    col_factor=c("region", "auto_region")
    grid_resolution=50
    long=FALSE
  }
  
  
  col_names <- colnames(df_nodes) %>% {.[!. %in% c(letters[24:26], paste0(letters[24:26], "_3d"))]}
  
  out <- as.list(1:length(col_names))
  names(out) <- col_names
  for( i in 1:length(col_names) ){
    col <- col_names[i]
    
    is.factor <- ifelse( col %in% col_factor, TRUE, FALSE )
    
    df_new <- df_nodes %>% select(x,y,col)
    wh.missing <- which(is.na(df_new[col]))
    if(length(wh.missing)>0){
      df_new <- df_new[-wh.missing,]
    }
    
    if( method == "KNN" ){
      fit <- interpolate_data_KNN(df_new, df_new, col, k=k, grid_resolution=grid_resolution, is.factor)[[1]]
    } else if( method == "IDW" ) {
      fit <- interpolate_data_IDW(df_new, df_new, col, grid_resolution=grid_resolution, is.factor)[[1]]
    }
    
    colnames(fit)[colnames(fit) == "value"] <- col
    if( i > 1 ){
      fit <- fit[,col]
    }
    
    out[[i]] <- fit
  }
  
  out_df <- dplyr::bind_cols(out, .name_repair="minimal")
  
  
  if(long){
    out_df <- out_df %>% gather(key="measure", value="value", 
                                colnames(out_df) %>% { .[!.%in%c("x","y","z","region","mv","hole","pv","autolabels","auto_region","zone")]} )
  }
  
  # out_df[,letters[24:26]] <- out_df[,letters[24:26]] %>% apply(2,round,3)
  
  out_df
}









# if(FALSE){
#   {
#     source("https://raw.githubusercontent.com/statpng/pngfunction/master/tidyverse/functions.R")
#     source("https://raw.githubusercontent.com/statpng/pngfunction/master/mesh3d/functions.R")
#     source("https://raw.githubusercontent.com/statpng/pngfunction/master/mesh3d/visualize.R")
#     
#     library(tidyverse)
#     library(broom)
#     library(survival)
#     library(broom)
#     library(patchwork)
#     
#     library(RColorBrewer)
#     RColorBrewer::brewer.pal(3, "Greens")[1:2]
#     RColorBrewer::brewer.pal(3, "Reds")[1:2]
#   }
#   
#   
#   
#   
#   
#   
#   
#   
#   # devtools::install_github("alexanderbates/deformetricar")
#   
#   library(freesurferformats)
#   
#   `%_%` <- function(x, y) {
#     paste0(x, y)
#   }
#   
#   path <- "/Volumes/png1/1.Mesh3d/data/flat/3218-CUVIA-cut-poly"
#   filename <- "3218-CUVIA-cut-poly"
#   filename_raw <- path %_% "/" %_% filename %_% ".vtk"
#   filename_flat <- path %_% "/" %_% filename %_% "_clipped_c_flat.vtk"
#   filename_values <- path %_% "/" %_% filename %_% "_clipped_c_flat_values.csv"
#   
#   df_values <- read.csv(filename_values)
#   colnames(df_values)[1:3] <- paste0(colnames(df_values)[1:3], "_3d")
#   
#   df_raw <- png_read.vtk(filename_raw)
#   df_flat <- png_read.vtk(filename_flat)
#   df_flat$nodes <- cbind( df_flat$nodes, df_values )
#   df_flat$nodes$region <- df_flat$nodes$region %>% round
#   df_flat_cleaned <- png.flat.remove_outlier(df_flat, type="na.omit")
#   
#   
#   # 3D reconstruction from ArrayValues
#   png.coord2mesh(nodes = df_flat_cleaned$nodes[,c("x","y","z") %>% paste0("_3d")],
#                  edges = df_flat_cleaned$edges ) %>% png.mesh3d.plot()
#   
#   png.coord2mesh(nodes = df_flat_cleaned$nodes,
#                  edges = df_flat_cleaned$edges ) %>% png.mesh3d.plot()
#   
#   plot(df_flat_cleaned$nodes[,letters[24:25]], cex=0.2)
#   
#   df_flat_cleaned$nodes %>% head
#   
#   mesh_raw <- df_raw %>% {png.coord2mesh(.$nodes, .$edges)}
#   mesh_flat <- df_flat %>% {png.coord2mesh(.$nodes, .$edges)}
#   mesh_flat_cleaned <- df_flat_cleaned %>% {png.coord2mesh(.$nodes, .$edges)}
#   
#   {
#     mesh_raw %>% png.mesh3d.plot()
#     mesh_flat %>% png.mesh3d.plot()
#     mesh_flat_cleaned %>% png.mesh3d.plot()
#   }
#   
#   
#   
#   nodes_intp <- png.flat.interpolate(df_nodes=df_flat_cleaned$nodes,
#                                      col_names=c("Voltage", "DF", "LAT", "Smax"),
#                                      keep_columns=c("region","zone"),
#                                      method="KNN", k=3, grid_resolution = 100)
#   
#   
#   library(ggplot2)
#   df_flat$nodes %>% ggplot() +
#     geom_point(aes(x,y,color=Voltage))
#   ggsave(filename="Figure - outliers.pdf", height=5, width=5)
#   
#   
#   nodes_intp %>% ggplot() +
#     geom_tile(aes(x,y,fill=Voltage))
#   
#   nodes_intp %>% ggplot() +
#     geom_tile(aes(x,y,fill=LAT))
#   
#   
#   png_reconstruction <- function(nodes_intp, nodes_3d, value_columns){
#     if(FALSE){
#       nodes_intp <- nodes_intp_selprob
#       nodes_3d <- df_flat_cleaned$nodes
#       value_columns <- c("VoltMap", "lam.9")
#       nodes_3d <- df_flat_cleaned$nodes
#       value_columns <- c("Voltage", "DF")
#     }
#     nodes_intp <- nodes_intp %>% as.matrix
#     nodes_3d <- nodes_3d %>% as.matrix
#     
#     nodes_intp %>% head(2)
#     nodes_3d %>% head(2)
#     
#     # row <- nodes_3d[1,letters[24:25]]
#     # diff_mat <- matrix(row, nrow(nodes_intp), ncol=2, byrow=TRUE) - nodes_intp[,letters[24:25]] %>% apply(1, function(x) norm(x, "2") )
#     # nodes_intp[which.min(dist_l2norm), target_columns]
#     
#     
#     out <- as.list(rep(NA, length(value_columns)))
#     names(out) <- value_columns
#     i=1; k=2
#     for( i in 1:length(value_columns) ){
#       value_column <- value_columns[i]
#       fit.knn <- knn.reg(train = nodes_intp[,letters[24:25]], test = nodes_3d[,letters[24:25]], y = nodes_intp[,value_column], k = k)
#       
#       out[[i]] <- fit.knn$pred
#     }
#     
#     out_df <- cbind.data.frame( nodes_3d[,paste0(letters[24:26],"_3d")], dplyr::bind_cols(out) )
#     colnames(out_df)[1:3] <- letters[24:26]
#     
#     out_df
#     
#   }
#   
#   
#   
#   
#   df_flat_reconst <- df_flat_cleaned
#   df_flat_reconst$nodes <- png_reconstruction(nodes_intp, nodes_3d, c("Voltage", "DF"))
#   colnames(df_flat_reconst$edges)[1:3] <- letters[24:26]
#   
#   df_flat_reconst
#   png.coord2mesh(df_flat_reconst$nodes, df_flat_reconst$edges, value = df_flat_reconst$nodes$Voltage)
#   df_flat_reconst$nodes %>% head(1)
#   
#   
#   df_flat_reconst
#   df_flat_reconst
#   
#   df_flat$nodes %>% select(-x,-y,-z) %>% select(x=x_3d, y=y_3d, z=z_3d, everything()) %>% write.csv("reconst_test_org - nodes.csv", row.names=FALSE)
#   df_flat$edges %>% write.csv("reconst_test_org - edges.csv", row.names=FALSE)
#   
#   write.csv(df_flat_reconst$nodes, "reconst_test - nodes.csv", row.names=FALSE)
#   write.csv(df_flat_reconst$edges, "reconst_test - edges.csv", row.names=FALSE)
#   
#   write.csv(df_flat_reconst$nodes, "reconst_test - nodes.csv", row.names=FALSE)
#   write.csv(df_flat_reconst$edges, "reconst_test - edges.csv", row.names=FALSE)
#   
#   
#   mesh_reconst <- png.coord2mesh(df_flat_reconst$nodes, df_flat_reconst$edges)
#   mesh_reconst$material$color <- df_flat_reconst$nodes$VoltMap
#   mesh_reconst %>% Rvcg::vcgStlWrite(filename="reconst_test.stl")
#   
#   
#   
#   png_read.vtk()
#   png.mesh3d.plot()
#   
#   
#   
# }





#' @export png.flat.read
png.flat.read <- function(path, remove.outlier=TRUE, round.region=FALSE){
  if(FALSE){
    path <- "/Users/png/Documents/6. Yonsei/1. Mesh3d/data/2826-cut2_poly"
    remove.outlier=TRUE
    round.region=FALSE
  }
  dirname <- png.str.extract_last_dir(path)
  
  filename_raw <- path %_% "/" %_% dirname %_% "_clipped_c.vtk"
  filename_flat <- path %_% "/" %_% dirname %_% "_clipped_c_flat.vtk"
  filename_values <- path %_% "/" %_% dirname %_% "_clipped_c_flat_values.csv"
  
  if( !file.exists(filename_flat) ) return(NULL)
    
  df_raw <- png_read.vtk(filename_raw)
  df_flat <- png_read.vtk(filename_flat)
  df_values <- read.csv(filename_values)
  colnames(df_values)[1:3] <- paste0(colnames(df_values)[1:3], "_3d")
  
  df_flat$nodes <- cbind( df_flat$nodes, df_values )
  
  if(round.region){
    df_flat$nodes$region <- df_flat$nodes$region %>% round
  }
  
  if(remove.outlier){
    df_flat_cleaned <- png.flat.remove_outlier(df_flat, radius=0.5, type="na.omit")
  }
  
  return( list(raw=df_raw, flat=df_flat) )
}









png_reconstruction <- function(df_intp, path, col_factor=c("region", "auto_region"), title="", print=FALSE){
  if(FALSE){
    df_intp <- df_pvalue %>% select(-measure)
    path <- "/Users/png/Documents/6. Yonsei/1. Mesh3d/data/2826-cut2_poly"
  }
  
  ID <- png.str.extract_last_dir(path) %>% {stringr::str_extract(., "(\\d+)", group=1)} %>% .[1]
  data_flat <- png.flat.read(path, remove.outlier=TRUE, round.region=FALSE)
  
  
  df_raw <- data_flat$raw$nodes
  df_flat <- data_flat$flat$nodes
  
  
  value_columns <- colnames(df_intp) %>% {.[!.%in%c("ID","xy","x","y","z","measure")]}

  out <- as.list(rep(NA, length(value_columns)))
  names(out) <- value_columns
  for( i in 1:length(value_columns) ){
    col <- value_columns[i]
    
    is.factor <- ifelse( col %in% col_factor, TRUE, FALSE )
    if(!is.factor){
      fit.knn <- knn.reg(train = df_intp[,letters[24:25]] %>% as.matrix, test = df_flat[,letters[24:25]] %>% as.matrix, y = df_intp[, col], k = 3)$pred
    } else {
      fit.knn <- knn(train = df_intp[,letters[24:25]] %>% as.matrix, test = df_flat[,letters[24:25]] %>% as.matrix, cl = df_intp[, col], k = 3)
    }
    
    out[[i]] <- fit.knn
  }

  out_df <- cbind.data.frame( df_flat[,letters[24:26]%_%"_3d"], dplyr::bind_cols(out) )
  colnames(out_df)[1:3] <- letters[24:26]

  
  
  
  if(print){
    df_reconst <- data_flat$flat
    df_reconst$nodes <- out_df

    # mesh_reconst <- png.coord2mesh(df_reconst$nodes,
    #                df_reconst$edges,
    #                value = df_reconst$nodes$p.value) %>% png.mesh3d.plot()

    write.csv(df_reconst$nodes, "./tables/Table-reconst-"%_% title %_% "-" %_% ID %_% "-nodes.csv", row.names=FALSE)
    write.csv(df_reconst$edges, "./tables/Table-reconst-"%_% title %_% "-" %_% ID %_% "-edges.csv", row.names=FALSE)

  }

  
  out_df
  
}





function(df_pvalue){
  
  df <- df_pvalue
  
  
  
  df_flat_reconst$nodes <- png_reconstruction(nodes_intp, df_flat_cleaned$nodes, c("VoltMap", "DF"))
  colnames(df_flat_reconst$edges)[1:3] <- letters[24:26]
  
  df_flat_reconst
  png.coord2mesh(df_flat_reconst$nodes, 
                 df_flat_reconst$edges, 
                 value = df_flat_reconst$nodes$VoltMap)
  
  write.csv(df_flat_reconst$nodes, "df_flat-" %_% ID %_% "-reconst-nodes.csv", row.names=FALSE)
  write.csv(df_flat_reconst$edges, "df_flat-" %_% ID %_% "-reconst-edges.csv", row.names=FALSE)
  png.coord2mesh(df_flat_reconst$nodes, df_flat_reconst$edges) %>% 
    Rvcg::vcgStlWrite(filename="df_flat-" %_% ID %_% "-reconst.stl")
  
  
  write.csv( cbind.data.frame(ID=ID, nodes_intp_long), file="nodes_intp_long-" %_% ID %_% ".csv" )
  
}
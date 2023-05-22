#' @export
`%_%` <- function(x, y) {
  paste0(x, y)
}


#' @export mark_NAregion
mark_NAregion <- function(coord, target) {
  coord$NA_region <- 0
  coord_distance_matrix <- apply(coord, 1, function(x) apply(target, 1, function(y) norm(x[1:2] - y, "2")))
  
  wh.region <- which(coord_distance_matrix < 1e-2, arr.ind = TRUE)[, 1]
  coord$NA_region[wh.region] <- 1
  
  return(coord)
}




#' @export png_predict
png_predict <- function(df, col_name){
  library(randomForest)
  
  
  if(FALSE){
    wh_NAregion <- png_predict(df_nodes, "LAT")
    
    plot_df_IDW <- fit.interpolate_IDW[[1]]
    plot_df_KNN <- fit.interpolate_KNN[[1]]
    plot_df_IDW$value[wh_NAregion] <- NA
    plot_df_KNN$value[wh_NAregion] <- NA
    
    
    
    
    df_nodes %>% mutate(value=LAT) %>% mutate(value=ifelse(value==0,NA,value)) %>% png_disc_plot() %>% ggsave(filename="./figures/Figure - 3218_LAT - ORG.pdf", width=5, height=5)
    plot_df_IDW %>% png_disc_plot() %>% ggsave(filename="./figures/Figure - 3218_LAT - IDW.pdf", width=5, height=5)
    plot_df_KNN %>% png_disc_plot() %>% ggsave(filename="./figures/Figure - 3218_LAT - KNN.pdf", width=5, height=5)
  }
  
  
  fit <- randomForest::randomForest(NA_region~x+y, data=df %>% mutate(NA_region=as.factor(get(col_name)==0)), ntree=2000)
  wh_NAregion <- which( predict(fit, fit.interpolate_IDW[[1]][,1:2])==TRUE )
  wh_NAregion
  
  
}
#





#' @export png.str.extract_last_dir
png.str.extract_last_dir <- function(path){
  # path <- "/Users/png/Documents/6. Yonsei/1. Mesh3d/data/2826-cut2_poly"
  strsplit(path, "/")[[1]] %>% {.[length(.)]}
}

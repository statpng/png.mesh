
#' @export png_read.vtk
png_read.vtk <- function(filename, from_python = TRUE){
  
  if(!file.exists(filename)) stop("Cannot read: ",filename)
  con=file(filename,open='rb',encoding='ASCII')
  on.exit(close(con))
  magic=readLines(con,n=1)
  if(regexpr("# vtk DataFile Version [2345]",magic,ignore.case =T)<0)
    stop("Bad header line in file: ",filename)
  
  title=readLines(con,1)
  encoding=readLines(con,1)
  if(regexpr("ASCII",encoding,ignore.case=TRUE)<0)
    stop("Can only read ASCII encoded VTK pointsets")
  
  datasetLine=toupper(readLines(con,1))
  if(regexpr("^DATASET",datasetLine)<0)
    stop("Missing DATASET line")
  
  datasetType=sub("DATASET\\s+(\\w+)","\\1",datasetLine)
  
  validDatasetTypes<-c("STRUCTURED_POINTS", "STRUCTURED_GRID",
                       "UNSTRUCTURED_GRID", "POLYDATA", "RECTILINEAR_GRID", "FIELD")
  
  if(!datasetType%in%validDatasetTypes)
    stop(datasetType," is not a valid VTK dataset type")
  if(datasetType!="POLYDATA")
    stop("ReadVTKLandmarks can currently only read POLYDATA.",
         " See http://www.vtk.org/VTK/img/file-formats.pdf for details.")
  
  
  
  
  pointsLine=toupper(readLines(con,1))
  if(regexpr("POINTS",pointsLine)<0)
    stop("Missing POINTS definition line")
  ptinfo=unlist(strsplit(pointsLine,"\\s+",perl=TRUE))
  if(length(ptinfo)!=3)
    stop("Unable to extract points information from POINTS line",pointsLine)
  
  
  nummarkers=as.integer(ptinfo[2])
  if(is.na(nummarkers))
    stop("Unable to extract number of points from POINTS line:",pointsLine)
  datatype=ptinfo[3]
  if(!datatype%in%toupper(c("unsigned_char", "char", "unsigned_short", "short", "unsigned_int", "int",
                            "unsigned_long", "long", "float", "double")))
    stop("Unrecognised VTK datatype: ",datatype)
  
  points=scan(con,what=1.0,n=3*nummarkers,quiet=TRUE)
  
  # VTK seems to be hardcoded for 3D
  
  
  m=matrix(points,ncol=3,byrow=T)
  colnames(m)=c("X","Y","Z")
  attr(m,"file")=filename
  attr(m,"title")=title
  attr(m,"vtk_datatype")=datatype
  
  
  
  
  
  triangLine=toupper(readLines(con,1))
  if( !grepl("POLYGONS", triangLine) ){
    triangLine=toupper(readLines(con,1))
  }
  if(length(triangLine)==0){
    warning("No data on polygons found")
    return(NULL)
  }
  if(regexpr("POLYGONS",triangLine)<0)
    stop("Missing POLYGONS definition line")
  lninfo=unlist(strsplit(triangLine,"\\s+",perl=TRUE))
  
  triangDataTypeLine=toupper(readLines(con,1))
  lnDataTypeinfo=unlist(strsplit(triangDataTypeLine,"\\s+",perl=TRUE))
  if(length(lninfo)!=3)
    stop("Unable to extract connection information from POLYGONS line:",triangLine)
  
  nummconns=as.integer(lninfo[2])
  if(is.na(nummconns))
    stop("Unable to extract number of connections from POLYGONS line:",triangLine)
  datatype=lnDataTypeinfo[2]
  
  triang=scan(con,what=1.0,n=nummconns,quiet=TRUE)
  triang <- triang[1:(length(triang) - length(triang)%%3)]
  triangle_df <- matrix(triang,ncol=3,byrow=T)
  attr(triangle_df,"file")=filename
  attr(triangle_df,"title")=title
  attr(triangle_df,"vtk_datatype")= "int"
  
  
  
  
  num_edges=as.integer(lninfo[3])
  while(TRUE){
    tmp_line = toupper(readLines(con,1))
    if( grepl("CONNECTIVITY", tmp_line) ) break
  }
  edges = scan(con,what=1.0,n=num_edges,quiet=TRUE)
  
  nodes_df <- matrix(points, ncol=3, byrow=TRUE) %>% as.data.frame
  edges_df <- matrix(edges, ncol=3, byrow=TRUE) %>% as.data.frame
  colnames(nodes_df) <- colnames(edges_df) <- letters[24:26]
  
  if( from_python ){
    edges_df <- edges_df+1
  }
  
  list(nodes=nodes_df, edges=edges_df)
}



#' @export png.read_plt
png.read_plt <- function(path, type="triangle"){
  library(data.table)
  
  if(FALSE){
    path <- "/Users/png/Downloads/721.plt"
  }
  
  lines <- readLines(path)
  header <- lines[2]
  lines <- lines[-(1:2)]
  
  change_index = as.numeric( stringr::str_extract(header, "[0-9]+") )
  
  first_part_lines <- lines[1:change_index]
  second_part_lines <- lines[change_index:length(lines)]
  
  # 각 부분을 data.table로 변환
  first_part_data <- data.table::fread(text = paste(first_part_lines, collapse = "\n"), header = FALSE, sep = " ") %>% as_tibble()
  second_part_data <- data.table::fread(text = paste(second_part_lines, collapse = "\n"), header = FALSE, sep = " ") %>% as_tibble()
  
  data.table::setnames(first_part_data, c("x", "y", "z", "value"))
  data.table::setnames(second_part_data, c("x", "y", "z"))
  
  if(type == "triangle"){
    mesh <- rgl::tmesh3d(vertices = t(first_part_data[,1:3]), indices = t(second_part_data))
  } else {
    mesh <- rgl::mesh3d(vertices = t(first_part_data[,1:3]), quads = t(second_part_data))
  }
  
  mesh$value <- as.matrix(first_part_data[,4])
  
  mesh
  
}




#' @export png.read_plts
png.read_plts <- function(file_list, nmax = 10, center = TRUE, type = "triangle") {
  library(data.table)
  
  mesh_list <- list()
  
  for (i in 1:length(file_list)) {
    if (i > nmax) break
    
    file_path <- file_list[i]
    mesh <- png.read_plt(file_path, type = type)
    
    if (center) {
      mesh$vb[1:3,] <- t(scale(t(mesh$vb[1:3,]), scale = FALSE))
    }
    
    mesh_list[[i]] <- mesh
  }
  
  return(mesh_list)
}



#' @export png.read_cuvia
png.read_cuvia <- function(path, level=1, start=4){
  # start <- 4
  
  for( i in 1:level ){
    if( i > 1 ){
      start <- out$edge_end
    }
    names(start) <- NULL
    rows <- read.csv(path, skip=start, nrows=1, sep=" ", header = FALSE) %>% as.numeric
    
    node_start <- start+2
    node_end <- start+rows[1]+1
    edge_start <- start+rows[1]+2
    edge_end <- start+rows[1]+rows[2]+1
    out <- list(edge_start=edge_start, 
                edge_end=edge_end, 
                node_start=node_start, 
                node_end=node_end)
    
  }
  
  df_edge <- read.csv(path, skip=out$edge_start-1, nrows = with(out, edge_end-edge_start+1), header=F, sep=" " )
  df_node <- read.csv(path, skip=out$node_start-1, nrows = with(out, node_end-node_start+1), header=F, sep=" " )
  
  df_edge[,1:3] <- df_edge[,1:3]+1
  
  list(edge=as_tibble(df_edge), 
       node=as_tibble(df_node),
       row=out)
}

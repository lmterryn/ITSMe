#' Calculate and save figures of dbh_pc function
#'
#' Calculates  the dbh and saves the figures acquired when running
#' \code{\link{dbh_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{dbh_pc}} with parameter plot = TRUE to calculate the dbh
#' and plot the circle fitting.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved.
#'
#' @return a numeric containing the dbh values for each tree point cloud. Plots
#'   are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PCs_path <- "path/to/folder/PCs/"
#' extension <- ".txt"
#' OUT_path <- "path/to/folder/for/saving/figures/"
#' dbh_values <- plot_dbh_fit_pcs(PCs_path,extension,OUT_path)
#' }
plot_dbh_fit_pcs <- function(PCs_path,extension=".txt",OUT_path){
  file_paths <- list.files(PCs_path, pattern=paste("*",extension,sep=""),
                           full.names=TRUE)
  file_names <- list.files(PCs_path, pattern=paste("*",extension,sep=""),
                           full.names=FALSE)
  DBHs <- c()
  for (i in 1:length(file_names)){
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    grDevices::jpeg(file=paste(OUT_path,"dbh_",strsplit(file_names[i],extension)[[1]],
                    ".jpeg",sep = ""))
    dbh <- dbh_pc(pc,TRUE)
    grDevices::dev.off()
    DBHs <- append(DBHs,dbh)
  }
  return(DBHs)
}

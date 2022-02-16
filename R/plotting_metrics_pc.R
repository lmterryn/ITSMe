#' Calculate and save figures of \code{\link{dbh_pc}} function
#'
#' Calculates  the dbh and saves the figures acquired when running
#' \code{\link{dbh_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{dbh_pc}} with parameter plot = TRUE to calculate the dbh and plot
#' the circle fitting.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved.
#'
#' @return a numeric containing the dbh values for each tree point cloud.
#'   Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PCs_path <- "path/to/folder/PCs/"
#' extension <- ".txt"
#' OUT_path <- "path/to/folder/for/saving/figures/"
#' dbh_values <- plot_dbh_fit_pcs(PCs_path, extension, OUT_path)
#' }
plot_dbh_fit_pcs <- function(PCs_path, extension = ".txt", OUT_path) {
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  DBHs <- c()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    grDevices::jpeg(file = paste(OUT_path, "dbh_",
                                 strsplit(file_names[i], extension)[[1]],
                                 ".jpeg", sep = ""))
    dbh <- dbh_pc(pc, TRUE)
    grDevices::dev.off()
    DBHs <- append(DBHs, dbh)
  }
  return(DBHs)
}

#' Calculate and save figures of \code{\link{dab_pc}} function
#'
#' Calculates the dab and saves the figures acquired when running
#' \code{\link{dab_pc}} on multiple tree point clouds in a folder. Use different
#' values for the thresholdbuttress and maxbuttressheight parameter to optimise
#' dab calculation for your tree point clouds.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{dab_pc}} with parameter plot = TRUE to calculate the dab and plot
#' the circle fitting.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses.
#'
#' @return a numeric containing the dbh values for each tree point cloud.
#'   Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PCs_path <- "path/to/folder/PCs/"
#' extension <- ".txt"
#' OUT_path <- "path/to/folder/for/saving/figures/"
#' dab_values <- plot_dab_fit_pcs(PCs_path, extension, OUT_path)
#' }
plot_dab_fit_pcs <- function(PCs_path, extension = ".txt", OUT_path,
                             thresholdbuttress = 0.001, maxbuttressheight = 7) {
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  DABs <- c()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    grDevices::jpeg(file = paste(OUT_path, "dab_",
                                 strsplit(file_names[i], extension)[[1]], "_",
                                 as.character(thresholdbuttress), "_",
                                 as.character(maxbuttressheight),
                                 ".jpeg", sep = ""))
    dab <- dab_pc(pc, thresholdbuttress, maxbuttressheight, TRUE)
    grDevices::dev.off()
    DABs <- append(DABs, dab)
  }
  return(DABs)
}

#' Save figures of \code{\link{classify_crown_pc}} function
#'
#' Classifies the tree point clouds into crown and non crown points and saves
#' the figures with \code{\link{classify_crown_pc}} for multiple tree point
#' clouds in a folder. Use different values for the thresholdbranch and
#' minheight parameter to optimise the crown classification for your tree point
#' clouds. Mainly minheight parameter needs to be optimised, small values (e.g.
#' 1) when the trees don't have buttresses and higher values (e.g 4) for trees
#' with buttresses. For buttressed trees, first optimise the thresholdbuttress
#' and maxbuttressheight parameter values using \code{\link{plot_dab_fit_pcs}}
#' and use those optimised values in this function.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{classify_crown_pc}} with parameter plot = TRUE to classify the
#' tree point cloud and plot the classification.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#'
#' @return Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PCs_path <- "path/to/folder/PCs/"
#' extension <- ".txt"
#' OUT_path <- "path/to/folder/for/saving/figures/"
#' plot_crown_classification_pcs(PCs_path, extension, OUT_path)
#' }
plot_crown_classification_pcs <- function(PCs_path, extension = ".txt",
                                          OUT_path, thresholdbranch = 1.5,
                                          minheight = 1, buttress = FALSE,
                                          thresholdbuttress = 0.001,
                                          maxbuttressheight = 7){
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    grDevices::jpeg(file = paste(OUT_path, "crown_",
                                 strsplit(file_names[i], extension)[[1]], "_",
                                 as.character(thresholdbranch), "_",
                                 as.character(minheight),
                                 ".jpeg", sep = ""))
    classify_crown_pc(pc, thresholdbranch, minheight, buttress,
                      thresholdbuttress, maxbuttressheight, TRUE)
    grDevices::dev.off()
  }
}

#' Calculate and save figures of \code{\link{projected_crown_area_pc}} function
#'
#' Calculates the projected crown area (pca) and saves the figures acquired when
#' running \code{\link{projected_crown_area_pc}} on multiple tree point clouds
#' in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{projected_crown_area_pc}} with parameter plot = TRUE to calculate
#' the pca and plot the crown projection and fitting. For buttressed trees,
#' first optimise the thresholdbuttress, maxbuttressheight, thresholdbranch and
#' minheight parameter values using \code{\link{plot_dab_fit_pcs}} and
#' \code{\link{plot_crown_classification_pcs}} and use those optimised values in
#' this function.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved.
#' @param concavity Numeric value (default=2) concavity for the computation of a
#'   concave hull based on \code{\link[concaveman]{concaveman}} in
#'   \code{\link{projected_crown_area_pc}}.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#'
#' @return a numeric containing the pca values for each tree point cloud.
#'   Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PCs_path <- "path/to/folder/PCs/"
#' extension <- ".txt"
#' OUT_path <- "path/to/folder/for/saving/figures/"
#' plot_pca_pcs(PCs_path, extension, OUT_path)
#' }
plot_pca_pcs <- function(PCs_path, extension = ".txt", OUT_path, concavity = 2,
                         thresholdbranch = 1.5, minheight = 1, buttress = FALSE,
                         thresholdbuttress = 0.001, maxbuttressheight = 7){
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  PCAs <- c()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    grDevices::jpeg(file = paste(OUT_path, "pca_",
                                 strsplit(file_names[i], extension)[[1]], "_",
                                 as.character(concavity), ".jpeg", sep = ""))
    pca <- projected_crown_area_pc(pc, concavity, thresholdbranch, minheight,
                                   buttress, thresholdbuttress,
                                   maxbuttressheight, TRUE)
    grDevices::dev.off()
    PCAs <- append(PCAs, pca)
  }
  return(PCAs)
}

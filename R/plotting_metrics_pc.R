#' Calculate and save figures of \code{\link{diameter_slice_pc}} function
#'
#' Calculates  the diameter and saves the figures acquired when running
#' \code{\link{diameter_slice_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{diameter_slice_pc}} with parameter plot = TRUE to calculate the
#' diameter and plot the circle fitting.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param slice_height Numeric value (default = 1.3) that determines the height
#'   above the lowest point of the point cloud at which the diameter is
#'   measured. Parameter of the \code{\link{diameter_slice_pc}} function used to
#'   calculate the diameter of a stem slice.
#' @param slice_thickness Numeric value (default = 0.6) that determines the
#'   thickness of the slice which is used to measure the diameter. Parameter of
#'   the \code{\link{diameter_slice_pc}} function used to calculate the diameter
#'   of a stem slice.
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved (default = current folder).
#'
#' @return A list with in the first element a numeric containing the diameter
#'   values for each tree point cloud. In the second element there is the list
#'   with the plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate diameters at a certain height and save circle fitting figures
#' diam_values <- plot_circle_fit_pcs(PCs_path = "path/to/folder/PCs/",
#'                                   extension = ".txt",
#'                                   OUT_path = "path/to/figure/folder/")
#' }
plot_circle_fit_pcs <- function(PCs_path, extension = ".txt",
                                slice_height = 1.3, slice_thickness = 0.06,
                                OUT_path = "./") {
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  Ds <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- diameter_slice_pc(pc, slice_height, slice_thickness, TRUE)
    filename <- paste(OUT_path, "circle_",
                      strsplit(file_names[i], extension)[[1]], "_",
                      as.character(slice_height), "_",
                      as.character(slice_thickness), "_", ".jpeg", sep = "")
    ggplot2::ggsave(filename, plot = out$plot)
    Ds <- append(Ds, out$diam)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("Diams"=Ds, "Plots"=Plots))
}

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
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height.
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved (default = current folder).
#'
#' @return A list with in the first element a numeric containing the dbh values
#'   for each tree point cloud. In the second element there is the list with the
#'   plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DBHs and save circle fitting figures
#' dbh_values <- plot_dbh_fit_pcs(PCs_path = "path/to/folder/PCs/",
#'                                extension = ".txt",
#'                                OUT_path = "path/to/figure/folder/")
#' }
plot_dbh_fit_pcs <- function(PCs_path, extension = ".txt", thresholdR2 = 0.001,
                             OUT_path = "./") {
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  DBHs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- dbh_pc(pc, thresholdR2, TRUE)
    filename <- paste(OUT_path, "dbh_",
                     strsplit(file_names[i], extension)[[1]], "_",
                     as.character(thresholdR2), "_", ".jpeg", sep = "")
    ggplot2::ggsave(filename, plot = out$plot)
    DBHs <- append(DBHs, out$dbh)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("DBHs"=DBHs, "Plots"=Plots))
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
#'   should be saved (default = current folder).
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses.
#'
#' @return A list with in the first element a numeric containing the dab values
#'   for each tree point cloud. In the second element there is the list with the
#'   plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DABs with default settings and save circle fitting figures
#' dab_values <- plot_dab_fit_pcs(PCs_path = "path/to/folder/PCs/",
#'                                extension = ".txt",
#'                                OUT_path = "path/to/figure/folder/")
#' # Calculate DABs with non-default settings and save circle fitting figures
#' dab_values <- plot_dab_fit_pcs(PCs_path = "path/to/folder/PCs/",
#'                                extension = ".txt",
#'                                OUT_path = "path/to/figure/folder/",
#'                                thresholdbuttress = 0.002,
#'                                maxbuttressheight = 5)
#' }
plot_dab_fit_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                             thresholdbuttress = 0.001, maxbuttressheight = 7) {
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  DABs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- dab_pc(pc, thresholdbuttress, maxbuttressheight, TRUE)
    filename = paste(OUT_path, "dab_", strsplit(file_names[i], extension)[[1]],
                     "_", as.character(thresholdbuttress), "_",
                     as.character(maxbuttressheight), ".jpeg", sep = "")
    ggplot2::ggsave(filename, plot = out$plot)
    DABs <- append(DABs, out$dab)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("DABs"=DABs, "Plots"=Plots))
}

#' Save figures of \code{\link{classify_crown_pc}} function
#'
#' Classifies the tree point clouds into crown and non crown points and saves
#' the figures with \code{\link{classify_crown_pc}} for multiple tree point
#' clouds in a folder. Use different values for the thresholdbranch and
#' minheight parameter to optimise the crown classification for your tree point
#' clouds. Mainly minheight parameter needs to be optimised, small values (e.g
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
#'   should be saved (default = current folder).
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#'
#' @return Returns a list with the plots and individual plots saved in the
#'   output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run the crown classification with default settings and save figures
#' plot_crown_classification_pcs(PCs_path = "path/to/folder/PCs/",
#'                               extension = ".txt",
#'                               OUT_path = "path/to/figure/folder/")
#' # Run the crown classification with non-default settings and save figures
#' plot_crown_classification_pcs(PCs_path = "path/to/folder/PCs/",
#'                               extension = ".txt",
#'                               OUT_path = "path/to/figure/folder/",
#'                               thresholdbranch = 2, minheight = 4)
#' # Run the crown classification with non-default settings and save figures
#' # for buttressed trees
#' plot_crown_classification_pcs(PCs_path = "path/to/folder/PCs/",
#'                               extension = ".txt",
#'                               OUT_path = "path/to/figure/folder/",
#'                               thresholdbranch = 2, minheight = 4,
#'                               buttress = TRUE, thresholdbuttress = 0.002,
#'                               maxbuttressheight = 5)
#' }
plot_crown_classification_pcs <- function(PCs_path, extension = ".txt",
                                          OUT_path = "./",
                                          thresholdbranch = 1.5,
                                          minheight = 1, buttress = FALSE,
                                          thresholdR2 = 0.001,
                                          thresholdbuttress = 0.001,
                                          maxbuttressheight = 7){
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- classify_crown_pc(pc, thresholdbranch, minheight, buttress,
                             thresholdR2, thresholdbuttress, maxbuttressheight,
                             TRUE)
    filename = paste(OUT_path, "crown_",
                     strsplit(file_names[i], extension)[[1]], "_",
                     as.character(thresholdbranch), "_",
                     as.character(minheight),
                     ".jpeg", sep = "")
    ggplot2::ggsave(filename, plot = out$plot, bg = "white")
    Plots <- append(Plots, list(out$plot))
  }
  return(Plots)
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
#'   should be saved (default = current folder).
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
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#'
#' @return A list with in the first element a numeric containing the pca values
#'   for each tree point cloud. In the second element there is the list with the
#'   plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate PCA with default settings and save projection figures
#' pcas <- plot_pca_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                      OUT_path = "path/to/figure/folder/")
#' # Calculate PCA with non-default settings and save projection figures
#' pcas <- plot_pca_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                      OUT_path = "path/to/figure/folder/", concavity = 3)
#' # Calculate PCA with non-default settings and save projection figures
#' # for buttressed trees
#' pcas <- plot_pca_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                      OUT_path = "path/to/figure/folder/", concavity = 3,
#'                      minheight = 4, buttress = TRUE)
#' }
plot_pca_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                         concavity = 2, thresholdbranch = 1.5, minheight = 1,
                         buttress = FALSE, thresholdR2 = 0.001,
                         thresholdbuttress = 0.001,
                         maxbuttressheight = 7){
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  PCAs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- projected_crown_area_pc(pc, concavity, thresholdbranch, minheight,
                                   buttress, thresholdR2, thresholdbuttress,
                                   maxbuttressheight, TRUE)
    filename = paste(OUT_path, "pca_", strsplit(file_names[i], extension)[[1]],
                     "_", as.character(concavity), ".jpeg", sep = "")
    ggplot2::ggsave(filename, plot = out$plot, bg = "white")
    PCAs <- append(PCAs, out$pca)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("PCAs"=PCAs, "Plots"=Plots))
}

#' Calculate and save figures of \code{\link{volume_crown_pc}} function
#'
#' Calculates the crown volume and saves the figures acquired when running
#' \code{\link{volume_crown_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{volume_crown_pc}} with parameter plot = TRUE to calculate the
#' crown volume and plot the crown and 3D the alpha-shape fitting. For
#' buttressed trees, first optimise the thresholdbuttress, maxbuttressheight,
#' thresholdbranch and minheight parameter values using
#' \code{\link{plot_dab_fit_pcs}} and
#' \code{\link{plot_crown_classification_pcs}} and use those optimised values in
#' this function.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved (default = current folder).
#' @param alpha Numeric value (default=1) alpha for the computation of the 3D
#'   alpha-shape of the tree crown based on \code{\link[alphashape3d]{ashape3d}}
#'   in \code{\link{volume_crown_pc}}.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when buttress == TRUE.
#'
#' @return a numeric containing the crown volume values for each tree point
#'   cloud. Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate CV with default settings and save alpha shape figures
#' cvs <- plot_cv_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                    OUT_path = "path/to/figure/folder/")
#' # Calculate CV with non-default settings and save alpha shape figures
#' cvs <- plot_cv_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                    OUT_path = "path/to/figure/folder/", alpha = 2)
#' # Calculate CV with non-default settings and save alpha shape figures
#' # for buttressed trees
#' cvs <- plot_cv_pcs(PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'                    OUT_path = "path/to/figure/folder/", alpha = 2,
#'                    minheight = 4, buttress = TRUE)
#' }
plot_cv_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                        alpha = 1, thresholdbranch = 1.5, minheight = 1,
                        buttress = FALSE, thresholdR2 = 0.001,
                        thresholdbuttress = 0.001,
                        maxbuttressheight = 7){
  file_paths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = TRUE)
  file_names <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                           full.names = FALSE)
  CVs <- c()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    fig_name <- paste(OUT_path, "cv_", strsplit(file_names[i], extension)[[1]],
                      "_", as.character(alpha), ".png", sep = "")
    out <- volume_crown_pc(pc, alpha, thresholdbranch, minheight,
                                   buttress, thresholdR2, thresholdbuttress,
                                   maxbuttressheight, TRUE)
    rgl::rgl.snapshot(fig_name, fmt = 'png')
    rgl::rgl.close()
    CVs <- append(CVs, out$cv)
    gc()
  }
  return(CVs)
}

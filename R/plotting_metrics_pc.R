#' Calculate and save figures of \code{\link{tree_height_pc}} function
#'
#' Calculates the tree height and saves the figures acquired when running
#' \code{\link{tree_height_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{tree_height_pc}} with parameter plot = TRUE to calculate the tree
#' height and plot the tree point cloud.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".
#' @param dtm The digital terrain model (default = NA), parameter of
#'   \code{\link{tree_height_pc}}.
#' @param r Numeric value (default=5) r, parameter of
#'   \code{\link{tree_height_pc}}. Only relevant if a dtm is provided.
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved (default = current folder).
#'
#' @return A list with in the first element a numeric containing the tree height
#'   values for each tree point cloud. In the second element there is the list
#'   with the plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate tree height and save figures
#' height_values <- plot_tree_height_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # Calculate tree height using dtm of resolution 2 and save figures
#' dtm_df <- read_tree_pc("path/to/dtm.txt")
#' height_values <- plot_tree_height_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt", dtm = dtm_df, r = 2,
#'   OUT_path = "path/to/figure/folder/"
#' )
#' }
plot_tree_height_pcs <- function(PCs_path, extension = ".txt", dtm = "NA",
                                 r = 5, OUT_path = "./") {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  Hs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- tree_height_pc(pc, dtm, r, TRUE)
    filename <- paste(OUT_path, "tree_height_",
      strsplit(file_names[i], extension)[[1]], ".jpeg",
      sep = ""
    )
    ggplot2::ggsave(filename,
      plot = out$plot, bg = "white", width = 60,
      height = 36, units = "cm"
    )
    Hs <- append(Hs, out$h)
    Plots <- append(Plots, out$plot)
  }
  return(list("File" = file_names,"Heights" = Hs, "Plots" = Plots))
}

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
#'   values for each tree point cloud, the second element the residuals on the
#'   circle fittings, the third element the functional diameters. In the fourth
#'   element there is the list with the plots. Figures are also saved in the
#'   output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate diameters at breast height (1.3m) and save circle fitting figures
#' diam_values <- plot_circle_fit_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # Calculate diameters at 2.5 m with a slice of 20 cm and save the figures
#' diam_values <- plot_circle_fit_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt", slice_height = 2.5,
#'   slice_thickness = 0.2,
#'   OUT_path = "path/to/figure/folder/"
#' )
#' }
plot_circle_fit_pcs <- function(PCs_path, extension = ".txt",
                                slice_height = 1.3, slice_thickness = 0.06,
                                OUT_path = "./") {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  Ds <- c()
  Rs <- c()
  fDs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- diameter_slice_pc(pc, slice_height, slice_thickness, TRUE)
    filename <- paste(OUT_path, "circle_",
      strsplit(file_names[i], extension)[[1]], "_",
      as.character(slice_height), "_",
      as.character(slice_thickness), "_", ".jpeg",
      sep = ""
    )
    ggplot2::ggsave(filename, plot = out$plot)
    Ds <- append(Ds, out$diameter)
    Rs <- append(Rs, out$R2)
    fDs <- append(fDs, out$fdiameter)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("File" = file_names, "Diams" = Ds, "R2s" = Rs, "fDiams" = fDs,
              "Plots" = Plots))
}

#' Calculate and save figures of \code{\link{dbh_pc}} function
#'
#' Calculates the dbh and saves the figures acquired when running
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
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height.
#' @param OUT_path A character with the path to the folder where the figures
#'   should be saved (default = current folder).
#'
#' @return A list with in the first element a numeric containing the dbh values
#'   for each tree point cloud, the second element the residuals on the circle
#'   fittings, the third element the functional diameters. In the fourth element
#'   there is the list with the plots. Figures are also saved in the output
#'   folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DBHs and save circle fitting figures
#' dbh_values <- plot_dbh_fit_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' }
plot_dbh_fit_pcs <- function(PCs_path, extension = ".txt", thresholdR2 = 0.001,
                             slice_thickness = 0.06, OUT_path = "./") {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  DBHs <- c()
  Rs <- c()
  fDBHs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- tryCatch(
      {
        dbh_pc(pc, thresholdR2, slice_thickness, TRUE)
        },
      error = function(cond){
        message(cond)
        return(list("dbh" = NaN, "R2" = NaN, "fdbh" = NaN,
                    "plot" = ggplot2::ggplot()))
      }
    )
    filename <- paste(OUT_path, "dbh_",
      strsplit(file_names[i], extension)[[1]], "_",
      as.character(thresholdR2), "_",
      as.character(slice_thickness), ".jpeg",
      sep = ""
    )
    ggplot2::ggsave(filename, plot = out$plot, width =  15, height = 10,
                    units = "cm")
    DBHs <- append(DBHs, out$dbh)
    Rs <- append(Rs, out$R2)
    fDBHs <- append(fDBHs, out$fdbh)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("File" = file_names, "DBHs" = DBHs, "R2s" = Rs, "fDBHs" = fDBHs,
              "Plots" = Plots))
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
#' @param slice_thickness Numeric value (default = 0.06) that determines the
#'   thickness of the slice which is used to measure the diameter.
#'
#' @return A list with in the first element a numeric containing the dab values
#'   for each tree point cloud, the second element the residuals on the circle
#'   fittings, the third element the functional diameters. In the fourth element
#'   there is the list with the plots. Figures are also saved in the output
#'   folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate DABs with default settings and save circle fitting figures
#' dab_values <- plot_dab_fit_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # Calculate DABs with non-default settings and save circle fitting figures
#' dab_values <- plot_dab_fit_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/",
#'   thresholdbuttress = 0.002,
#'   maxbuttressheight = 5
#' )
#' }
plot_dab_fit_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                             thresholdbuttress = 0.001, maxbuttressheight = 7,
                             slice_thickness = 0.06) {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  DABs <- c()
  Rs <- c()
  fDABs <- c()
  Hs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- dab_pc(pc, thresholdbuttress, maxbuttressheight, slice_thickness,
                  TRUE)
    filename <- paste(OUT_path, "dab_", strsplit(file_names[i], extension)[[1]],
      "_", as.character(thresholdbuttress), "_",
      as.character(maxbuttressheight), ".jpeg",
      sep = ""
    )
    ggplot2::ggsave(filename, plot = out$plot, width =  15, height = 10, units = "cm")
    DABs <- append(DABs, out$dab)
    Rs <- append(Rs, out$R2)
    fDABs <- append(fDABs, out$fdab)
    Hs <- append(Hs, out$h)
    Plots <- append(Plots, list(out$plot))
  }
  return(list("File" = file_names, "DABs" = DABs, "R2s" = Rs, "fDABs" = fDABs,
              "Hs" = Hs, "Plots" = Plots))
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
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} and \code{\link{dab_pc}} functions used to calculate
#'   the diameter at breast height and above buttresses.
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
#' plot_crown_classification_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # Run the crown classification with non-default settings and save figures
#' plot_crown_classification_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/",
#'   thresholdbranch = 2, minheight = 4
#' )
#' # Run the crown classification with non-default settings and save figures
#' # for buttressed trees
#' plot_crown_classification_pcs(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/figure/folder/",
#'   thresholdbranch = 2, minheight = 4,
#'   buttress = TRUE, thresholdbuttress = 0.002,
#'   maxbuttressheight = 5
#' )
#' }
plot_crown_classification_pcs <- function(PCs_path, extension = ".txt",
                                          OUT_path = "./",
                                          thresholdbranch = 1.5,
                                          minheight = 1, buttress = FALSE,
                                          thresholdR2 = 0.001,
                                          slice_thickness = 0.06,
                                          thresholdbuttress = 0.001,
                                          maxbuttressheight = 7) {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    out <- classify_crown_pc(
      pc, thresholdbranch, minheight, buttress,
      thresholdR2, slice_thickness, thresholdbuttress,
      maxbuttressheight, TRUE
    )
    filename <- paste(OUT_path, "crown_",
      strsplit(file_names[i], extension)[[1]], "_",
      as.character(thresholdbranch), "_",
      as.character(minheight),
      ".jpeg",
      sep = ""
    )
    ggplot2::ggsave(filename,
      plot = out$plot, bg = "white", width = 60,
      height = 36, units = "cm"
    )
    Plots <- append(Plots, list(out$plot))
  }
  return(list("File" = file_names, "Plots" = Plots))
}

#' Calculate and save figures of \code{\link{projected_area_pc}} function
#'
#' Calculates the projected (crown) area and saves the figures acquired when
#' running \code{\link{projected_area_pc}} on multiple tree point clouds in a
#' folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{projected_area_pc}} with parameter plot = TRUE to calculate the
#' projected area and plot the crown projection and fitting. Choose crown =
#' TRUE, if you want to calculate and plot the projected crown area. In this
#' case a crown classification is done using \code{\link{classify_crown_pc}} and
#' the crown points are used as an input to \code{\link{projected_area_pc}}. For
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
#' @param concavity Numeric value (default=2) concavity for the computation of a
#'   concave hull based on \code{\link[concaveman]{concaveman}} in
#'   \code{\link{projected_area_pc}}.
#' @param crown Logical (default=FALSE), indicates if the area is calculated
#'   based on the full point clouds (crown = FALSE) or only on the crown point
#'   clouds (crown = TRUE).
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}. Only relevant when crown == TRUE.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#'   Only relevant when crown == TRUE.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height). Only relevant when crown == TRUE.
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when crown == TRUE and buttress == FALSE.
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} and \code{\link{dab_pc}} functions used to calculate
#'   the diameter at breast height and above buttresses.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when crown == TRUE and buttress == FALSE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when crown == TRUE and buttress == FALSE.
#'
#' @return A list with in the first element a numeric containing the projected
#'   area values for each tree point cloud. In the second element there is the
#'   list with the plots. Figures are also saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate projected area with default settings and save projection figures
#' pas <- plot_pa_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # With non-default settings and save projection figures
#' pas <- plot_pa_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/", concavity = 3
#' )
#' # Calculate projected crown area and save projection figures
#' pcas <- plot_pa_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/", concavity = 3,
#'   crown = TRUE, minheight = 4, buttress = TRUE
#' )
#' }
plot_pa_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                        concavity = 2, crown = FALSE, thresholdbranch = 1.5,
                        minheight = 1, buttress = FALSE, thresholdR2 = 0.001,
                        slice_thickness = 0.06, thresholdbuttress = 0.001,
                        maxbuttressheight = 7) {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  PAs <- c()
  Plots <- list()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    if (crown) {
      crown_pc <- classify_crown_pc(
        pc, thresholdbranch, minheight, buttress,
        thresholdR2, slice_thickness,
        thresholdbuttress, maxbuttressheight, FALSE
      )
      out <- projected_area_pc(crown_pc$crownpoints, concavity, TRUE)
      plot_area <- out$plot +
        ggplot2::ggtitle(bquote(PCA == .(round(out$pa, 2)) ~ m^2))
      filename <- paste(OUT_path, "pca_", strsplit(
        file_names[i],
        extension
      )[[1]],
      "_", as.character(concavity), ".jpeg",
      sep = ""
      )
    } else {
      out <- projected_area_pc(pc, concavity, TRUE)
      plot_area <- out$plot
      filename <- paste(OUT_path, "pa_",
        strsplit(file_names[i], extension)[[1]], "_", as.character(concavity),
        ".jpeg",
        sep = ""
      )
    }
    ggplot2::ggsave(filename,
      plot = plot_area, bg = "white", width = 60,
      height = 36, units = "cm"
    )
    PAs <- append(PAs, out$pa)
    Plots <- append(Plots, list(plot))
  }
  return(list("File" = file_names, "PAs" = PAs, "Plots" = Plots))
}

#' Calculate and save figures of \code{\link{alpha_volume_pc}} function
#'
#' Calculates the (crown) volume and saves the figures acquired when running
#' \code{\link{alpha_volume_pc}} on multiple tree point clouds in a folder.
#'
#' Uses \code{\link{read_tree_pc}} to read the point clouds and
#' \code{\link{alpha_volume_pc}} with parameter plot = TRUE to calculate the
#' volume and plot the 3D the alpha-shape fitting. Choose crown = TRUE, if you
#' want to calculate and plot the crown volume. In this case a crown
#' classification is done using \code{\link{classify_crown_pc}} and the crown
#' points are used as an input to \code{\link{alpha_volume_pc}}. For buttressed
#' trees, first optimise the thresholdbuttress, maxbuttressheight,
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
#'   in \code{\link{alpha_volume_pc}}.
#' @param crown Logical (default=FALSE), indicates if the volume is calculated
#'   based on the full point clouds (crown = FALSE) or only on the crown point
#'   clouds (crown = TRUE).
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}. Only relevant when crown == TRUE.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height). Only relevant when crown == TRUE.
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when crown == TRUE and buttress == FALSE.
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} and \code{\link{dab_pc}} functions used to calculate
#'   the diameter at breast height and above buttresses.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when crown == TRUE and buttress == FALSE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses which is used in \code{\link{classify_crown_pc}}. Only relevant
#'   when crown == TRUE and buttress == FALSE.
#'
#' @return a numeric containing the volume values for each tree point cloud.
#'   Figures are saved in the output folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate the volume with default settings and save alpha shape figures
#' vs <- plot_av_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/"
#' )
#' # Calculate the volume with non-default settings and save alpha shape figures
#' vs <- plot_av_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/", alpha = 2
#' )
#' # Calculate crown volume and save alpha shape figures
#' cvs <- plot_av_pcs(
#'   PCs_path = "path/to/folder/PCs/", extension = ".txt",
#'   OUT_path = "path/to/figure/folder/", alpha = 2,
#'   crown = TRUE, minheight = 4, buttress = TRUE
#' )
#' }
plot_av_pcs <- function(PCs_path, extension = ".txt", OUT_path = "./",
                        alpha = 1, crown = FALSE, thresholdbranch = 1.5,
                        minheight = 1, buttress = FALSE, thresholdR2 = 0.001,
                        slice_thickness = 0.06, thresholdbuttress = 0.001,
                        maxbuttressheight = 7) {
  file_paths <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = TRUE
  )
  file_names <- list.files(PCs_path,
    pattern = paste("*", extension, sep = ""),
    full.names = FALSE
  )
  AVs <- c()
  for (i in 1:length(file_names)) {
    print(paste("processing ", file_names[i]))
    pc <- read_tree_pc(file_paths[i])
    if (crown) {
      crown_pc <- classify_crown_pc(
        pc, thresholdbranch, minheight, buttress,
        thresholdR2, slice_thickness,
        thresholdbuttress, maxbuttressheight, FALSE
      )
      out <- alpha_volume_pc(crown_pc$crownpoints, alpha, TRUE)
      fig_name <- paste(OUT_path, "cv_",
        strsplit(file_names[i], extension)[[1]], "_", as.character(alpha),
        ".png",
        sep = ""
      )
    } else {
      out <- alpha_volume_pc(pc, alpha, TRUE)
      fig_name <- paste(OUT_path, "av_",
        strsplit(file_names[i], extension)[[1]], "_", as.character(alpha),
        ".png",
        sep = ""
      )
    }
    rgl::rgl.snapshot(fig_name, fmt = "png")
    rgl::close3d()
    AVs <- append(AVs, out$av)
    gc()
  }
  return(list("File" = file_names, "AVs" = AVs))
}

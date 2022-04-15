#' Mean tree point cloud position
#'
#' Returns the mean (X,Y,Z)-position of a tree point cloud.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#'
#' @return Numeric with the XYZ coordinates (location) of the tree stem.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the tree position
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' pos <- mean_tree_position_pc(pc = pc_tree)
#' }
mean_tree_position_pc <- function(pc) {
  X_mean <- mean(pc$X)
  Y_mean <- mean(pc$Y)
  Z_mean <- mean(pc$Z)
  return(c(X_mean, Y_mean, Z_mean))
}


#' Projected tree area point cloud
#'
#' Returns the projected tree area measured from a tree point cloud.
#'
#' This function uses \code{\link[sf]{st_area}} and
#' \code{\link[concaveman]{concaveman}} to calculate the area of the concave
#' hull fitted to the point cloud.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param concavity Numeric value (default=2) concavity for the computation of a
#'   concave hull based on \code{\link[concaveman]{concaveman}}.
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return The projected tree area (numeric value) as the area of the concave
#'   hull computed from the tree points of a tree point cloud. Also optionally
#'   (plot=TRUE) plots the concave hull fitting and in this case returns a list
#'   with the pta as first element and the plot as the second element.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the projected crown area
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' pta <- projected_tree_area_pc(pc = pc_tree)
#' # and plot the concave hull fitting
#' output <- projected_tree_area_pc(pc = pc_tree, plot = TRUE)
#' pta <- output$pta
#' }
projected_tree_area_pc <- function(pc, concavity = 2, plot = FALSE) {
  points <- sf::st_as_sf(unique(pc[1:2]), coords = c("X", "Y"))
  hull <- concaveman::concaveman(points, concavity)
  pta <- sf::st_area(hull)
  if (plot) {
    X <- Y <- NULL
    plotPTA <- ggplot2::ggplot() +
      ggplot2::geom_point(data = pc,
                          ggplot2::aes(X, Y, color = "tree points"),
                          size = 0.1, stroke = 0, shape = ".") +
      ggplot2::geom_sf(data = sf::st_geometry(hull),
                       ggplot2::aes(color = "concave hull"),
                       col = "red", show.legend = "line", size = 1, fill = NA) +
      ggplot2::ggtitle(bquote(PCA == .(round(pta,2)) ~ m^2)) +
      ggplot2::scale_color_manual(name = "",
                                  values = c("concave hull" = "red",
                                             "crown points" = "black"),
                                  guide = ggplot2::guide_legend(
                                    override.aes =
                                      list(linetype = c(1, 0),
                                           shape = c(NA, 16),
                                           size = c(2, 2))))
    print(plotPTA)
    return(list("pta" = pta,"plot" = plotPTA))
  } else {
    return(pta)
  }
}

#' Tree volume point cloud
#'
#' Returns the tree volume measured from a tree point cloud.
#'
#' This function uses \code{\link[alphashape3d]{ashape3d}} and
#' \code{\link[alphashape3d]{volume_ashape3d}} to calculate the volume of 3D the
#' alpha-shape fitted to the tree point cloud.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param alpha Numeric value (default=1) alpha for the computation of the 3D
#'   alpha-shape of the tree crown based on
#'   \code{\link[alphashape3d]{ashape3d}}.
#' @param plot Logical (default=FALSE), indicates if the alpha-shape is plotted.
#'
#' @return The volume of the tree (numeric value) as the volume of the 3D
#'   alpha-shape computed from the tree point cloud. Also optionally (plot=TRUE)
#'   plots the alpha-shape and in this case returns a list with the point cloud
#'   as first element and the alphashape3d object as the second element. The 3D
#'   plot can be reconstructed using plot(output$alphashape3d).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the crown volume
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' vol_tree <- volume_tree_pc(pc = pc_tree)
#' # and plot the 3D alpha-shape
#' output <- volume_tree_pc(pc = pc_tree, plot = TRUE)
#' vol_tree <- output$volume
#' }
volume_tree_pc <- function(pc, alpha = 1, plot = FALSE) {
  pc_norm <- normalize_pc(pc)
  tree_xyz <- data.matrix(unique(pc_norm[1:3]))
  ashape3d.obj <- alphashape3d::ashape3d(tree_xyz, alpha = alpha, pert = TRUE)
  vol_tree <- alphashape3d::volume_ashape3d(ashape3d.obj)
  if (plot) {
    graphics::par(pty = "s")
    rgl::bg3d("white")
    rgl::par3d(windowRect = c(20, 30, 800, 800))
    plot(ashape3d.obj)
    return(list("cv" = vol_tree, "ashape3d" = ashape3d.obj))
  } else {
    return(vol_tree)
  }
}

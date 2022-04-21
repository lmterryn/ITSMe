#' Tree point cloud position
#'
#' Returns the (X,Y)-position of a tree point cloud based on the mean X and Y
#' value of the points within a 6mm thick horizontal slice at breast height
#' (from 1.27 m to 1.33 m above the lowest tree point).
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#'
#' @return Numeric with the XY coordinates (location) of the tree stem.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the tree position
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' pos <- tree_position_pc(pc = pc_tree)
#' }
tree_position_pc <- function(pc) {
  slice_height <- 1.3
  slice_thickness <- 0.06
  pc_dbh <- pc[(pc$Z > min(pc$Z) + slice_height-slice_thickness/2) &
                 (pc$Z < min(pc$Z) + slice_height+slice_thickness/2), ]
  xy_dbh <- pc_dbh[, c("X", "Y")]
  XY_dbh <- data.matrix(xy_dbh)
  k <- 2
  knn1 <- nabor::knn(XY_dbh, XY_dbh, k = k, radius = 0)
  knn_ind <- data.frame(knn = knn1[[1]][, 2:k])
  knn_dist <- data.frame(knn.dist = knn1[[2]][, 2:k])
  remove <- which(knn_dist[, k - 1] > 0.05)
  if (length(remove) != 0) {
    xy_dbh <- xy_dbh[-remove, ]
  }
  x_dbh <- xy_dbh$X
  y_dbh <- xy_dbh$Y
  X_mean <- mean(x_dbh) # first estimate of the center
  Y_mean <- mean(y_dbh)
  return(c(X_mean, Y_mean))
}

#' Tree height point cloud
#'
#' Returns the tree height measured from a tree point cloud.
#'
#' The tree height is measured as the difference between the Z-value of the
#' highest and lowest point of the tree point cloud.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#'
#' @return The tree height (numeric value).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the tree height
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' tree_height <- tree_height_pc(pc = pc_tree)
#' }
tree_height_pc <- function(pc) {
  return(max(pc$Z) - min(pc$Z))
}

#' Distance from the center
#'
#' Calculates the distance of each 2D point (X,Y) in a point cloud from the
#' center (xc, yc) of a circle.
#'
#' Support function used to determine the DBH from a tree point cloud with
#' \code{\link{dbh_pc}} and \code{\link{dab_pc}}.
#'
#' @param x X values of the points.
#' @param y Y values of the points.
#' @param xc X-coordinate of the center.
#' @param yc Y-coordinate of the center.
#'
#' @return The distance of 2D points to the center
#'
#' @examples
#' \dontrun{
#' Ri <- calc_r(x_dbh, y_dbh, x_c, y_c)
#' R <- mean(Ri)
#' }
calc_r <- function(x, y, xc, yc) {
  return(sqrt((x - xc)**2 + (y - yc)**2))
}

#' Algebraic distance from the center
#'
#' Calculates the algebraic distance between the data points and the mean circle
#' centered at c=(xc, yc) based on \code{\link{calc_r}}.
#'
#' Support function used to determine the DBH from a tree point cloud with the
#' functions \code{\link{dbh_pc}} and \code{\link{dab_pc}}.
#'
#' @param c First estimate of the center coordinates to be optimised (xc,yc).
#' @param x X values of the points.
#' @param y Y values of the points.
#'
#' @return When optimised returns the optimised center estimate of the circle
#'   fitting.
#'
#' @examples
#' \dontrun{
#' center_estimate <- optim(par = c(x_m, y_m), fn = f, x = x_dbh, y = y_dbh)
#' }
f <- function(c, x, y) {
  Ri <- calc_r(x, y, c[1], c[2])
  return(sum((Ri - mean(Ri))**2))
}

#' Diameter at certain height point cloud
#'
#' Returns the diameter at a certain height of a tree measured from a tree point
#' cloud.
#'
#' The diameter is measured of the optimal circle fitted through a horizontal
#' slice. A least squares circle fitting algorithm was applied to find the
#' optimal fit. The height and thickness of the slice can be specified using
#' slice_height and slice_thickness parameters. This is also a Support function
#' used to determine the DBH from a tree point cloud with \code{\link{dbh_pc}}.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param slice_height Numeric value (default = 1.3) that determines the height
#'   above the lowest point of the point cloud at which the diameter is
#'   measured.
#' @param slice_thickness Numeric value (default = 0.6) that determines the
#'   thickness of the slice which is used to measure the diameter.
#' @param plot Logical (default=FALSE), indicates if the optimized circle
#'   fitting is plotted.
#'
#' @return A list with the diameter at a specified height (numeric value), the
#'   residual between circle fit and the points and the center of the circle
#'   fit. Also optionally (plot=TRUE) plots the circle fitting on the horizontal
#'   slice.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the diameter
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' diameter <- diameter_slice_pc(pc = pc_tree)
#' # and plot the circle fitting
#' output <- diameter_slice_pc(pc = pc_tree, plot = TRUE)
#' diameter <- output$diameter
#' residual <- output$R2
#' center <- out$center
#' }
diameter_slice_pc <- function(pc, slice_height = 0.1, slice_thickness = 0.06,
                           plot = FALSE){
  if (max(pc$Z) - min(pc$Z) > slice_height) {
    pc_slice <- pc[(pc$Z > min(pc$Z) + slice_height-slice_thickness/2) &
                     (pc$Z < min(pc$Z) + slice_height+slice_thickness/2), ]
    xy_slice <- pc_slice[, c("X", "Y")]
    XY_slice <- data.matrix(xy_slice)
    k <- 2
    knn1 <- nabor::knn(XY_slice, XY_slice, k = k, radius = 0)
    knn_ind <- data.frame(knn = knn1[[1]][, 2:k])
    knn_dist <- data.frame(knn.dist = knn1[[2]][, 2:k])
    remove <- which(knn_dist[, k - 1] > 0.05)
    if (length(remove) != 0) {
      xy_slice <- xy_slice[-remove, ]
    }
    x_slice <- xy_slice$X
    y_slice <- xy_slice$Y
    x_m <- mean(x_slice) # first estimate of the center
    y_m <- mean(y_slice)
    center_estimate <- stats::optim(par = c(x_m, y_m), fn = f, x = x_slice,
                                    y = y_slice)
    x_c <- center_estimate$par[1]
    y_c <- center_estimate$par[2]
    Ri <- calc_r(x_slice, y_slice, x_c, y_c)
    R <- mean(Ri) # radius (DBH/2)
    residu <- sum((Ri - R)**2) / length(Ri) # average residual
    diam <- 2 * R
    if (plot) {
      X <- Y <- x0 <- y0 <- r <- NULL
      data_circle <- data.frame(x0 = x_c, y0 = y_c, r = R)
      plotDIAM <- ggplot2::ggplot() +
        ggplot2::geom_point(data = xy_slice,
                            ggplot2::aes(X, Y, color = "points stem slice"),
                            size = 1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_point(data = data_circle,
                            ggplot2::aes(x0, y0, color = "estimated center"),
                            size = 1) +
        ggforce::geom_circle(data = data_circle,
                             ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                          color = "fitted circle"),
                             inherit.aes = FALSE, show.legend = TRUE,
                             size = 1) +
        ggplot2::ggtitle(paste("diameter = ", as.character(round(diam, 2)),
                               " m at H = ",
                               as.character(round(slice_height, 2)),
                               " m", sep = "")) +
        ggplot2::scale_color_manual(name = "",
                                    values = c("points stem slice" = "black",
                                               "estimated center" = "red",
                                               "fitted circle" = "blue"),
                                    guide = ggplot2::guide_legend(
                                      override.aes =
                                        list(linetype = c(0, 0, 1),
                                             shape = c(16, 16, NA),
                                             size = c(2, 2, 1))))
      print(plotDIAM)
      return(list("diameter" = diam, "R2" = residu, "center" = center_estimate,
                  "plot" = plotDIAM))
    } else {
      return(list("diameter" = diam, "R2" = residu, "center" = center_estimate))
    }
  } else {
    return(NaN)
  }
}

#' Extract lower trunk point cloud
#'
#' Returns the trunk points below 1.5 m (above the lowest point of the tree
#' point cloud).
#'
#' This function iteratively adds trunk points to the trunk point cloud starting
#' from 0.15 m above the lowest point of the tree point cloud (everything below
#' 0.15 m is assumed to be trunk). For each slice as many crown/branch points
#' are removed based on kmeans clustering and the distance of the clusters to
#' the center of the previous slice. Support function used to determine the DBH
#' from a tree point cloud with \code{\link{dbh_pc}}.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#'
#' @return Data.frame with the lower trunk point cloud (part of the trunk below
#'   1.5 m).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the DBH
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' trunk_pc <- extract_lower_trunk_pc(pc = pc_tree)
#' }
extract_lower_trunk_pc <- function(pc){
  diam <- diameter_slice_pc(pc = pc)
  d <- diam$diameter
  minheight <- 0.15
  dh <- 0.08
  lh <- minheight - dh/2
  uh <- minheight + dh/2
  pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
  center_trunk <- c(diam$center$par[1], diam$center$par[2])
  trunk_pc <- pc[pc$Z < min(pc$Z) + uh, ]
  crown_pc <- pc[FALSE, ]
  n <- 0
  while (lh < 1.5) {
    if (n > 0) {
      crown_pc <- rbind(crown_pc, pc_slice[
        pc_slice$C %in% crown,
        c("X", "Y", "Z")
      ])
      trunk_pc <- rbind(trunk_pc, trunk_slice)
    }
    n <- n + 1
    lh <- lh + dh/2
    uh <- uh + dh/2
    pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh) &
                     (pc$X > center_trunk[1]-0.75*d) &
                     (pc$X < center_trunk[1]+0.75*d) &
                     (pc$Y > center_trunk[2]-0.75*d) &
                     (pc$Y < center_trunk[2]+0.75*d), ]
    if (nrow(pc_slice) > 10){
      k10 <- stats::kmeans(pc_slice, centers = 10, nstart = 25, iter.max = 100)
      pc_slice$C <- k10$cluster
      distance_to_centers <- c()
      centers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      for (i in 1:10) {
       distance_to_centers <- append(
          distance_to_centers, ((k10$centers[i, "X"] - center_trunk[1])^2 +
            (k10$centers[i, "Y"] - center_trunk[2])^2)^(1/2))
      }
      crown <- centers[distance_to_centers > 2*d]
      trunk_slice <- pc_slice[!(pc_slice$C %in% crown), c("X", "Y", "Z")]
      if (nrow(trunk_slice) > 0){
        diam <- diameter_slice_pc(pc = rbind(trunk_pc, trunk_slice),
                             slice_height = lh+dh/2, slice_thickness = dh)
        if(diam$R2 > 0.001*diam$diameter){
          R <- sqrt(((trunk_slice$X - diam$center[[1]][1])^2 +
                       (trunk_slice$Y - diam$center[[1]][2])^2))
          trunk_slice <- trunk_slice[R < diam$diameter/2*1.1,]
          diam <- diameter_slice_pc(pc = rbind(trunk_pc, trunk_slice),
                                    slice_height = lh+dh/2,
                                    slice_thickness = dh)
        }
        d <- diam$diameter
        center_trunk <- c(diam$center$par[1], diam$center$par[2])
      }
    } else {
      diam <- diameter_slice_pc(pc = pc)
      d <- diam$diameter
      minheight <- 0.15
      dh <- dh + 0.02
      lh <- minheight - dh/2
      uh <- minheight + dh/2
      pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
      center_trunk <- c(diam$center$par[1], diam$center$par[2])
      trunk_pc <- pc[pc$Z < min(pc$Z) + uh, ]
      crown_pc <- pc[FALSE, ]
      n <- 0
    }
  }
  return(trunk_pc)
}

#' Diameter at breast height point cloud
#'
#' Returns the diameter at breast height (DBH) of a tree measured from a tree
#' point cloud.
#'
#' The DBH is measured as the diameter of the optimal circle fitted through a
#' 6mm thick horizontal slice (from 1.27 m to 1.33 m above the lowest tree
#' point) using \code{\link{diameter_slice_pc}}. A least squares circle fitting
#' algorithm is applied to find the optimal fit. In case there are branches or
#' foliage at this height, the lower trunk is extracted using
#' \code{\link{extract_lower_trunk_pc}}. Wether this is the case is determined
#' using the thresholdR2 parameter.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param thresholdR2 Numeric value (default=0.001) that is multiplied with the
#'   radius to determine if at breast height (1.3 m above the lowest point of
#'   the point cloud) the circle fit is influenced by branches. If the resulting
#'   value is exceeded, the lower trunk without branches is extracted using
#'   \code{\link{extract_lower_trunk_pc}}. Increase the thresholdR2 if your
#'   point cloud quality is low (for example, errors in co-registration of point
#'   clouds in multi-scan due to wind-effect).
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return Diameter of the stem at breast height (numeric value). Also
#'   optionally (plot=TRUE) plots the circle fitting on the horizontal slice and
#'   in this case returns a list with the dbh value as first element and the
#'   plot as the second element.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the DBH
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' dbh <- dbh_pc(pc = pc_tree)
#' # and plot the circle fitting
#' output <- dbh_pc(pc = pc_tree, plot = TRUE)
#' dbh <- output$dbh
#' }
dbh_pc <- function(pc, thresholdR2 = 0.001, plot = FALSE) {
  out <- diameter_slice_pc(pc = pc, slice_height = 1.3, slice_thickness = 0.06)
  if(out$R2 > thresholdR2*out$diameter | (out$diameter > 2 & out$R2 > 0.002)){
    trunk_pc <- extract_lower_trunk_pc(pc = pc)
    out <- diameter_slice_pc(pc = trunk_pc, slice_height = 1.3,
                             slice_thickness = 0.06)
  }
  if (plot){
    pc_dbh <- pc[(pc$Z > min(pc$Z) + 1.3-0.06/2) &
                 (pc$Z < min(pc$Z) + 1.3+0.06/2), ]
    X <- Y <- x0 <- y0 <- r <- NULL
    data_circle <- data.frame(x0 = out$center[[1]][1], y0 = out$center[[1]][2],
                              r = out$diameter/2)
    plotDBH <- ggplot2::ggplot() +
      ggplot2::geom_point(data = pc_dbh,
                          ggplot2::aes(X, Y, color = "points stem slice"),
                          size = 1) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::geom_point(data = data_circle,
                          ggplot2::aes(x0, y0, color = "estimated center"),
                          size = 1) +
      ggforce::geom_circle(data = data_circle,
                           ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                        color = "fitted circle"),
                           inherit.aes = FALSE, show.legend = TRUE,
                           size = 1) +
      ggplot2::ggtitle(paste("DBH = ",
                             as.character(round(out$diameter, 2)),
                             " m", sep = "")) +
      ggplot2::scale_color_manual(name = "",
                                  values = c("points stem slice" = "black",
                                             "estimated center" = "red",
                                             "fitted circle" = "blue"),
                                  guide = ggplot2::guide_legend(
                                    override.aes =
                                      list(linetype = c(0, 0, 1),
                                           shape = c(16, 16, NA),
                                           size = c(2, 2, 1))))
  print(plotDBH)
  return(list("dbh" = out$diameter, "plot" = plotDBH))
  } else {
    return(out$diameter)
  }
}

#' Diameter above buttresses point cloud
#'
#' Returns the diameter above buttresses (DAB) of a tree measured from a tree
#' point cloud.
#'
#' The DAB is measured as the diameter of the optimal circle fitted through a
#' 6mm thick horizontal slice taken above the buttresses. A least squares circle
#' fitting algorithm was applied to find the optimal fit. The height at which
#' the horizontal slice is taken, is determined iteratively. Starting at 1.27 m
#' to 1.33 m from the lowest point of the tree point cloud. The average residual
#' between the points and the fitted circle is calculated. When the average
#' residual exceeds a value of "thresholdbuttress" times the radius, indicating
#' a non-circular (irregular) stem shape and presumably buttresses, the process
#' is repeated with a new slice 6 mm higher than the previous one until a slice
#' above the buttresses is reached. When the "maxbuttressheight" is exceeded the
#' iterative process is restarted with a "thresholdbuttress" increased with
#' 0.0005.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param thresholdbuttress Numeric value (default=0.001) that is multiplied
#'   with the radius to determine if the stem is circular or irregular at the
#'   height the slice is taken. For example with the default value 0.001: when
#'   the average residual (obtained after an initial circle fitting at 1.3 m)
#'   exceeds a value of 0.001 times the radius, indicating a non-circular
#'   (irregular) stem shape and presumably buttresses, the circle fitting
#'   process is repeated with a new slice 6 mm higher than the previous one
#'   until a slice above the buttresses is reached.
#' @param maxbuttressheight Numeric value (default=7) that limits the height at
#'   which the diameter is measured. When this height is reached (because
#'   residuals do not become smaller than thresholdbuttress * R), the
#'   thresholdbuttress value is increased with 0.0005 and the fitting starts
#'   again at 1.3 m.
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return Diameter of the stem above buttresses (numeric value). Also
#'   optionally (plot=TRUE) plots the circle fitting on the horizontal slice and
#'   in this case returns a list with the dab value as first element and the
#'   plot as the second element.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the DAB
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' dab <- dab_pc(pc = pc_tree)
#' # and plot the circle fitting
#' output <- dab_pc(pc = pc_tree, plot = TRUE)
#' dab <- output$dab
#' # with non-default settings
#' dab <- dab_pc(pc = pc_tree, thresholdbuttress = 0.002, maxbuttressheight = 5)
#' }
dab_pc <- function(pc, thresholdbuttress = 0.001, maxbuttressheight = 7,
                   plot = FALSE) {
  lh <- 1.27
  uh <- 1.33
  residu <- 1
  R <- 0.5
  r_diff <- 1
  RES <- c()
  THRESH <- c()
  DBH_HEIGHT <- c()
  R_slices <- c()
  loop <- 1
  while (loop == 1) {
    while ((residu > thresholdbuttress * R) & (uh < maxbuttressheight) |
           (R > 2) | (r_diff > 2)) {
      if (max(pc$Z) - min(pc$Z) > (lh + uh) / 2) {
        pc_dbh <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
        xy_dbh <- pc_dbh[, c("X", "Y")]
        XY_dbh <- data.matrix(xy_dbh)
        k <- 2
        knn1 <- nabor::knn(XY_dbh, XY_dbh, k = k, radius = 0)
        knn_ind <- data.frame(knn = knn1[[1]][, 2:k])
        knn_dist <- data.frame(knn.dist = knn1[[2]][, 2:k])
        remove <- which(knn_dist[, k - 1] > 0.05)
        if (length(remove) != 0) {
          xy_dbh <- xy_dbh[-remove, ]
        }
        if (lh == 1.27) {
          dbh_slice <- xy_dbh
        }
        x_dbh <- xy_dbh$X
        y_dbh <- xy_dbh$Y
        x_m <- mean(x_dbh) # first estimate of the center
        y_m <- mean(y_dbh)
        center_estimate <- stats::optim(par = c(x_m, y_m), fn = f, x = x_dbh,
                                        y = y_dbh)
        x_c <- center_estimate$par[1]
        y_c <- center_estimate$par[2]
        Ri <- calc_r(x_dbh, y_dbh, x_c, y_c)
        R <- mean(Ri) # radius (DBH/2)
        residu <- sum((Ri - R)**2) / length(Ri) # average residual between
        if (lh == 1.27) {
          r_diff <- 1
        } else {
          r_diff <- utils::tail(R_slices, n = 1) / R
        }
        R_slices <- append(R_slices, R)
        RES <- append(RES, residu)
        THRESH <- append(THRESH, thresholdbuttress * R)
        DBH_HEIGHT <- append(DBH_HEIGHT, (lh + uh) / 2)
        lh <- lh + 0.06
        uh <- uh + 0.06
      } else {
        return(NaN)
      }
    }
    if (uh < maxbuttressheight) {
      lh <- lh - 0.06
      uh <- uh - 0.06
      loop <- 0
    } else {
      thresholdbuttress <- thresholdbuttress + 0.0005
      lh <- 1.27
      uh <- 1.33
      residu <- 1
      R <- 0.5
      RES <- c()
      THRESH <- c()
      DBH_HEIGHT <- c()
      R_slices <- c()
    }
  }
  dab <- 2 * R
  if (plot) {
    X <- Y <- x0 <- y0 <- r <- NULL
    data_circle <- data.frame(x0 = x_c, y0 = y_c, r = R)
    if (lh != 1.27) {
      plotDAB <- ggplot2::ggplot() +
        ggplot2::geom_point(data = dbh_slice,
                            ggplot2::aes(X, Y, color = "points at breast
                                         height"), size = 1) +
        ggplot2::geom_point(data = xy_dbh,
                            ggplot2::aes(X, Y, color = "points above
                                         buttresses"), colour = "black",
                            size = 1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_point(data = data_circle,
                            ggplot2::aes(x0, y0, color = "estimated center")) +
        ggforce::geom_circle(data = data_circle,
                             ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                          color = "fitted circle"),
                             inherit.aes = FALSE, show.legend = TRUE) +
        ggplot2::ggtitle(paste("DAB = ", as.character(round(dab, 2)),
                               " m at H = ",
                               as.character(round((lh + uh) / 2, 2)),
                               " m", sep = "")) +
        ggplot2::scale_color_manual(name = "",
                                    values = c("points above buttresses" =
                                                 "black",
                                               "estimated center" = "red",
                                               "points at breast height" =
                                                 "grey",
                                               "fitted circle" = "blue"),
                                    guide = ggplot2::guide_legend(
                                      override.aes =
                                        list(linetype = c(0, 0, 0, 1),
                                             shape = c(16, 16, 16, NA),
                                             size = c(2, 2, 2, 1))))
    } else {
      plotDAB <- ggplot2::ggplot() +
        ggplot2::geom_point(data = xy_dbh,
                            ggplot2::aes(X, Y, color = "points at breast
                                         height"),
                            size = 1, colour = "black") +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_point(data = data_circle,
                            ggplot2::aes(x0, y0, color = "estimated center")) +
        ggforce::geom_circle(data = data_circle,
                             ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                          color = "fitted circle"),
                             inherit.aes = FALSE, show.legend = TRUE) +
        ggplot2::ggtitle(paste("DBH = ", as.character(round(dab, 2)),
                               " m", sep = "")) +
        ggplot2::scale_color_manual(name = "",
                                    values = c("points at breast height" =
                                                 "black",
                                               "estimated center" = "red",
                                               "fitted circle" = "blue"),
                                    guide = ggplot2::guide_legend(
                                      override.aes =
                                        list(linetype = c(0, 0, 1),
                                             shape = c(16, 16, NA),
                                             size = c(2, 2, 1))))
    }
    print(plotDAB)
    return(list("dab" = dab,"plot" = plotDAB))
  } else {
    return(dab)
  }
}

#' Crown classification point cloud
#'
#' Returns the crown points from a tree point cloud.
#'
#' The classification is based on the increased distance between the minimum and
#' maximum X (and Y) coordinates of the tree points within a horizontal slice
#' when the first branch is reached with increasing height.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param thresholdbranch Numeric value (default=1.5) that is multiplied with
#'   the diameter of the tree (calculated with \code{\link{dbh_pc}} or
#'   \code{\link{dab_pc}} when buttress =TRUE) which determines the cutt-off
#'   where a branch emerges and the crown begins.
#' @param minheight Numeric value (default=1) with the minimum height at which
#'   the crown begins. Should be above the widest part of the buttresses for
#'   buttressed trees (value of 4 is recommended). For non-buttressed trees
#'   choose a lower value (such as 1).
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == TRUE.
#' @param plot Logical (default=FALSE), indicates if the classified tree is
#'   plotted.
#'
#' @return Data.frame with the crown point cloud (part of the tree above the
#'   first branch). Also optionally (plot=TRUE) plots the crown vs non-crown
#'   points and in this case returns a list with the crown point cloud as first
#'   element and the plot as the second element.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and extract the crown points
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' crown_pc <- classify_crown_pc(pc = pc_tree)
#' # and plot the classification results
#' output <- classify_crown_pc(pc = pc_tree, plot = TRUE)
#' crown_pc <- output$crownpoints
#' # with non-default settings for a buttressed tree
#' crown_pc <- classify_crown_pc(pc = pc_tree, minheight = 4, buttress = TRUE)
#' }
classify_crown_pc <- function(pc, thresholdbranch = 1.5, minheight = 1,
                              buttress = FALSE, thresholdR2 = 0.001,
                              thresholdbuttress = 0.001, maxbuttressheight = 7,
                              plot = FALSE) {
  if (buttress) {
    dab <- dab_pc(pc, thresholdbuttress, maxbuttressheight)
  } else {
    dab <- dbh_pc(pc, thresholdR2)
  }
  d <- thresholdbranch * dab + 0.1
  dh <- 0.25
  lh <- minheight - dh # initiation first slice
  uh <- minheight # initiation first slice
  S_X <- S_Y <- 0
  n <- 0
  while ((S_X < d) & (S_Y < d)) {
    n <- n + 1
    lh <- lh + dh
    uh <- uh + dh
    pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
    if (nrow(pc_slice) == 0) {
      S_X <- S_Y <- 0
    } else {
      xy_dbh <- pc_slice[, c("X", "Y")]
      XY_dbh <- data.matrix(xy_dbh)
      k <- 2
      distance <- dab / 10
      knn1 <- nabor::knn(XY_dbh, XY_dbh, k = k, radius = 0)
      knn_ind <- data.frame(knn = knn1[[1]][, 2:k])
      knn_dist <- data.frame(knn.dist = knn1[[2]][, 2:k])
      remove <- which(knn_dist[, k - 1] > distance)
      if (length(remove) != 0) {
        pc_slice <- pc_slice[-remove, ]
      }
      if (nrow(pc_slice) != 0) {
        S_X <- max(pc_slice$X) - min(pc_slice$X)
        S_Y <- max(pc_slice$Y) - min(pc_slice$Y)
      } else {
        S_X <- S_Y <- 0
      }
    }
  }
  if (n == 1){
    while (((S_X > d) | (S_Y > d)) & (lh > dh)){
      lh <- lh - dh/10
      uh <- uh - dh/10
      pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
      if (nrow(pc_slice) == 0) {
        S_X <- S_Y <- 0
      } else {
        xy_dbh <- pc_slice[, c("X", "Y")]
        XY_dbh <- data.matrix(xy_dbh)
        k <- 2
        distance <- dab / 10
        knn1 <- nabor::knn(XY_dbh, XY_dbh, k = k, radius = 0)
        knn_ind <- data.frame(knn = knn1[[1]][, 2:k])
        knn_dist <- data.frame(knn.dist = knn1[[2]][, 2:k])
        remove <- which(knn_dist[, k - 1] > distance)
        if (length(remove) != 0) {
          pc_slice <- pc_slice[-remove, ]
        }
        if (nrow(pc_slice) != 0) {
          S_X <- max(pc_slice$X) - min(pc_slice$X)
          S_Y <- max(pc_slice$Y) - min(pc_slice$Y)
        } else {
          S_X <- S_Y <- 0
        }
      }
    }
  }
  lh <- lh - dh
  uh <- uh - dh
  pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
  k1 <- stats::kmeans(pc_slice, centers = 1, nstart = 10, iter.max = 100)
  pc_slice$C <- k1$cluster
  center_trunk <- k1$centers
  trunk_pc <- pc[pc$Z < min(pc$Z) + uh, ]
  crown_pc <- pc[FALSE, ]
  d <- thresholdbranch * dab
  S_X <- S_Y <- n <- stop <- 0
  while ((S_X < d) & (S_Y < d) & (stop == 0)) {
    if (n > 0) {
      crown_pc <- rbind(crown_pc,
                        pc_slice[pc_slice$C %in% crown, c("X", "Y", "Z")])
      trunk_pc <- rbind(trunk_pc, trunk_slice)
    }
    n <- n + 1
    lh <- lh + dh
    uh <- uh + dh
    pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh), ]
    k10 <- stats::kmeans(pc_slice, centers = 10, nstart = 25, iter.max = 100)
    pc_slice$C <- k10$cluster
    distance_to_centers <- c()
    centers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    for (i in 1:10) {
      distance_to_centers <- append(
        distance_to_centers,
        ((k10$centers[i, "X"] - k1$centers[1, "X"])^2 +
          (k10$centers[i, "Y"] - k1$centers[1, "Y"])^2)^(1 / 2))
    }
    crown <- centers[distance_to_centers > d]
    trunk_slice <- pc_slice[!(pc_slice$C %in% crown), c("X", "Y", "Z")]
    if (nrow(trunk_slice) == 0) {
      stop <- 1
      S_X <- S_Y <- 0
    } else {
      k1 <- stats::kmeans(trunk_slice, centers = 1, nstart = 25, iter.max = 100)
      center_trunk <- k1$centers
      S_X <- max(trunk_slice$X) - min(trunk_slice$X)
      S_Y <- max(trunk_slice$Y) - min(trunk_slice$Y)
    }
  }
  crown_pc <- rbind(crown_pc, pc[pc$Z > min(pc$Z) + lh, ])
  if (plot) {
    downsample <- 0.1
    crown <- crown_pc[sample(nrow(crown_pc),
      size = floor(nrow(crown_pc) * downsample),
      replace = FALSE, prob = NULL
    ), ]
    crown$class <- "crown"
    X <- Y <- Z <- NULL
    if (nrow(trunk_pc) == 0) {
      tree <- crown
      tree$Z <- tree$Z - min(tree$Z)
      plotXZ <- ggplot2::ggplot(tree, ggplot2::aes(X, Z)) +
        ggplot2::geom_point(size = 0.1, ggplot2::aes(col = class),
                            shape = ".") +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0,0,0,0), "lines")) +
        ggplot2::scale_color_manual(name = "class",
                                    values = c("crown" = "green",
                                               "trunk" = "brown"),
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(shape = c(16, 16),
                                                          size = c(2, 2))))
      plotYZ <- ggplot2::ggplot(tree, ggplot2::aes(Y, Z)) +
        ggplot2::geom_point(size = 0.1, ggplot2::aes(col = class),
                            shape = ".") +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0,0,0,0), "lines")) +
        ggplot2::scale_color_manual(name = "class",
                                    values = c("crown" = "green",
                                               "trunk" = "brown"),
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(shape = c(16, 16),
                                                          size = c(2, 2))))
      s <- (max(pc$X)-min(pc$X)+max(pc$Y)-min(pc$Y))/(max(pc$Z)-
                                                        min(pc$Z))*0.5-1.05
      plotCrown <- ggpubr::ggarrange(plotXZ, NULL, plotYZ, nrow = 1, ncol = 3,
                                     common.legend = TRUE, heights=c(5,5),
                                     widths = c(1, s ,1))
    } else {
      trunk <- trunk_pc[sample(nrow(trunk_pc),
        size = floor(nrow(trunk_pc) * downsample),
        replace = FALSE, prob = NULL
      ), ]
      trunk$class <- "trunk"
      tree <- rbind(crown, trunk)
      tree$Z <- tree$Z - min(tree$Z)
      plotXZ <- ggplot2::ggplot(tree, ggplot2::aes(X, Z)) +
        ggplot2::geom_point(size = 0.1, ggplot2::aes(col = class),
                            shape = ".") +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0,0,0,0), "lines")) +
        ggplot2::scale_color_manual(name = "class",
                                    values = c("crown" = "green",
                                               "trunk" = "brown"),
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(shape = c(16, 16),
                                                          size = c(2, 2))))
      plotYZ <- ggplot2::ggplot(tree, ggplot2::aes(Y, Z)) +
        ggplot2::geom_point(size = 0.1, ggplot2::aes(col = class),
                            shape = ".") +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0,0,0,0), "lines")) +
        ggplot2::scale_color_manual(name = "class",
                                    values = c("crown" = "green",
                                               "trunk" = "brown"),
                                    guide = ggplot2::guide_legend(
                                      override.aes = list(shape = c(16, 16),
                                                          size = c(2, 2))))
      s <- (max(pc$X)-min(pc$X)+max(pc$Y)-min(pc$Y))/(max(pc$Z)-
                                                        min(pc$Z))*0.5-1.05
      plotCrown <- ggpubr::ggarrange(plotXZ, NULL, plotYZ, nrow = 1, ncol = 3,
                                     common.legend = TRUE, heights=c(5,5),
                                     widths = c(1, s ,1))
    }
    print(plotCrown)
    return(list("crownpoints" = crown_pc, "trunkpoints" = trunk_pc,
                "plot" = plotCrown, "plotXZ" = plotXZ, "plotYZ" = plotYZ))
  } else {
    return(list("crownpoints" = crown_pc, "trunkpoints" = trunk_pc))
  }
}

#' Normalize a tree point cloud
#'
#' Normalizes a tree point cloud by subtracting from each column its respective
#' the min value (e.g. all X-values - min(all X-values)).
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#'
#' @return Normalized point cloud as a data.frame with columns X,Y,Z.
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and normalise the point cloud
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' pc_norm <- normalize_pc(pc)
#' }
normalize_pc <- function(pc) {
  pc$X <- pc$X - min(pc$X)
  pc$Y <- pc$Y - min(pc$Y)
  pc$Z <- pc$Z - min(pc$Z)
  return(pc)
}

#' Projected crown area point cloud
#'
#' Returns the projected crown area measured from a tree point cloud.
#'
#' This function uses \code{\link[sf]{st_area}} and
#' \code{\link[concaveman]{concaveman}} to calculate the area of the concave
#' hull fitted to the crown points obtained with
#' \code{\link{classify_crown_pc}}.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param concavity Numeric value (default=2) concavity for the computation of a
#'   concave hull based on \code{\link[concaveman]{concaveman}}.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == TRUE.
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return The projected crown area (numeric value) as the area of the concave
#'   hull computed from the crown points of a tree point cloud. Also optionally
#'   (plot=TRUE) plots the concave hull fitting and in this case returns a list
#'   with the pca as first element and the plot as the second element.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the projected crown area
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' pca <- projected_crown_area_pc(pc = pc_tree)
#' # and plot the concave hull fitting
#' output <- projected_crown_area_pc(pc = pc_tree, plot = TRUE)
#' pca <- output$pca
#' # with non-default settings for a buttressed tree
#' pca <- projected_crown_area_pc(pc = pc_tree, concavity = 3, minheight = 4,
#'                                buttress = TRUE)
#' }
projected_crown_area_pc <- function(pc, concavity = 2, thresholdbranch = 1.5,
                                    minheight = 1, buttress = FALSE,
                                    thresholdR2 = 0.001,
                                    thresholdbuttress = 0.001,
                                    maxbuttressheight = 7, plot = FALSE) {
  crown_pc <- classify_crown_pc(pc, thresholdbranch, minheight, buttress,
                                thresholdR2, thresholdbuttress,
                                maxbuttressheight, FALSE)
  points <- sf::st_as_sf(unique(crown_pc$crownpoints[1:2]),
                         coords = c("X", "Y"))
  hull <- concaveman::concaveman(points, concavity)
  pca <- sf::st_area(hull)
  if (plot) {
    X <- Y <- NULL
    plotPCA <- ggplot2::ggplot() +
      ggplot2::geom_point(data = crown_pc$crownpoints,
                          ggplot2::aes(X, Y, color = "crown points"),
                          size = 0.1, stroke = 0, shape = ".") +
      ggplot2::geom_sf(data = sf::st_geometry(hull),
                       ggplot2::aes(color = "concave hull"),
                       col = "red", show.legend = "line", size = 1, fill = NA) +
      ggplot2::ggtitle(bquote(PCA == .(round(pca,2)) ~ m^2)) +
      ggplot2::scale_color_manual(name = "",
                                  values = c("concave hull" = "red",
                                             "crown points" = "black"),
                                  guide = ggplot2::guide_legend(
                                    override.aes =
                                      list(linetype = c(1, 0),
                                           shape = c(NA, 16),
                                           size = c(2, 2))))
    print(plotPCA)
    return(list("pca" = pca,"plot" = plotPCA))
  } else {
    return(pca)
  }
}

#' Crown volume point cloud
#'
#' Returns the crown volume measured from a tree point cloud.
#'
#' This function uses \code{\link[alphashape3d]{ashape3d}} and
#' \code{\link[alphashape3d]{volume_ashape3d}} to calculate the volume of 3D the
#' alpha-shape fitted to the crown points obtained with
#' \code{\link{classify_crown_pc}}.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param alpha Numeric value (default=1) alpha for the computation of the 3D
#'   alpha-shape of the tree crown based on
#'   \code{\link[alphashape3d]{ashape3d}}.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height). Only relevant if the tree point
#'   clouds are available.
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == FALSE.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == TRUE.
#' @param plot Logical (default=FALSE), indicates if the alpha-shape is plotted.
#'
#' @return The volume of the tree crown (numeric value) as the volume of the 3D
#'   alpha-shape computed from the crown points of a tree point cloud. Also
#'   optionally (plot=TRUE) plots the alpha-shape and in this case returns a
#'   list with the crown point cloud as first element and the alphashape3d
#'   object as the second element. The 3D plot can be reconstructed using
#'   plot(output$alphashape3d).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree point cloud and calculate the crown volume
#' pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' vol_crown <- volume_crown_pc(pc = pc_tree)
#' # and plot the 3D alpha-shape
#' output <- volume_crown_pc(pc = pc_tree, plot = TRUE)
#' vol_crown <- output$volume
#' # with non-default settings for a buttressed tree
#' vol_crown <- volume_crown_pc(pc = pc_tree, alpha = 2, minheight = 4)
#' }
volume_crown_pc <- function(pc, alpha = 1, thresholdbranch = 1.5, minheight = 1,
                            buttress = FALSE, thresholdR2 = 0.001,
                            thresholdbuttress = 0.001, maxbuttressheight = 7,
                            plot = FALSE) {
  crown_pc <- classify_crown_pc(pc, thresholdbranch, minheight, thresholdR2,
                                buttress, thresholdbuttress, maxbuttressheight,
                                FALSE)
  crown_pc_norm <- normalize_pc(crown_pc$crownpoints)
  crown_xyz <- data.matrix(unique(crown_pc_norm[1:3]))
  ashape3d.obj <- alphashape3d::ashape3d(crown_xyz, alpha = alpha)
  vol_crown <- alphashape3d::volume_ashape3d(ashape3d.obj)
  if (plot) {
    graphics::par(pty = "s")
    rgl::bg3d("white")
    rgl::par3d(windowRect = c(20, 30, 800, 800))
    plot(ashape3d.obj)
    return(list("cv" = vol_crown, "ashape3d" = ashape3d.obj))
  } else {
    return(vol_crown)
  }
}

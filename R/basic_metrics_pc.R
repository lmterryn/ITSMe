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
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' pos <- tree_position_pc(pc)
#' }
tree_position_pc <- function(pc){
  pc_bh <- pc[(pc$Z > min(pc$Z) + 1.27) & (pc$Z < min(pc$Z) + 1.33),]
  X_mean <- mean(pc_bh$X)
  Y_mean <- mean(pc_bh$Y)
  return(c(X_mean,Y_mean))
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
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' tree_height <- tree_height_pc(pc)
#' }
tree_height_pc <-function(pc) {
  return(max(pc$Z)-min(pc$Z))
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
#' Ri <- calc_r(x_dbh,y_dbh,x_c,y_c)
#' R <- mean(Ri)
#' }
calc_r <- function(x, y, xc, yc){
  return(sqrt((x-xc)**2 + (y-yc)**2))
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
#' center_estimate <- optim(par=c(x_m,y_m), fn=f, x=x_dbh, y=y_dbh)
#' }
f <- function(c, x, y){
  Ri <- calc_r(x, y, c[1], c[2])
  return(sum((Ri-mean(Ri))**2))
}

#' Diameter at breast height point cloud
#'
#' Returns the diameter at breast height (DBH) of a tree measured from a tree
#' point cloud.
#'
#' The DBH is measured as the diameter of the optimal circle fitted through a
#' 6mm thick horizontal slice (from 1.27 m to 1.33 m above the lowest tree
#' point). A least squares circle fitting algorithm was applied to find the
#' optimal fit.
#'
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}.
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return Diameter of the stem at breast height (numeric value). Also
#'   optionally (plot=TRUE) plots the circle fitting on the horizontal slice.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' dbh <- dbh_pc(pc)
#' dbh <- dbh_pc(pc,TRUE)
#' }
dbh_pc <-function(pc,plot=FALSE) {
  if(max(pc$Z)-min(pc$Z) > 1.3){
    pc_dbh <- pc[(pc$Z > min(pc$Z) + 1.27) & (pc$Z < min(pc$Z) + 1.33),]
    xy_dbh <- pc_dbh[,c("X","Y")]
    XY_dbh <- data.matrix(xy_dbh)
    k <- 2
    knn1 <- nabor::knn(XY_dbh, XY_dbh, k=k, radius = 0)
    knn_ind <- data.frame(knn=knn1[[1]][,2:k])
    knn_dist <- data.frame(knn.dist=knn1[[2]][,2:k])
    remove <- which(knn_dist[,k-1]>0.05)
    if(length(remove)!=0){
      xy_dbh <- xy_dbh[-remove,]
    }
    x_dbh <- xy_dbh$X
    y_dbh <- xy_dbh$Y
    x_m <- mean(x_dbh) #first estimate of the center
    y_m <- mean(y_dbh)
    center_estimate <- stats::optim(par=c(x_m,y_m), fn=f, x=x_dbh, y=y_dbh)
    x_c <- center_estimate$par[1]
    y_c <- center_estimate$par[2]
    Ri <- calc_r(x_dbh,y_dbh,x_c,y_c)
    R <- mean(Ri) #radius (DBH/2)
    residu <- sum((Ri - R)**2)/length(Ri) #average residual
    if(plot){
      X <- Y <- x0 <- y0 <- r <- NULL
      data_circle <- data.frame(x0 = x_c, y0 = y_c, r = R)
      plotDBH <- ggplot2::ggplot() +
        ggplot2::geom_point(data=xy_dbh, ggplot2::aes(X,Y), size=1) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::geom_point(data=data_circle,
                            ggplot2::aes(x0,y0, color="estimated center")) +
        ggforce::geom_circle(data=data_circle,
                             ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                          color="fitted circle"),
                             inherit.aes = FALSE, show.legend = FALSE) +
        ggplot2::ggtitle(paste("DBH = ",as.character(round(2*R,2)),"m",sep = ""))
      print(plotDBH)
    }
    dbh <- 2*R
  } else {
    dbh <- NaN
  }
  return(dbh)
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
#' @param maxbuttressheight Numeric value (default=9) that limits the height at
#'   which the diameter is measured. When this height is reached (because
#'   residuals do not become smaller than thresholdbuttress * R), the
#'   thresholdbuttress value is increased with 0.0005 and the fitting starts
#'   again at 1.3 m.
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return Diameter of the stem above buttresses (numeric value). Also
#'   optionally (plot=TRUE) plots the circle fitting on the horizontal slice.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' dab <- dab_pc(pc)
#' dab <- dab_pc(pc,0.001,9,TRUE)
#' }
dab_pc <- function(pc,thresholdbuttress=0.001,maxbuttressheight=9,
                           plot=FALSE){
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
  while(loop ==1){
    while((residu > thresholdbuttress*R) & (uh < maxbuttressheight) | (R > 2) |
          (r_diff > 2)){
      if(max(pc$Z)-min(pc$Z) > (lh+uh)/2){
        pc_dbh <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh),]
        xy_dbh <- pc_dbh[,c("X","Y")]
        XY_dbh <- data.matrix(xy_dbh)
        k <- 2
        knn1 <- nabor::knn(XY_dbh, XY_dbh, k=k, radius = 0)
        knn_ind <- data.frame(knn=knn1[[1]][,2:k])
        knn_dist <- data.frame(knn.dist=knn1[[2]][,2:k])
        remove <- which(knn_dist[,k-1]>0.05)
        if(length(remove)!=0){
          xy_dbh <- xy_dbh[-remove,]
        }
        x_dbh <- xy_dbh$X
        y_dbh <- xy_dbh$Y
        x_m <- mean(x_dbh) #first estimate of the center
        y_m <- mean(y_dbh)
        center_estimate <- stats::optim(par=c(x_m,y_m), fn=f, x=x_dbh, y=y_dbh)
        x_c <- center_estimate$par[1]
        y_c <- center_estimate$par[2]
        Ri <- calc_r(x_dbh,y_dbh,x_c,y_c)
        R <- mean(Ri) #radius (DBH/2)
        residu <- sum((Ri - R)**2)/length(Ri) #average residual between
        if(lh ==1.27){
          r_diff <- 1
        } else {
          r_diff <- utils::tail(R_slices, n=1)/R
        }
        R_slices <- append(R_slices,R)
        RES <- append(RES,residu)
        THRESH <- append(THRESH,thresholdbuttress*R)
        DBH_HEIGHT <- append(DBH_HEIGHT,(lh+uh)/2)
        lh <- lh+0.06
        uh <- uh+0.06
      }else{
        dbh <- NaN
      }
    }
    if(uh < 9){
      lh <- lh-0.06
      uh <- uh-0.06
      loop <- 0
    }else {
      thresholdbuttress <- thresholdbuttress+0.0005
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
  if (plot) {
    X <- Y <- x0 <- y0 <- r <- NULL
    data_circle <- data.frame(x0 = x_c, y0 = y_c, r = R)
    plotDAB <- ggplot2::ggplot() +
      ggplot2::geom_point(data=xy_dbh, ggplot2::aes(X,Y), size=1) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::geom_point(data=data_circle,
                          ggplot2::aes(x0,y0, color="estimated center")) +
      ggforce::geom_circle(data=data_circle,
                           ggplot2::aes(x0 = x0, y0 = y0, r = r,
                                        color="fitted circle"),
                           inherit.aes = FALSE, show.legend = FALSE) +
      ggplot2::ggtitle(paste("DAB = ",as.character(round(2*R,2)),"m at H = ",
                             as.character(round((lh+uh)/2,2)),"m",sep = ""))
    print(plotDAB)
  }
  dbh <- 2*R
  return(dbh)
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
#'   the diameter of the tree (calculated with \code{\link{dab_pc}})
#'   which determines the cutt-off where a branch emerges and the crown begins.
#' @param minheight Numeric value (default=4) with the minimum height at which
#'   the crown begins. Should be above the widest part of the buttresses.
#' @param plot Logical (default=FALSE), indicates if the classified tree is
#'   plotted.
#'
#' @return Data.frame with the crown point cloud (part of the tree above the
#'   first branch). Also optionally (plot=TRUE) plots the crown vs non-crown
#'   points.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' crown_pc <- classify_crown_pc(pc)
#' crown_pc <- classify_crown_pc(pc,1.5,4,TRUE)
#' }
classify_crown_pc <- function(pc,thresholdbranch=1.5,minheight=4,
                                      plot=FALSE){
  dab <- dab_pc(pc)
  d <- thresholdbranch*dab+0.1
  dh <- 0.25
  lh <- minheight-dh #initiation first slice
  uh <- minheight #initiation first slice
  S_X <- S_Y <- 0
  while ((S_X < d) & (S_Y < d)) {
    lh <- lh + dh
    uh <- uh + dh
    pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh),]
    if (nrow(pc_slice)==0) {
      S_X <- S_Y <- 0
    } else {
      xy_dbh <- pc_slice[,c("X","Y")]
      XY_dbh <- data.matrix(xy_dbh)
      k <- 2
      distance <- dab/10
      knn1 <- nabor::knn(XY_dbh, XY_dbh, k=k, radius = 0)
      knn_ind <- data.frame(knn=knn1[[1]][,2:k])
      knn_dist <- data.frame(knn.dist=knn1[[2]][,2:k])
      remove <- which(knn_dist[,k-1]>distance)
      if(length(remove)!=0){
        pc_slice <- pc_slice[-remove,]
      }
      if (nrow(pc_slice)!=0){
        S_X <- max(pc_slice$X)-min(pc_slice$X)
        S_Y <- max(pc_slice$Y)-min(pc_slice$Y)
      } else {
        S_X <- S_Y <- 0
      }
    }
  }
  lh <- lh-dh
  uh <- uh-dh
  pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh),]
  k1 <- stats::kmeans(pc_slice, centers = 1, nstart = 10)
  pc_slice$C <- k1$cluster
  center_trunk <- k1$centers
  trunk_pc <- pc[pc$Z < min(pc$Z)+uh,]
  crown_pc <- pc[FALSE,]
  d <- thresholdbranch*dab
  S_X <- S_Y <- n <- stop <- 0
  while((S_X < d) & (S_Y < d) & (stop==0)){
    if (n>0){
      crown_pc <- rbind(crown_pc, pc_slice[pc_slice$C %in% crown,
                                           c("X","Y","Z")])
      trunk_pc <- rbind(trunk_pc, trunk_slice)
    }
    n <- n + 1
    lh <- lh+dh
    uh <- uh+dh
    pc_slice <- pc[(pc$Z > min(pc$Z) + lh) & (pc$Z < min(pc$Z) + uh),]
    k10 <- stats::kmeans(pc_slice, centers = 10, nstart = 25)
    pc_slice$C <- k10$cluster
    distance_to_centers <- c()
    centers <- c(1,2,3,4,5,6,7,8,9,10)
    for (i in 1:10){
      distance_to_centers <- append(distance_to_centers,
                                    ((k10$centers[i,"X"]-k1$centers[1,"X"])^2+
                                       (k10$centers[i,"Y"]-k1$centers[1,"Y"])^2+
                                       (k10$centers[i,"Z"]-
                                          k1$centers[1,"Z"])^2)^(1/2))
    }
    crown <- centers[distance_to_centers>d]
    trunk_slice <- pc_slice[!(pc_slice$C %in% crown),c("X","Y","Z")]
    if(nrow(trunk_slice)==0){
      stop <- 1
      S_X <- S_Y <- 0
    }else{
      k1 <- stats::kmeans(trunk_slice, centers = 1, nstart = 25)
      center_trunk <- k1$centers
      S_X <- max(trunk_slice$X)-min(trunk_slice$X)
      S_Y <- max(trunk_slice$Y)-min(trunk_slice$Y)
    }
  }
  crown_pc <- rbind(crown_pc,pc[pc$Z > min(pc$Z)+lh,])
  if (plot){
    downsample <- 0.1
    crown <- crown_pc[sample(nrow(crown_pc),
                             size = floor(nrow(crown_pc)*downsample),
                             replace = FALSE, prob = NULL), ]
    crown$class <- "crown"
    X <- Y <- Z <- NULL
    if (nrow(trunk_pc)==0){
      tree <- crown
      plotXZ <- ggplot2::ggplot(tree, ggplot2::aes(X,Z)) +
        ggplot2::geom_point(size=1,ggplot2::aes(col=class)) +
        ggplot2::coord_fixed(ratio = 1)
      plotYZ <- ggplot2::ggplot(tree, ggplot2::aes(Y,Z)) +
        ggplot2::geom_point(size=1,ggplot2::aes(col=class)) +
        ggplot2::coord_fixed(ratio = 1)
      gridExtra::grid.arrange(plotXZ, plotYZ, ncol=2)
    } else {
      trunk <- trunk_pc[sample(nrow(trunk_pc),
                               size = floor(nrow(trunk_pc)*downsample),
                               replace = FALSE, prob = NULL), ]
      trunk$class <- "trunk"
      tree <- rbind(crown,trunk)
      plotXZ <- ggplot2::ggplot(tree, ggplot2::aes(X,Z)) +
        ggplot2::geom_point(size=1,ggplot2::aes(col=class)) +
        ggplot2::coord_fixed(ratio = 1)
      plotYZ <- ggplot2::ggplot(tree, ggplot2::aes(Y,Z)) +
        ggplot2::geom_point(size=1,ggplot2::aes(col=class)) +
        ggplot2::coord_fixed(ratio = 1)
      gridExtra::grid.arrange(plotXZ, plotYZ, ncol=2)
    }
  }
  return(crown_pc)
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
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return The projected crown area (numeric value) as the area of the concave
#'   hull computed from the crown points of a tree point cloud.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' pca <- projected_crown_area_pc(pc)
#' pca <- projected_crown_area_pc(pc,0.3,FALSE)
#' pca <- projected_crown_area_pc(pc,1,TRUE)
#' }
projected_crown_area_pc <- function(pc, concavity=2, plot=FALSE){
  crown_pc <- classify_crown_pc(pc,1.5,4,FALSE)
  points <- sf::st_as_sf(unique(crown_pc[1:2]), coords=c("X","Y"))
  hull <- concaveman::concaveman(points, concavity)
  pca <- sf::st_area(hull)
  if(plot){
    plot(sf::st_geometry(hull), col = "lightgrey")
  }
  return(pca)
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
#' @param plot Logical (default=FALSE), indicates if the optimised circle
#'   fitting is plotted.
#'
#' @return The volume of the tree crown (numeric value) as the volume of the 3D
#'   alpha-shape computed from the crown points of a tree point cloud.
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path,1)
#' vol_crown <- volume_crown_pc(pc)
#' vol_crown <- volume_crown_pc(pc,0.3,FALSE)
#' vol_crown <- volume_crown_pc(pc,1,TRUE)
#' }
volume_crown_pc <- function(pc, alpha=1, plot=FALSE){
  crown_pc <- classify_crown_pc(pc,1.5,4,FALSE)
  crown_xyz <- data.matrix(unique(crown_pc[1:3]))
  ashape3d.obj <- alphashape3d::ashape3d(crown_xyz, alpha = alpha)
  if(plot){
    graphics::par(pty="s")
    plot(ashape3d.obj)
  }
  vol_crown <- alphashape3d::volume_ashape3d(ashape3d.obj)
  return(vol_crown)
}



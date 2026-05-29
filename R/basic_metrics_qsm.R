#' QSM position
#'
#' Returns the (X,Y)-position of a treeQSM based on the start position of the
#' first cylinder that is higher than 1.3 m above ground.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   TRUE.
#'
#' @return Numeric with the XY coordinates (location) of the tree stem.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and calculate tree position
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' pos <- tree_position_qsm(cylinder = qsm$cylinder)
#' }
tree_position_qsm <- function(cylinder) {
  j <- 0
  while (sum(cylinder$length[1:(j + 1)]) < 1.3) {
    j <- j + 1
  }
  x_location <- cylinder$start[j + 1, 1]
  y_location <- cylinder$start[j + 1, 2]
  return(c(x_location, y_location))
}

#' Total cylinder length QSM
#'
#' Extracts the total cylinder length from the cylinder data of a TreeQSM or the
#' a RCT QSM.
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#'
#' @return The total length of all the cylinders (branch and trunk) of
#'   a TreeQSM or RCT QSM in meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract total cylinder length
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' tot_len <- total_cyl_length_qsm(treedata = qsm$treedata)
#' }
total_cyl_length_qsm <- function(treedata) {
    return(treedata$TotalLength[1])
}

#' Total tree volume QSM
#'
#' Extracts the total tree volume from the treedata of a TreeQSM or RCT QSM
#' (treeinfo needs to have been run with --branch_data to access the required
#' information).
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#' @param cylinder Cylinder field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE, (default = NA - you do not need cylinder when
#'   cylindercutoff == 0).
#' @param cylindercutoff This is the cutoff radius in meters for which cylinders
#'   are to be included in the volume calculation. Default of 0 includes all
#'   cylinders.
#'
#' @return The total volume of the TreeQSM or RCT QSM in liters. If the trunk
#'   was modeled with triangulation (option in TreeQSM v2.4.x) the total volume
#'   is the sum of the triangulated volume of the stem (bottom), the volume of
#'   the stem cylinder (top) and the volume of the branch cylinders.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract tree volume
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' tot_vol <- tree_volume_qsm(treedata = qsm$treedata)
#' # Only include cylinders larger than 2.5 cm in radius
#' tot_vol <- tree_volume_qsm(treedata = qsm$treedata, cylinder = qsm$cylinder,
#'                            cylindercutoff = 0.25)
#' }
tree_volume_qsm <-
  function(treedata,
           cylinder = NA,
           cylindercutoff = 0) {
    if (cylindercutoff > 0 & length(cylinder) > 1) {
      trunk_vol <- trunk_volume_qsm(treedata)
      cylinder_ind <- which(cylinder$BranchOrder > 0 &
                              cylinder$radius > cylindercutoff)
      if (is.null(cylinder$volume)){
        branch_vol <- sum((cylinder$radius[cylinder_ind]) ^ 2 * pi *
                          (cylinder$length[cylinder_ind])) * 1000
      } else {
        branch_vol <- sum(cylinder$volume[cylinder_ind]) * 1000
      }
      volume <- trunk_vol + branch_vol
    } else {
      if (length(treedata) > 83) {
        volume <- treedata$MixTotalVolume[1]
      } else {
        volume <- treedata$TotalVolume[1]
      }
    }
    return(volume)
  }

#' Total trunk volume QSM
#'
#' Extracts the total trunk volume from the treedata of a TreeQSM or a RCT QSM
#' (treeinfo needs to have been run with --branch_data to access the required
#' information).
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#'
#' @return The total trunk volume of the TreeQSM or RCT QSM in liters. If the
#' trunk was modelled with triangulation (option in TreeQSM v2.4.x) the total
#' volume is the sum of the triangulated volume of the stem (bottom) and the
#' volume of the stem cylinder (top).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract trunk volume
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' trunkvol <- trunk_volume_qsm(treedata = qsm$treedata)
#' }
trunk_volume_qsm <- function(treedata) {
  if (length(treedata) > 83) {
    volume <- treedata$MixTrunkVolume[1]
  } else {
    volume <- treedata$TrunkVolume[1]
  }
  return(volume)
}

#' Total branch volume QSM
#'
#' Extracts the total branch volume from the treedata of a TreeQSM or a RCT QSM
#' (treeinfo needs to have been run with --branch_data to access the required
#' information).
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#'
#' @return The total branch volume of the TreeQSM or RCT QSM in liters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract total branch volume
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' branchvol <- total_branch_volume_qsm(treedata = qsm$treedata)
#' }
total_branch_volume_qsm <- function(treedata) {
  return(treedata$BranchVolume[1])
}

#' Branch volume by order QSM
#'
#' Extracts the branch volumes by branching order from the cylinder data of a
#' TreeQSM or a RCT QSM (treeinfo needs to have been run with --branch_data to
#' access the required information).
#'
#' @param cylinder Cylinder field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#'
#' @return A data frame with the branching orders of the TreeQSM or RCT QSM and
#'   their total volumes in liters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract branch volumes by order
#' qsm <- read_rct_qsm(QSM_path = "path/to/qsm.mat")
#' branchvol_byorder <- branch_volume_byorder_qsm(cylinder = qsm$qsm1$cylinder)
#' }
branch_volume_byorder_qsm <- function(cylinder) {
  if (is.null(cylinder$volume)) {
    cylinder$volume <- cylinder$radius ^ 2 * pi * cylinder$length * 1000
  }
  cylinder <- as.data.frame(cylinder)
  result <- stats::aggregate(volume ~ BranchOrder, data = cylinder, sum)
  out <- as.data.frame(t(result$volume))
  colnames(out) <- paste0("branchorder_", result$BranchOrder)
  return(out)
}


#' Cylinder volume by radius size QSM
#'
#' Extracts the total cylinder volumes by radius size from the cylinder data of
#' a TreeQSM or a RCT QSM (treeinfo needs to have been run with --branch_data to
#' access the required information).
#'
#' @param cylinder Cylinder field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#' @param radius_bin_size Numeric. The radius_bin_size determines the size that
#'   the bins should be in metres. A value of 0.01 will divide the cylinders in
#'   1 cm bins and count up the volumes of all cylinders in that size bin. 1 cm
#'   is the default.
#'
#' @return A data frame with the radius ranges of the TreeQSM or RCT QSM and
#'   their total volumes in liters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract branch volumes by order
#' qsm <- read_rct_qsm(QSM_path = "path/to/qsm.mat")
#' cylvol_byradius <- cylinder_volume_byradius_qsm(cylinder = qsm$qsm1$cylinder)
#' }
cylinder_volume_byradius_qsm <- function(cylinder, radius_bin_size = 0.01) {
  if (is.null(cylinder$volume)) {
    cylinder$volume <- cylinder$radius ^ 2 * pi * cylinder$length * 1000
  }
  cylinder <- as.data.frame(cylinder)
  cylinder$radius_bin <- cut(
    cylinder$radius,
    breaks = seq(
      floor(min(cylinder$radius) / radius_bin_size) * radius_bin_size,
      ceiling(max(cylinder$radius) / radius_bin_size) * radius_bin_size,
      by = radius_bin_size
    ),
    include.lowest = TRUE
  )
  result <- stats::aggregate(volume ~ radius_bin, data = cylinder, sum)
  out <- as.data.frame(t(result$volume))
  colnames(out) <- paste0("radius_", gsub("\\]|\\(|\\[", "", result$radius_bin))
  return(out)
}


#' Total branch length QSM
#'
#' Extracts the total branch length from the treedata of a TreeQSM or RCT QSM
#' (treeinfo needs to have been run with --branch_data to access the required
#' information).
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}} with remove_root
#'   = TRUE.
#'
#' @return The total branch length of the TreeQSM or RCT QSM in meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract total branch length
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' branchlen <- total_branch_length_qsm(treedata = qsm$treedata)
#' }
total_branch_length_qsm <- function(treedata) {
  return(treedata$BranchLength[1])
}

#' Tree height QSM
#'
#' Extracts the tree height from the treedata of a TreeQSM or RCT QSM.
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}}.
#'
#' @return The tree height of the TreeQSM or RCT QSM in meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract tree height
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' height <- tree_height_qsm(treedata = qsm$treedata)
#' }
tree_height_qsm <- function(treedata) {
  return(treedata$TreeHeight[1])
}

#' Diameter at breast height QSM
#'
#' Extracts the DBH from the treedata of a TreeQSM or RCT QSM.
#'
#' The DBH is calculated as the diameter of the cylinder in the QSM at the right
#' height (cylinder at 1.3 m). If the trunk was modeled with triangulation the
#' DBH is calculated as mean length of the diagonals in the triangulation.
#'
#' @param treedata Treedata field of a TreeQSM or RCT QSM that is returned by
#'   \code{\link{read_tree_qsm}} or \code{\link{read_rct_qsm}}.
#'
#' @return The DBH of the TreeQSM or RCT QSM in meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract DBH
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' dbh <- dbh_qsm(treedata = qsm$treedata)
#' }
dbh_qsm <- function(treedata) {
  if (length(treedata) > 83) {
    dbh <- treedata$DBHtri[1]
  } else {
    dbh <- treedata$DBHqsm[1]
  }
  return(dbh)
}

#' Alpha crown area TreeQSM
#'
#' Extracts the alpha crown area from the treedata of a TreeQSM. This is
#' calculated during the TreeQSM process as the area (m$^2$) of the crown's
#' planar projection's alpha shape.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The alpha crown area of the TreeQSM in square meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract tree height
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' caa <- crown_area_alpha_qsm(treedata = qsm$treedata)
#' }
crown_area_alpha_qsm <- function(treedata) {
  return(treedata$CrownAreaAlpha)
}

#' Trunk length QSM
#'
#' Extracts the trunk length from the treedata of a TreeQSM or RCT QSM. For
#' stems without and with trangulation (option in TreeQSM v2.4.x) it selects the
#' TrunkLenght and TriaTrunkLength object from treedata respectively.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The trunk height of the TreeQSM in meters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read tree qsm and extract tree height
#' qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
#' trunk_height <- trunk_height_qsm(treedata = qsm$treedata)
#' }
trunk_length_qsm <- function(treedata) {
  if (length(treedata) > 83) {
    th <- treedata$TriaTrunkLength[1]
  } else {
    th <- treedata$TrunkLength[1]
  }
  return(th)
}

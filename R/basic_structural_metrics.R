#' Determine the position of a treeQSM
#'
#' @param cylinder Cylinder field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The XY coordinates (location) of the tree stem.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' pos <- tree_position_qsm(cylinder)
tree_position_qsm <- function(cylinder) {
  j <- 0
  while (sum(cylinder$length[1:(j+1)]) < 1.3)
  {
    j <- j+1
  }

  x_location <- cylinder$start[j+1,1]
  y_location <- cylinder$start[j+1,2]
  return(c(x_location,y_location))
}

#'Extract the total cylinder length from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The total length of all the cylinders (branch and trunk) in m of a TreeQSM.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' cyllen <- total_cyl_length_qsm(treedata)
total_cyl_length_qsm <- function(treedata) {
  return(treedata$TotalLength[1])
}

#' Extract the total tree volume from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The total volume in L of the TreeQSM. If the trunk was modelled with triangulation
#' the total volume is the sum of the triangulated volume of the stem (bottom),
#' the volume of the stem cylinder (top) and the volume of the branch cylinders.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' totvol <- tree_volume_qsm(treedata)
tree_volume_qsm <- function(treedata) {
  if (length(treedata) > 83){
    volume <- treedata$MixTotalVolume[1]
  } else {
    volume <- treedata$TotalVolume[1]
  }
  return(volume)
}

#' Extract the total trunk volume from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The total trunk volume in L of the TreeQSM. If the trunk was modelled with triangulation
#' the total volume is the sum of the triangulated volume of the stem (bottom) and
#' the volume of the stem cylinder (top).
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' trunkvol <- trunk_volume_qsm(treedata)
trunk_volume_qsm <- function(treedata) {
  if (length(treedata) > 83){
    volume <- treedata$MixTrunkVolume[1]
  } else {
    volume <- treedata$TrunkVolume[1]
  }
  return(volume)
}

#' Extract the total branch volume from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The total branch volume in L of the TreeQSM.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' branchvol <- total_branch_volume_qsm(treedata)
total_branch_volume_qsm  <- function(treedata) {
  return(treedata$BranchVolume[1])
}

#' Extract the tree height from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The tree height in m of the TreeQSM.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' height <- tree_height_qsm(treedata)
tree_height_qsm <- function(treedata) {
  return(treedata$TreeHeight[1])
}

#' Extract the DBH from the treedata of a TreeQSM
#'
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The DBH in m of the TreeQSM.
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' dbh <- DBH_qsm(treedata)
DBH_qsm <- function(treedata) {
  return(treedata$DBHqsm[1])
}

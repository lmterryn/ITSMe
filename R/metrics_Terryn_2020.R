#' Diameter at breast height/above buttresses
#'
#' Returns the diameter at breast height or diameter above buttresses.
#'
#' If the tree point cloud is available the calculations are based on the point
#' cloud (most accurate). In this case the diameter at breast height (dbh) or
#' diameter above buttresses (dab) is calculated with \code{\link{dbh_pc}} or
#' \code{\link{dab_pc}} respectively. If the tree point cloud is not available
#' the dbh is based on the treeQSM with \code{\link{dbh_qsm}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point cloud
#'   is available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available.
#'
#' @return The dbh or dab in meters.
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' DBH <- dbh(qsm)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' DBH <- dbh(qsm,pc,TRUE)
#' }
dbh <- function(treedata,pc=NA,buttress=FALSE){
  if(!is.data.frame(pc)){
    return(dbh_qsm(treedata))
  } else {
    if(buttress){
      return(dab_pc(pc))
    } else {
      return(dbh_pc(pc))
    }
  }
}

#' Tree height
#'
#' Returns the tree height.
#'
#' If the tree point cloud is available the tree_height calculation is based on
#' the point cloud (most accurate) with \code{\link{tree_height_pc}}. If the
#' tree point cloud is not available the tree height is based on the treeQSM
#' with \code{\link{tree_height_qsm}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. If the point cloud is not available NA is used
#'   as input (default=NA).
#'
#' @return The tree height in meters.
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' h <- tree_height(qsm)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' h <- tree_height(qsm,pc)
#' }
tree_height <- function(treedata,pc=NA){
  if(!is.data.frame(pc)){
    return(tree_height_qsm(treedata))
  } else {
    return(tree_height_pc(pc))
  }
}

#' Stem branch angle TreeQSM
#'
#' Calculates the stem branch angle from a TreeQSM.
#'
#' The stem branch angle is defined as "the median of the branching angles of
#' the 1st order branches in degrees. 0 is upwards and 180 downwards (parallel
#' with the trunk)" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @param branch Branch field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The stem branch angle in degrees. NaN when there are no stem
#'   branches.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sba <- stem_branch_angle_qsm(qsm$branch)
#' }
stem_branch_angle_qsm <- function(branch) {
  indices_stem_branches <- which(branch$order == 1)
  if (length(indices_stem_branches) > 0){
    angle_stem_branches <- branch$angle[indices_stem_branches]
    sba <- stats::median(angle_stem_branches)
  } else {
    sba <- NaN
  }
  return(sba)
}

#' Stem branch cluster size TreeQSM
#'
#' Calculates the stem branch cluster size from a TreeQSM.
#'
#' The stem branch cluster size is defined as "the average number of 1st order
#' branches inside a 40cm height interval for 1st order branches. Each branch
#' can only belong to one interval" (Akerblom et al., 2017 & Terryn et al.,
#' 2020).
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The stem branch cluster size. NaN when there are no stem branches.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
#' }
stem_branch_cluster_size_qsm <- function(cylinder) {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1
                                  & cylinder$BranchOrder == 1)
  if (length(indices_stem_cylinders) > 0){
    cylinder_heights <- cylinder$start[indices_stem_cylinders,3]
    cylinder_heights <- cylinder_heights[order(cylinder_heights)]
    start_heights <- cylinder_heights - 0.2
    end_heights <- cylinder_heights + 0.2
    label <- integer(length(cylinder_heights))
    cluster <- c()
    for(i in 1:length(cylinder_heights)) {
      indices_in_interval <- which(cylinder_heights > start_heights[i]
                                   & cylinder_heights < end_heights[i])
      number_clusters <- 0
      for(j in 1:length(indices_in_interval)) {
        if (label[indices_in_interval[j]] == 1) {
          #do nothing
        } else if(label[indices_in_interval[j]] == 0) {
          label[indices_in_interval[j]] <- 1
          number_clusters <- number_clusters + 1
        }
      }
      if(length(indices_in_interval) > 0 & number_clusters>0){
        cluster <- append(cluster,number_clusters)
      }
    }
    sbcs <- mean(cluster)
  } else {
    sbcs <- NaN
  }
  return(sbcs)
}

#' Stem branch radius TreeQSM
#'
#' Calculates the stem branch radius from a TreeQSM.
#'
#' The stem branch radius is defined as "Mean of the 10 largest 1st order
#' branches measured at the base. Can be normalised by the tree height or the
#' the stem radius at respective height" (Akerblom et al., 2017 & Terryn et al.,
#' 2020). Tree height is calculated with \code{\link{tree_height}}.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param normalisation Can be either "treeheight" or "parentcylinder". In case
#'   of "treeheight" the mean radius of the 10 biggest branches is divided by
#'   the tree height (Terryn et al., 2020). In case of "parentcylinder" the mean
#'   is taken of the ratios of the radius of the 10 biggest branches and the
#'   radius of their parent cylinders (Akerblom et al., 2017). When something
#'   different than "treeheight" or "parentcylinder" is given, no normalisation
#'   is done. Default is no normalisation.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point cloud
#'   is available. Only relevant if normalisation equals "treeheight".
#'
#' @return The stem branch radius. Unitless with normalisation, in meters
#'   without normalisation.  NaN when there are no stem branches.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbr <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata, "parentcyl")
#' sbr <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' sbr <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata,"treeheight",pc)
#' }
stem_branch_radius_qsm <- function(cylinder, treedata,
                                   normalisation="treeheight", pc=NA) {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1
                                  & cylinder$BranchOrder == 1)
  if(length(indices_stem_cylinders) > 0) {
    branch_radius <- cylinder$radius[indices_stem_cylinders]
    branch_indices_sorted <- indices_stem_cylinders[order(branch_radius,
                                                          decreasing = TRUE)]
    branch_radius_sorted <- branch_radius[order(branch_radius,
                                                decreasing = TRUE)]
    if(length(branch_indices_sorted) < 10){
      indices_biggest_ten <- branch_indices_sorted
      radius_biggest_ten <- branch_radius_sorted
    } else {
      indices_biggest_ten <- branch_indices_sorted[1:10]
      radius_biggest_ten <- branch_radius_sorted[1:10]
    }
    indices_parents_bt <- cylinder$parent[indices_biggest_ten]
    radius_parents_bt <- cylinder$radius[indices_parents_bt]
    if(normalisation == "treeheight") {
      tree_height <- tree_height(treedata,pc)
      sbr <- mean(radius_biggest_ten)/tree_height
    } else if (normalisation == "parentcylinder") {
      sbr <- mean(radius_biggest_ten/radius_parents_bt)
    } else {
      print("No normalisation")
      sbr <- mean(radius_biggest_ten)
    }
  } else {
    sbr <- NaN
  }
  return(sbr)
}

#' Stem branch length TreeQSM
#'
#' Calculates the stem branch length from a TreeQSM.
#'
#' The stem branch length is defined as "the average length of 1st order
#' branches. Can be normalised by DBH or tree height" (Akerblom et al., 2017 &
#' Terryn et al., 2020). DBH and tree height are calculated with
#' \code{\link{dbh}} and \code{\link{tree_height}}.
#'
#' @param branch Branch field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param normalisation Can be either "dbh" or "treeheight". In case of "dbh"
#'   the mean of the lengths of the stem branches are divided by the DBH
#'   (Akerblom et al., 2017). In case of "treeheight" the mean is divided by the
#'   tree height (Terryn et al., 2020). When something different than "dbh" or
#'   "treeheight" is given, no normalisation is done. Default is no
#'   normalisation.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available. Only relevant if normalisation equals "dbh" or
#'   "treeheight".
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available and normalisation equals
#'   "dbh".
#'
#' @return The stem branch length. Unitless with normalisation, in meters
#'   without normalisation. NaN when there are no stem branches.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbl <- stem_branch_length_qsm(qsm$branch, qsm$treedata)
#' sbl <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "treeheight")
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' sbl <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "dbh", pc, TRUE)
#' }
stem_branch_length_qsm <- function(branch, treedata, normalisation="treeheight",
                                   pc=NA, buttress=FALSE) {
  indices_stem_branches <- which(branch$order == 1)
  if (length(indices_stem_branches) > 0) {
    branch_lengths=branch$length[indices_stem_branches]
    if (normalisation == "dbh") {
      dbh <- dbh(treedata,pc,buttress)
      sbl <- mean(branch_lengths)/dbh
    } else if (normalisation == "treeheight") {
      tree_height <- tree_height(treedata,pc)
      sbl <- mean(branch_lengths)/tree_height
    } else {
      print("No normalisation")
      sbl <- mean(branch_lengths)
    }
  } else {
    sbl <- NaN
  }
  return(sbl)
}

#' Stem branch distance TreeQSM
#'
#' Calculates the stem branch distance from a TreeQSM.
#'
#' The stem branch distance is defined as "Average distance between 1st order
#' branches computed using a moving average with a window width 1 m. If window
#' is empty average distance in window is set as half of window width. Can be
#' normalised by the DBH" (Akerblom et al., 2017 & Terryn et al., 2020). When
#' something different than "dbh" is given, no normalisation is done. Default is
#' no normalisation. DBH is calculated with \code{\link{dbh}}.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param normalisation Can either be "dbh" or nothing. In case of "dbh" the
#'   average distance is divided by the DBH (Akerblom et al., 2017).
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available. Only relevant if normalisation equals "dbh".
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available and normalisation equals
#'   "dbh".
#'
#' @return The stem branch distance. Unitless with normalisation, in meters
#'   without normalisation. NaN when there are no stem branches.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbd <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata)
#' sbd <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata, "dbh")
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' sbd <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata, "dbh", pc, TRUE)
#' }
stem_branch_distance_qsm <- function(cylinder, treedata, normalisation="no",
                                     pc=NA, buttress=FALSE) {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1
                                  & cylinder$BranchOrder == 1)
  cylinder_heights <- cylinder$start[indices_stem_cylinders,3]
  cylinder_heights <- cylinder_heights[order(cylinder_heights)]
  if(length(indices_stem_cylinders) > 0) {
    upper_heights <- cylinder_heights[2:length(cylinder_heights)]
    lower_heights <- cylinder_heights[1:length(cylinder_heights)-1]
    height_difference <- upper_heights - lower_heights
    start_heights <- cylinder_heights - 0.5
    end_heights <- cylinder_heights + 0.5
    average_distance <- c()
    for(i in 1:length(cylinder_heights)){
      indices_in_interval <- which(cylinder_heights > start_heights[i]
                                   & cylinder_heights < end_heights[i])
      if(length(indices_in_interval) == 1) {
        average_distance <- append(average_distance,0.5)
      } else {
        distance <- c()
        for(j in 1:length(indices_in_interval)-1) {
          distance <- append(distance,
                             height_difference[indices_in_interval[j]])
        }
        average_distance <- append(average_distance,mean(distance))
      }
    }
    if(normalisation == "dbh") {
      dbh <- dbh(treedata,pc,buttress)
      sbd <- mean(average_distance)/dbh
    } else {
      #print("No normalisation")
      sbd <- mean(average_distance)
    }
  } else {
    sbd <- NaN
  }
  return(sbd)
}

#' DBH-tree height ratio TreeQSM
#'
#' Calculates DBH-tree height ratio from a TreeQSM.
#'
#' DBH and tree height are calculated with \code{\link{dbh}} and
#' \code{\link{tree_height}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available.
#'
#' @return DBH divided by the tree height.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ratio_height <- dbh_height_ratio_qsm(qsm$treedata)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' ratio_height <- dbh_height_ratio_qsm(qsm$treedata, pc)
#' ratio_height <- dbh_height_ratio_qsm(qsm$treedata, pc, TRUE)
#' }
dbh_height_ratio_qsm <- function(treedata,pc=NA,buttress=FALSE){
  dbh <- dbh(treedata,pc,buttress)
  tree_height <- tree_height(treedata,pc)
  return(dbh/tree_height)
}

#' DBH-tree volume ratio TreeQSM
#'
#' Calculates DBH-tree volume ratio from a TreeQSM.
#'
#' DBH and tree volume are calculated with \code{\link{dbh}} and
#' \code{\link{tree_volume_qsm}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available.
#'
#' @return DBH divided by the tree volume (trunk plus branches) in meters-2.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ratio_vol <- dbh_volume_ratio_qsm(qsm$treedata)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' ratio_vol <- dbh_volume_ratio_qsm(qsm$treedata,pc)
#' ratio_vol <- dbh_volume_ratio_qsm(qsm$treedata,pc,TRUE)
#' }
dbh_volume_ratio_qsm <- function(treedata,pc=NA,buttress=FALSE){
  dbh <- dbh(treedata,pc,buttress)
  volume <- tree_volume_qsm(treedata)
  return(dbh/volume)
}

#' Volume below 55 TreeQSM
#'
#' Calculates the volume below 55% from a TreeQSM.
#'
#' The volume below 55 is defined as "the relative branch volume below 55% of
#' tree height" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The volume below 55.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' vol_55 <- volume_below_55_qsm(qsm$cylinder, qsm$treedata)
#' }
volume_below_55_qsm <- function(cylinder, treedata){
  tree_height <- tree_height_qsm(treedata)
  volume_branches <- total_branch_volume_qsm(treedata)
  height_at_55 <- tree_height*0.55+min(cylinder$start[,3])
  indices_branch_cylinders_under_55 <- which(cylinder$start[,3]
                                             < height_at_55
                                             & cylinder$BranchOrder != 0)
  volumes_branch_cylinders_under_55 <- cylinder$length[
    indices_branch_cylinders_under_55]*
    pi%*%cylinder$radius[indices_branch_cylinders_under_55]**2
  vb55 <- sum(volumes_branch_cylinders_under_55)/volume_branches
  return(vb55)
}

#' Cylinder length-volume ratio TreeQSM
#'
#' Calculates the cylinder length volume ratio from a TreeQSM.
#'
#' The cylinder length volume ratio is defined as "the ratio between total
#' length and volume of the branch cylinders" (Akerblom et al., 2017 & Terryn et
#' al., 2020).
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The cylinder length volume ratio in meters-2.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' len_vol_ratio <- cylinder_length_volume_ratio_qsm(qsm$treedata)
#' }
cylinder_length_volume_ratio_qsm <- function(treedata){
  total_branch_volume <- total_branch_volume_qsm(treedata)
  total_branch_length <- total_branch_length_qsm(treedata)
  return(total_branch_length/total_branch_volume)
}

#' Shedding ratio TreeQSM
#'
#' Calculates the shedding ratio from a TreeQSM.
#'
#' The shedding ratio is defined as "The number of stem branches without
#' children divided by the number of all branches in the bottom third" (Akerblom
#' et al., 2017 & Terryn et al., 2020).
#'
#' @param branch Branch field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The shedding ratio.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' shed_ratio <- shedding_ratio_qsm(qsm$branch, qsm$treedata)
#' }
shedding_ratio_qsm <- function(branch,treedata){
  indices_stem_branches <- which(branch$order == 1)
  tree_height <- treedata$TreeHeight[1]
  if(length(indices_stem_branches) > 0){
    indices_stem_branches_under_third <- which(branch$height < tree_height/3
                                               & branch$order == 1)
    if (rapportools::is.empty(indices_stem_branches_under_third)){
      sr <- 0
    } else {
      number_stem_branches_under_third <- length(
        indices_stem_branches_under_third)
      number_without_children <- 0
      for(i in 1:number_stem_branches_under_third){
        number_children <- sum(branch$parent ==
                                 indices_stem_branches_under_third[i])
        if(number_children == 0){
          number_without_children <- number_without_children + 1
        }
      }
      if(number_without_children > 0){
        sr <- number_without_children/number_stem_branches_under_third
      } else {
        sr <- 0
      }
    }
  } else {
    sr <- NaN
  }
  return(sr)
}

#' Branch angle ratio TreeQSM
#'
#' Calculates the branch angle ratio from a TreeQSM.
#'
#' The branch angle ratio is defined as "Ratio of the medians of the branching
#' angles of the 1st order branches and 2nd order branches" (Terryn et al.,
#' 2020).
#'
#' @param branch Branch field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The branch angle ratio
#'
#' @references Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y.,
#'   Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using
#'   structural features derived from terrestrial laser scanning. ISPRS Journal
#'   of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ba_ratio <- branch_angle_ratio_qsm(qsm$branch)}
branch_angle_ratio_qsm <- function(branch){
  indices_first_order_branches <- which(branch$order == 1)
  indices_second_order_branches <- which(branch$order == 2)
  angle_first_order_branches <- branch$angle[indices_first_order_branches]
  angle_second_order_branches <- branch$angle[indices_second_order_branches]
  return(stats::median(angle_first_order_branches)/
           stats::median(angle_second_order_branches))
}

#' Relative volume ratio TreeQSM
#'
#' Calculates the relative volume ratio from a TreeQSM.
#'
#' The relative volume ratio is defined as "Ratio of the percentage volume
#' within 80 to 90% of the tree height and the percentage volume within 0 to 10%
#' of the tree height" (Terryn et al., 2020).
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The relative volume ratio.
#'
#' @references Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y.,
#'   Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using
#'   structural features derived from terrestrial laser scanning. ISPRS Journal
#'   of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' relvol_ratio <- relative_volume_ratio_qsm(qsm$cylinder, qsm$treedata)
#' }
relative_volume_ratio_qsm <- function(cylinder,treedata){
  tree_height <- treedata$TreeHeight[1]
  volume <- treedata$TotalVolume[1]
  number <- 10
  interval <- tree_height/number
  cylinder_z_coordinates <- cylinder$start[,3]
  minimum_z_coordinate <- min(cylinder_z_coordinates)
  volume_distribution <- integer(1)
  for(i in 0:number){
    interval_start <- minimum_z_coordinate + i*interval
    interval_end <- interval_start + interval
    indices_cylinders_in_interval <- which(cylinder_z_coordinates >=
                                             interval_start
                                           & cylinder_z_coordinates <
                                             interval_end)
    volume_cylinders_in_interval <- sum(cylinder$length
                                        [indices_cylinders_in_interval]*
                                          pi%*%cylinder$radius
                                        [indices_cylinders_in_interval]**2)
    volume_distribution <- append(volume_distribution,
                                  volume_cylinders_in_interval)
  }
  relative_volume_distribution <- volume_distribution/volume
  return(relative_volume_distribution[10]/relative_volume_distribution[2])
}

#' Crownset TreeQSM
#'
#' Returns the indices of the cylinders belonging to the crown of the treeQSM.
#'
#' The crownset is determined based on four steps (that are designed to exclude
#' dead branches at the bottom of the stem) according to Akerblom et al. (2017).
#' STEP 1: Initialize the crown set as cylinders that have a branching order
#' higher than three. If the initial set is empty, the minimum order is lowered
#' until the set becomes non-empty. STEP 2: As long as the crown set extends,
#' append the parent cylinders of the crown set that are not part of the stem.
#' STEP 3: Append to the crown set cylinders that are not part of the stem but
#' whose start point is higher than the lowest starting point of crown cylinders
#' connected to the stem. STEP 4: As long as the crown set extends, append the
#' child cylinders of the crown set.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return An integer containing the indices of the cylinders belonging to the
#'   crownset.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' crown <- crownset_qsm(qsm$cylinder)
#' }
crownset_qsm <- function(cylinder){
  children <- c()
  for(i in 1:length(cylinder$parent)){
    c <- list(which(cylinder$parent == i))
    children <- append(children,c)
  }
  #STEP 1
  order <- 3
  crownset <- which(cylinder$BranchOrder >= order)
  while(length(crownset)==0 & order>1){
    order <- order - 1
    crownset <- which(cylinder$BranchOrder == order)
  }
  if(length(crownset)>0){
    #STEp 2
    parents <- unique(cylinder$parent[crownset])
    parents <- parents[cylinder$BranchOrder[parents]>0]
    while(length(parents)>0){
      crownset <- unique(append(crownset,parents))
      newparents <- unique(cylinder$parent[parents])
      newparents <- newparents[cylinder$BranchOrder[newparents]>0]
      parents <- newparents
    }
    #STEP 3
    crownset_parent0 <- crownset[cylinder$BranchOrder
                                 [cylinder$parent[crownset]]==0]
    min_height <- min(cylinder$start[crownset_parent0,3])
    crownset <- unique(append(crownset,which(cylinder$start[,3]>min_height
                                             & cylinder$BranchOrder>0)))
    #STEP 4
    init_length <- 0
    while (length(crownset)>init_length){
      init_length <- length(crownset)
      for (i in 1:length(children[crownset])){
        crownset <- unique(append(crownset,children[crownset][[i]]))
      }
    }
  }
  return(crownset)
}

#' Crown start height TreeQSM
#'
#' Calculates the crown start height from a TreeQSM.
#'
#' The crown start height is defined as "The height of the first stem branch in
#' the tree crown relative to the tree height" (Akerblom et al., 2017 & Terryn
#' et al., 2020). The tree height is calculated with \code{\link{tree_height}}.
#' Crown cylinders are determined with \code{\link{crownset_qsm}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#'
#' @return The crown start height.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' csh <- crown_start_height_qsm(qsm$treedata,qsm$cylinder)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' csh <- crown_start_height_qsm(qsm$treedata,qsm$cylinder,pc)
#' }
crown_start_height_qsm <- function(treedata,cylinder,pc=NA){
  crownset <- crownset_qsm(cylinder)
  tree_height <- tree_height(treedata,pc)
  crownset_parent0 <- crownset[cylinder$BranchOrder
                               [cylinder$parent[crownset]]==0]
  min_height <- min(cylinder$start[crownset_parent0,3])
  if(length(crownset)>0){
    sh <- (min_height-min(cylinder$start[,3]))/tree_height
  } else {
    sh <- NaN
  }
  return(sh)
}

#' Crown height TreeQSM
#'
#' Calculates the crown height from a TreeQSM.
#'
#' The crown height is defined as "the vertical distance between the highest and
#' the lowest crown cylinder relative to the tree height" (Akerblom et al., 2017
#' & Terryn et al., 2020). The tree height is calculated with
#' \code{\link{tree_height}}. Crown cylinders are determined with
#' \code{\link{crownset_qsm}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#'
#' @return The crown height.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ch <- crown_height_qsm(qsm$treedata,qsm$cylinder)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' ch <- crown_height_qsm(qsm$treedata,qsm$cylinder,pc)
#' }
crown_height_qsm <- function(treedata,cylinder,pc=NA){
  crownset <- crownset_qsm(cylinder)
  tree_height <- tree_height(treedata,pc)
  if(length(crownset)>0){
    minz_crown <- min(cylinder$start[crownset,3])
    maxz_crown <- max(cylinder$start[crownset,3])
    x <- which(cylinder$start[,3]==maxz_crown)
    maxz_crown <- maxz_crown+cylinder$length[x]*cylinder$axis[x,3]
    ch <- (maxz_crown-minz_crown)/tree_height
  } else {
    ch <- NaN
  }
  return(ch)
}

#' Crown evenness TreeQSM
#'
#' Calculates the crown evenness from a TreeQSM.
#'
#' The crown evenness is defined as "The crown cylinders divided into 8 angular
#' bins. Ratio between the minimum heights of the highest and lowest bin."
#' (Akerblom et al., 2017 & Terryn et al., 2020). Crown cylinders are determined
#' with \code{\link{crownset_qsm}}.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The crown evenness.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ce <- crown_evenness_qsm(qsm$cylinder,crown)
#' }
crown_evenness_qsm <- function(cylinder){
  crownset <- crownset_qsm(cylinder)
  if(length(crownset)>0){
    bins <- c(0*2*pi/8-pi,1*2*pi/8-pi,2*2*pi/8-pi,3*2*pi/8-pi,4*2*pi/8-pi,
              5*2*pi/8-pi,6*2*pi/8-pi,7*2*pi/8-pi,2*pi/1-pi)
    crownset_bo1 <- crownset[cylinder$BranchOrder[crownset]==1]
    crownset_bo1_po0 <- crownset_bo1[cylinder$BranchOrder[cylinder$parent[
      crownset_bo1]]==0]
    center_z <- min(cylinder$start[crownset_bo1_po0,3])
    center <- crownset_bo1_po0[cylinder$start[crownset_bo1_po0,3] == center_z]
    center_x <- cylinder$start[center,1]
    center_y <- cylinder$start[center,2]
    R <- sqrt(((cylinder$start[crownset,1]-center_x)^2+
                 (cylinder$start[crownset,2]-center_y)^2))
    theta <- atan2((cylinder$start[crownset,2]-center_y),
                   (cylinder$start[crownset,1]-center_x))
    minimums <- c()
    Indices_Bin <- c()
    for (i in 2:length(bins)){
      indices_bin <- (theta < bins[i] & theta >= bins[(i-1)])
      Indices_Bin <- append(Indices_Bin,list(indices_bin))
      if(sum(indices_bin)>0){
        minimums <- append(minimums,
                           min(cylinder$start[crownset[indices_bin],3]))
      }
    }
    if(length(minimums)==length(bins)){
      ce <- (min(minimums)-min(cylinder$start[,3]))/
        (max(minimums)-min(cylinder$start[,3]))
    } else {
      ce <- 0
    }
  }
  for (i in 1:length(Indices_Bin)){
    b <- cylinder$start[crownset[Indices_Bin[[i]]],]
    if(!is.null(nrow(b)) & nrow(b)>0){
      plot(b[,1],b[,2],col=colors()[i+10])
    } else if (length(b)>1) {
      plot(b[1],b[2],col=colors()[i+10])
    }
  }
  return(ce)
}

#' Radii vertical bins TreeQSM
#'
#' Calculates the radii of the three vertical bins fitted to the TreeQSM
#' cylinders.
#'
#' These radii are the radii of the cylinders whose axis are vertical and goes
#' through the bin centre point, and which contains approximately 90% of the
#' volume of the branch cylinders in that bin. The tree is divided into three
#' vertical bins, and the centre point of each bin is defined as the average of
#' mean points of stem cylinders in the bin. If the bin does not contain stem
#' cylinders the centre of the previous bin is used (Akerblom et al., 2017 &
#' Terryn et al., 2020).
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return The radii of the three vertical bins.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' radii <- vertical_bin_radii_qsm(qsm$treedata,qsm$cylinder)
#' }
vertical_bin_radii_qsm <- function(treedata,cylinder){
  dbh <- dbh_qsm(treedata)
  sx <- cylinder$start[,1]
  sy <- cylinder$start[,2]
  sz <- cylinder$start[,3]
  cx <- cylinder$length*cylinder$axis[,1]/2+cylinder$start[,1]
  cy <- cylinder$length*cylinder$axis[,2]/2+cylinder$start[,2]
  cz <- cylinder$length*cylinder$axis[,3]/2+cylinder$start[,3]
  ex <- cylinder$length*cylinder$axis[,1]+cylinder$start[,1]
  ey <- cylinder$length*cylinder$axis[,2]+cylinder$start[,2]
  ez <- cylinder$length*cylinder$axis[,3]+cylinder$start[,3]
  vol <- cylinder$length*cylinder$radius^2*pi
  height <- max(ez)-min(sz)
  bins <- c()
  for (i in 1:length(ez)){
    if (ez[i] <= min(sz)+height/3){
      bins <- append(bins,1)
    } else if(ez[i] <= min(sz)+height*2/3) {
      bins <- append(bins,2)
    } else {
      bins <- append(bins,3)
    }
  }
  bin1 <- which(bins==1)
  bin2 <- which(bins==2)
  bin3 <- which(bins==3)
  stem <- which(cylinder$BranchOrder==0)
  bin1_cx <- mean(cx[stem[stem %in% bin1]])
  bin1_cy <- mean(cy[stem[stem %in% bin1]])
  if(sum(stem %in% bin2)>0){
    bin2_cx <- mean(cx[stem[stem %in% bin2]])
    bin2_cy <- mean(cy[stem[stem %in% bin2]])
    if(sum(stem %in% bin3)>0){
      bin3_cx <- mean(cx[stem[stem %in% bin3]])
      bin3_cy <- mean(cy[stem[stem %in% bin3]])
    } else {
      bin3_cx <- bin2_cx
      bin3_cy <- bin2_cy
    }
  } else {
    bin2_cx <- bin3_cx <-bin1_cx
    bin2_cy <- bin3_cy <-bin1_cy
  }
  de_bin1 <- sqrt((ex[bin1]-bin1_cx)^2+(ey[bin1]-bin1_cy)^2)
  de_bin2 <- sqrt((ex[bin2]-bin2_cx)^2+(ey[bin2]-bin2_cy)^2)
  de_bin3 <- sqrt((ex[bin3]-bin3_cx)^2+(ey[bin3]-bin3_cy)^2)
  ds_bin1 <- sqrt((sx[bin1]-bin1_cx)^2+(sy[bin1]-bin1_cy)^2)
  ds_bin2 <- sqrt((sx[bin2]-bin2_cx)^2+(sy[bin2]-bin2_cy)^2)
  ds_bin3 <- sqrt((sx[bin3]-bin3_cx)^2+(sy[bin3]-bin3_cy)^2)
  if(sum(!(bin1 %in% stem))==0){
    if(max(de_bin1)<=dbh/2){
      r1 <- dbh
    } else {
      r1 <- max(de_bin1)
    }
  } else {
    bin1_branch <- bin1[!(bin1 %in% stem)]
    de_bin1_branch <- de_bin1[which(!(bin1 %in% stem))]
    ds_bin1_branch <- ds_bin1[which(!(bin1 %in% stem))]
    d_bin1_branch <- de_bin1_branch-ds_bin1_branch
    p <- max(de_bin1_branch)
    ratio <- 1
    while(ratio > 0.9){
      p <- p-0.01
      v1 <- sum(vol[bin1_branch[de_bin1_branch <= p]])
      v2 <- sum((p-ds_bin1_branch[de_bin1_branch > p & ds_bin1_branch <= p])/
                  (d_bin1_branch[de_bin1_branch > p & ds_bin1_branch <= p])*
                  vol[bin1_branch[de_bin1_branch > p & ds_bin1_branch <= p]])
      vol_p <- v1+v2
      ratio <- vol_p/sum(vol[bin1_branch])
    }
    r1 <- p+0.005
  }
  if(sum(!(bin2 %in% stem))==0){
    if(max(de_bin2)<=max(cylinder$radius[bin2])){
      r2 <- max(cylinder$radius[bin2])
    } else {
      r2 <- max(de_bin2)
    }
  } else {
    bin2_branch <- bin2[!(bin2 %in% stem)]
    de_bin2_branch <- de_bin2[which(!(bin2 %in% stem))]
    ds_bin2_branch <- ds_bin2[which(!(bin2 %in% stem))]
    d_bin2_branch <- de_bin2_branch-ds_bin2_branch
    p <- max(de_bin2_branch)
    ratio <- 1
    while(ratio > 0.9){
      p <- p-0.01
      v1 <- sum(vol[bin2_branch[de_bin2_branch <= p]])
      v2 <- sum((p-ds_bin2_branch[de_bin2_branch > p & ds_bin2_branch <= p])/
                  (d_bin2_branch[de_bin2_branch > p & ds_bin2_branch <= p])*
                  vol[bin2_branch[de_bin2_branch > p & ds_bin2_branch <= p]])
      vol_p <- v1+v2
      ratio <- vol_p/sum(vol[bin2_branch])
    }
    r2 <- p+0.005
  }
  if(sum(!(bin3 %in% stem))==0){
    if(max(de_bin3)<=max(cylinder$radius[bin3])){
      r3 <- max(cylinder$radius[bin3])
    } else {
      r3 <- max(de_bin3)
    }
  } else {
    bin3_branch <- bin3[!(bin3 %in% stem)]
    de_bin3_branch <- de_bin3[which(!(bin3 %in% stem))]
    ds_bin3_branch <- ds_bin3[which(!(bin3 %in% stem))]
    d_bin3_branch <- de_bin3_branch-ds_bin3_branch
    p <- max(de_bin3_branch)
    ratio <- 1
    while(ratio > 0.9){
      p <- p-0.01
      v1 <- sum(vol[bin3_branch[de_bin3_branch <= p]])
      v2 <- sum((p-ds_bin3_branch[de_bin3_branch > p & ds_bin3_branch <= p])/
                  (d_bin3_branch[de_bin3_branch > p & ds_bin3_branch <= p])*
                  vol[bin3_branch[de_bin3_branch > p & ds_bin3_branch <= p]])
      vol_p <- v1+v2
      ratio <- vol_p/sum(vol[bin3_branch])
    }
    r3 <- p+0.005
  }
  return(c(r1,r2,r3))
}

#' Crown diameter height ratio TreeQSM
#'
#' Calculates the ratio between the crown diameter and the crown height from a
#' TreeQSM (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' The crown diameter is the maximum radii of the vertical bin radius estimates
#' that are calculated with \code{\link{vertical_bin_radii_qsm}}. The crown
#' height is the vertical distance between the highest and the lowest crown
#' cylinder and is obtained from \code{\link{crown_height_qsm}} multiplied with
#' the tree_height.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#'
#' @return The ratio of the crown diameter and crown height.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' cdh_ratio <- crown_diameterheight_ratio_qsm(qsm$treedata,qsm$cylinder,crown)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' cdh_ratio <- crown_diameterheight_ratio_qsm(qsm$treedata,qsm$cylinder,crown,
#'              pc)
#' }
crown_diameterheight_ratio_qsm <- function(treedata,cylinder,pc=NA){
  radii <- vertical_bin_radii_qsm(treedata,cylinder)
  diameter <- max(radii)*2
  ch  <- crown_height_qsm(treedata,cylinder)
  tree_height <- tree_height(treedata,pc)
  height <- ch*tree_height
  return(diameter/height)
}

#' DBH minimum tree radius ratio TreeQSM
#'
#' Calculates the ratio between the dbh and the minimum tree radius from a
#' TreeQSM.
#'
#' This ratio is defined as "Ratio between the DBH and the minimum of the
#' vertical bin radius estimates" (Akerblom et al., 2017 & Terryn et al., 2020).
#' The vertical bin radius estimates are calculated with
#' \code{\link{vertical_bin_radii_qsm}}. DBH is calculated with
#' \code{\link{dbh}}.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param pc The tree point cloud as a data.frame with columns X,Y,Z. Output of
#'   \code{\link{read_tree_pc}}. Default is NA and indicates no tree point
#'   cloud is available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if pc is available.
#'
#' @return The ratio of the dbh and the minimum tree radius.
#'
#' @references Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M.
#'   (2017). Automatic tree species recognition with quantitative structure
#'   models. Remote Sensing of Environment, 191, 1-12.
#'
#'   Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ...
#'   & Verbeeck, H. (2020). Tree species classification using structural
#'   features derived from terrestrial laser scanning. ISPRS Journal of
#'   Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' QSM_path <- "path/to/qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' dbh_rad_ratio <- dbh_minradius_ratio_qsm(qsm$treedata,qsm$cylinder)
#' PC_path <- "path/to/point_cloud.txt"
#' pc <- read_tree_pc(PC_path)
#' dbh_rad_ratio <- dbh_minradius_ratio_qsm(qsm$treedata,qsm$cylinder,pc)
#' dbh_rad_ratio <- dbh_minradius_ratio_qsm(qsm$treedata,qsm$cylinder,pc,TRUE)
#' }
dbh_minradius_ratio_qsm <- function(treedata,cylinder,pc=NA,buttress=FALSE){
  radii <- vertical_bin_radii_qsm(treedata,cylinder)
  diameter <- min(radii)*2
  dbh <- dbh(treedata,pc,buttress)
  return(dbh/diameter)
}


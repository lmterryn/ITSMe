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
#' @return The stem branch angle.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sba <- stem_branch_angle_qsm(qsm$branch)
stem_branch_angle_qsm <- function(branch) {
  indices_stem_branches <- which(branch$order == 1)
  angle_stem_branches <- branch$angle[indices_stem_branches]
  return(stats::median(angle_stem_branches))
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
#' @return The stem branch cluster size.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
stem_branch_cluster_size_qsm <- function(cylinder) {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1
                                  & cylinder$BranchOrder == 1)
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
  return(mean(cluster))
}

#' Stem branch radius TreeQSM
#'
#' Calculates the stem branch radius from a TreeQSM.
#'
#' The stem branch radius is defined as "Mean of the 10 largest 1st order
#' branches measured at the base. Can be normalised by the tree height or the
#' the stem radius at respective height" (Akerblom et al., 2017 & Terryn et al.,
#' 2020).
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
#'
#' @return The stem branch radius.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbr_th <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata, "treeheight")
#' sbr_nn <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata, "no")
stem_branch_radius_qsm <- function(cylinder, treedata, normalisation="no") {
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
    tree_height <- treedata$TreeHeight[1]
    if(normalisation == "treeheight") {
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
#' Terryn et al., 2020).
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
#'
#' @return The stem branch length.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbl_dbh <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "dbh")
#' sbl_th <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "treeheight")
#' sbl_nn <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "no")
stem_branch_length_qsm <- function(branch, treedata, normalisation="no") {
  indices_stem_branches <- which(branch$order == 1)
  if (length(indices_stem_branches) > 0) {
    branch_lengths=branch$length[indices_stem_branches]
    if (normalisation == "dbh") {
      dbh <- treedata$DBHqsm[1]
      sbl <- mean(branch_lengths)/dbh
    } else if (normalisation == "treeheight") {
      tree_height <- treedata$TreeHeight[1]
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
#' no normalisation.
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param normalisation Can either be "dbh" or nothing. In case of "dbh" the
#'   average distance is divided by the DBH (Akerblom et al., 2017).
#'
#' @return The stem branch distance.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' sbd_dbh <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata, "dbh")
#' sbd_nn <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata, "no")
stem_branch_distance_qsm <- function(cylinder, treedata, normalisation="no") {
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
      dbh <- treedata$DBHqsm[1]
      sbd <- mean(average_distance)/dbh
    } else {
      print("No normalisation")
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
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ratio_height <- dbh_height_ratio_qsm(qsm$treedata)
dbh_height_ratio_qsm <- function(treedata){
  dbh <- treedata$DBHqsm[1]
  tree_height <- treedata$TreeHeight[1]
  return(dbh/tree_height)
}

#' DBH-tree volume ratio TreeQSM
#'
#' Calculates DBH-tree volume ratio from a TreeQSM.
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#'
#' @return DBH divided by the tree volume (trunk plus branches).
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ratio_vol <- dbh_volume_ratio_qsm(qsm$treedata)
dbh_volume_ratio_qsm <- function(treedata){
  dbh <- treedata$DBHqsm[1]
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' vol_55 <- volume_below_55_qsm(qsm$cylinder, qsm$treedata)
volume_below_55_qsm <- function(cylinder, treedata){
  tree_height <- treedata$TreeHeight[1]
  volume_branches <- treedata$BranchVolume[1]
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
#' @return The cylinder length volume ratio.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' len_vol_ratio <- cylinder_length_volume_ratio_qsm(qsm$treedata)
cylinder_length_volume_ratio_qsm <- function(treedata){
  total_branch_volume <- treedata$BranchVolume[1]
  total_branch_length <- treedata$BranchLength[1]
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' shed_ratio <- shedding_ratio_qsm(qsm$branch, qsm$treedata)
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' ba_ratio <- branch_angle_ratio_qsm(qsm$branch)
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' relvol_ratio <- relative_volume_ratio(qsm$cylinder, qsm$treedata)
relative_volume_ratio <- function(cylinder,treedata){
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' crown <- crownset(qsm$cylinder)
crownset <- function(cylinder){
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
#' et al., 2020).
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param crownset  An integer containing the indices of the cylinders belonging
#'   to the crownset which is returned by \code{\link{crownset}}.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' crown <- crownset(qsm$cylinder)
#' csh <- crown_start_height(qsm$treedata,qsm$cylinder,crown)
crown_start_height <- function(treedata,cylinder,crownset){
  tree_height <- treedata$TreeHeight[1]
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
#' & Terryn et al., 2020).
#'
#' @param treedata Treedata field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param crownset An integer containing the indices of the cylinders belonging
#'   to the crownset which is returned by \code{\link{crownset}}.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' crown <- crownset(qsm$cylinder)
#' ch <- crown_height(qsm$treedata,qsm$cylinder,crown)
crown_height <- function(treedata,cylinder,crownset){
  tree_height <- treedata$TreeHeight[1]
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
#' (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @param cylinder Cylinder field of a TreeQSM that is returned by
#'   \code{\link{read_tree_qsm}}.
#' @param crownset An integer containing the indices of the cylinders belonging
#'   to the crownset which is returned by \code{\link{crownset}}.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
#' crown <- crownset(qsm$cylinder)
#' ce <- crown_evenness(qsm$cylinder,crown)
crown_evenness <- function(cylinder,crownset){
  if(length(crownset)>0){
    bins <- c(0*2*pi/8-pi,1*2*pi/8-pi,2*2*pi/8-pi,3*2*pi/8-pi,4*2*pi/8-pi,5*2*pi/8-pi,
              6*2*pi/8-pi,7*2*pi/8-pi,2*pi/1-pi)
    crownset_bo1 <- crownset[cylinder$BranchOrder[crownset]==1]
    crownset_bo1_po0 <- crownset_bo1[cylinder$BranchOrder[cylinder$parent[crownset_bo1]]==0]
    center_z <- min(cylinder$start[crownset_bo1_po0,3])
    center <- crownset_bo1_po0[cylinder$start[crownset_bo1_po0,3] == center_z]
    center_x <- cylinder$start[center,1]
    center_y <- cylinder$start[center,2]
    R <- sqrt(((cylinder$start[crownset,1]-center_x)^2+(cylinder$start[crownset,2]-center_y)^2))
    theta <- atan2((cylinder$start[crownset,2]-center_y),(cylinder$start[crownset,1]-center_x))
    minimums <- c()
    for (i in 2:length(bins)){
      indices_bin <- (theta < bins[i] & theta >= bins[(i-1)])
      if(sum(indices_bin)>0){
        minimums <- append(minimums,min(cylinder$start[crownset[indices_bin],3]))
      }
    }
    if(length(minimums)==length(bins)){
      ce <- (min(minimums)-min(cylinder$start[,3]))/(max(minimums)-min(cylinder$start[,3]))
    } else {
      ce <- 0
    }
  }
  return(ce)
}

#' Calculate the stem branch angle from a TreeQSM
#'
#' @param branch Branch field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The stem branch angle defined as "the median of the branching angles
#' of the 1st order branches in degrees. 0 is upwards and 180 downwards
#' (parallel with the trunk)" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @references
#' Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017). Automatic tree species recognition with quantitative structure models. Remote Sensing of Environment, 191, 1-12.
#'
#' Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using structural features derived from terrestrial laser scanning. ISPRS Journal of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' sba <- stem_branch_angle_qsm(branch)
stem_branch_angle_qsm <- function(branch) {
  indices_stem_branches <- which(branch$order == 1)
  angle_stem_branches <- branch$angle[indices_stem_branches]
  return(stats::median(angle_stem_branches))
}

#' Calculate the stem branch cluster size from a TreeQSM
#'
#' @param cylinder Cylinder field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#'
#' @return The stem branch cluster size defined as "the average number of 1st order
#' branches inside a 40cm height interval for 1st order branches. Each branch can
#' only belong to one interval" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @references
#' Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017). Automatic tree species recognition with quantitative structure models. Remote Sensing of Environment, 191, 1-12.
#'
#' Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using structural features derived from terrestrial laser scanning. ISPRS Journal of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' sbcs <- stem_branch_cluster_size_qsm(cylinder)
stem_branch_cluster_size_qsm <- function(cylinder) {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1 & cylinder$BranchOrder == 1)
  cylinder_heights <- cylinder$start[indices_stem_cylinders,3]
  cylinder_heights <- cylinder_heights[order(cylinder_heights)]
  start_heights <- cylinder_heights - 0.2
  end_heights <- cylinder_heights + 0.2
  label <- integer(length(cylinder_heights))
  cluster <- c()
  for(i in 1:length(cylinder_heights)) {
    indices_in_interval <- which(cylinder_heights > start_heights[i] & cylinder_heights < end_heights[i])
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

#' Calculate the stem branch radius from a TreeQSM
#'
#' @param cylinder Cylinder field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param normalisation Can be either "treeheight" or "parentcylinder".
#' In case of "treeheight" the mean radius of the 10 biggest branches is divided
#' by the tree height (Terryn et al., 2020). In case of "parentcylinder" the mean
#' is taken of the ratios of the radius of the 10 biggest branches and the radius of
#' their parent cylinders (Akerblom et al., 2017). When something different than
#' "treeheight" or "parentcylinder" is given, no normalisation is done. Default
#' is no normalisation.
#'
#' @return The stem branch radius defined as "Mean of the 10 largest
#' 1st order branches measured at the base. Can be normalised by the tree height or
#' the the stem radius at respective height" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @references
#' Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017). Automatic tree species recognition with quantitative structure models. Remote Sensing of Environment, 191, 1-12.
#'
#' Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using structural features derived from terrestrial laser scanning. ISPRS Journal of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' sbr_th <- stem_branch_radius_qsm(cylinder, treedata, "treeheight")
#' sbr_pc <- stem_branch_radius_qsm(cylinder, treedata, "parentcylinder")
#' sbr_nn <- stem_branch_radius_qsm(cylinder, treedata, "no")
stem_branch_radius_qsm <- function(cylinder, treedata, normalisation="no") {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1 & cylinder$BranchOrder == 1)
  if(length(indices_stem_cylinders) > 0) {
    branch_radius <- cylinder$radius[indices_stem_cylinders]
    branch_indices_sorted <- indices_stem_cylinders[order(branch_radius, decreasing = TRUE)]
    branch_radius_sorted <- branch_radius[order(branch_radius, decreasing = TRUE)]
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

#' Calculate the stem branch length from a TreeQSM
#'
#' @param branch Branch field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param normalisation Can be either "dbh" or "treeheight". In case of "dbh" the
#' mean of the lengths of the stem branches are divided by the DBH (Akerblom et al., 2017).
#' In case of "treeheight" the mean is divided by the tree height (Terryn et al., 2020).
#' When something different than "dbh" or "treeheight" is given, no normalisation is done.
#' Default is no normalisation.
#'
#' @return The stem branch length defined as "the average length of 1st order
#' branches. Can be normalised by DBH or tree height" (Akerblom et al., 2017 & Terryn et al., 2020).
#'
#' @references
#' Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017). Automatic tree species recognition with quantitative structure models. Remote Sensing of Environment, 191, 1-12.
#'
#' Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using structural features derived from terrestrial laser scanning. ISPRS Journal of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' sbl_dbh <- stem_branch_length_qsm(branch, treedata, "dbh")
#' sbl_th <- stem_branch_length_qsm(branch, treedata, "treeheight")
#' sbl_nn <- stem_branch_length_qsm(branch, treedata, "no")
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

#' Calculate the stem branch distance from a TreeQSM
#'
#' @param cylinder Cylinder field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param treedata Treedata field of a TreeQSM which is imported in the
#' global environment with the read_tree_qsm function.
#' @param normalisation Can either be "dbh" or nothing. In case of "dbh" the average
#' distance is divided by the DBH (Akerblom et al., 2017).
#'
#' @return The stem branch distance defined as "Average distance between 1st order
#' branches computed using a moving average with a window width 1 m. If window is
#' empty average distance in window is set as half of window width. Can be normalised by
#' the DBH" (Akerblom et al., 2017 & Terryn et al., 2020). When something different
#' than "dbh" is given, no normalisation is done. Default is no normalisation.
#'
#' @references
#' Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017). Automatic tree species recognition with quantitative structure models. Remote Sensing of Environment, 191, 1-12.
#'
#' Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G., ... & Verbeeck, H. (2020). Tree species classification using structural features derived from terrestrial laser scanning. ISPRS Journal of Photogrammetry and Remote Sensing, 168, 170-181.
#'
#' @export
#'
#' @examples
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' read_tree_qsm(QSM_path)
#' sbd_dbh <- stem_branch_distance_qsm(cylinder, treedata, "dbh")
#' sbd_nn <- stem_branch_distance_qsm(cylinder, treedata, "no")
stem_branch_distance_qsm <- function(cylinder, treedata, normalisation="no") {
  indices_stem_cylinders <- which(cylinder$PositionInBranch == 1 & cylinder$BranchOrder == 1)
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
      indices_in_interval <- which(cylinder_heights > start_heights[i] & cylinder_heights < end_heights[i])
      if(length(indices_in_interval) == 1) {
        average_distance <- append(average_distance,0.5)
      } else {
        distance <- c()
        for(j in 1:length(indices_in_interval)-1) {
          distance <- append(distance, height_difference[indices_in_interval[j]])
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

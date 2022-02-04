#' Summary features Terryn et al. (2020)
#'
#' Returns a summary data.frame containing the tree height, dbh and the metrics
#' defined by Terryn et al. (2020). Also contains the projected crown area and
#' crown volume (NaN when point clouds are not available).
#'
#' Metrics Terryn et al. (2020): stem branch angle (sba,
#' \code{\link{stem_branch_angle_qsm}}), stem branch cluster size (sbcs,
#' \code{\link{stem_branch_cluster_size_qsm}}), stem branch radius (sbr,
#' \code{\link{stem_branch_radius_qsm}}), stem branch length (sbl,
#' \code{\link{stem_branch_length_qsm}}), stem branch distance (sbd,
#' \code{\link{stem_branch_distance_qsm}}), dbh height ratio (dhr,
#' \code{\link{dbh_height_ratio_qsm}}), dbh volume ratio (dvr,
#' \code{\link{dbh_volume_ratio_qsm}}), volume below 55 (vb55,
#' \code{\link{volume_below_55_qsm}}), cylinder length volume ratio (clvr,
#' \code{\link{cylinder_length_volume_ratio_qsm}}), shedding ratio (sr,
#' \code{\link{shedding_ratio_qsm}}), branch angle ratio (bar,
#' \code{\link{branch_angle_ratio_qsm}}), relative volume ratio (rvr,
#' \code{\link{relative_volume_ratio_qsm}}), crown start height (csh,
#' \code{\link{crown_start_height_qsm}}), crown height (ch,
#' \code{\link{crown_height_qsm}}), crown evenness (ce,
#' \code{\link{crown_evenness_qsm}}), crown diameter height ratio (cdhr,
#' \code{\link{crown_diameterheight_ratio_qsm}}), dbh minimum radius ratio (dmr,
#' \code{\link{dbh_minradius_ratio_qsm}}). Tree height, DBH, crown projection
#' area and crown volume are determined with \code{\link{tree_height}},
#' \code{\link{dbh}}, \code{\link{projected_crown_area_pc}} and
#' \code{\link{volume_crown_pc}}. All functions are run with default parameters
#' (except for the parameters pc and buttress if point clouds are available.)
#'
#' @param QSMs_path A charachter with the path to the folder that contains the
#'   treeQSMs. These files have to be of the format xxx_000_qsm.mat (xxx is the
#'   plotname, 000 is the tree number) or xxx_000_qsm_0.mat (0 at the end is for
#'   example the n-th QSM that is made for tree 000). When multiple QSMs are
#'   present for one tree the mean of the values of the different QSMs is taken
#'   for that tree as a final value for a certain feature.
#' @param version A character indicating the version of TreeQSM that was used to
#'   produce the qsms. Default version is 2.4.0. Other possible versions are
#'   2.2.0.
#' @param PCs_path A charachter with the path to the folder that contains the
#'   tree point clouds. Default is "NA" when the point clouds are not available.
#'   The point clouds are used to determine the DBH, tree height, projected
#'   crown area and crown volume. The DBH and tree height obtained from the tree
#'   point clouds are then used for the normalisation of the other features. The
#'   point cloud files have to be of the format xxx_000_pc in order to link the
#'   tree point cloud to its' respective treeQSM.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if the tree point clouds are available.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las". Only
#'   relevant if the tree point clouds are available.
#'
#' @return The summary of all metrics from Terryn et al. (2020) as a data.frame.
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
#' QSMs_path <- "path/to/folder/QSMs/"
#' PCs_path <- "path/to/folder/PCs/"
#' summary <- summary_Terryn_2020(QSMs_path)
#' summary <- summary_Terryn_2020(QSMs_path,version="2.4.0",PCs_path,
#'                                buttress=TRUE,extension=".txt")
#' }
summary_Terryn_2020 <- function(QSMs_path,version="2.4.0",PCs_path="NA",
                                buttress=FALSE,extension=".txt"){
  filenames <- list.files(QSMs_path, pattern="*.mat", full.names=FALSE)
  unique_tree_ids <- c()
  tree_ids <- c()
  for (i in 1:length(filenames)){
    id <- strsplit(filenames[i],"_qsm_")[[1]][1]
    if(!(id %in% tree_ids)){
      unique_tree_ids <- append(unique_tree_ids,id)
    }
    tree_ids <- append(tree_ids,id)
  }
  df <- data.frame(height=double(),
                   dbh=double(),
                   pca=double(),
                   cv=double(),
                   sba=double(),
                   sbcs=double(),
                   sbr=double(),
                   sbl=double(),
                   sbd=double(),
                   dhr=double(),
                   dvr=double(),
                   vb55=double(),
                   clvr=double(),
                   sr=double(),
                   bar=double(),
                   rvr=double(),
                   csh=double(),
                   ch=double(),
                   ce=double(),
                   cdhr=double(),
                   dmr=double())
  results <- df
  for (i in 1:length(unique_tree_ids)){
    print(paste("processing ", unique_tree_ids[i]))
    qsms <- filenames[tree_ids == unique_tree_ids[i]]
    if (PCs_path != "NA"){
      pc <- read_tree_pc(paste(PCs_path,unique_tree_ids[i],"_pc",extension,sep = ""))
      pca <- projected_crown_area_pc(pc)
      vol <- volume_crown_pc(pc)
    } else {
      pc <- "NA"
      pca <- NaN
      vol <- NaN
    }
    trees <- df
    for (j in 1:length(qsms)){
      print(paste("processing ", unique_tree_ids[i], as.character(j)))
      qsm <- read_tree_qsm(paste(QSMs_path,qsms[j],sep = ""),version)
      h <- tree_height(qsm$treedata,pc)
      dbh <- dbh(qsm$treedata,pc,buttress)
      sba <- stem_branch_angle_qsm(qsm$branch)
      sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
      sbr <- stem_branch_radius_qsm(qsm$cylinder,qsm$treedata,"treeheight",pc)
      sbl <- stem_branch_length_qsm(qsm$branch,qsm$treedata,"treeheight",pc)
      sbd <- stem_branch_distance_qsm(qsm$cylinder,qsm$treedata,"no",pc,
                                      buttress)
      dhr <- dbh_height_ratio_qsm(qsm$treedata,pc,buttress)
      dvr <- dbh_volume_ratio_qsm(qsm$treedata,pc,buttress)
      vb55 <- volume_below_55_qsm(qsm$cylinder,qsm$treedata)
      clvr <- cylinder_length_volume_ratio_qsm(qsm$treedata)
      sr <- shedding_ratio_qsm(qsm$branch,qsm$treedata)
      bar <- branch_angle_ratio_qsm(qsm$branch)
      rvr <- relative_volume_ratio_qsm(qsm$cylinder,qsm$treedata)
      csh <- crown_start_height_qsm(qsm$treedata,qsm$cylinder,pc)
      ch <- crown_height_qsm(qsm$treedata,qsm$cylinder,pc)
      ce <- crown_evenness_qsm(qsm$cylinder)
      cdhr <- crown_diameterheight_ratio_qsm(qsm$treedata,qsm$cylinder,pc)
      dmr <- dbh_minradius_ratio_qsm(qsm$treedata,qsm$cylinder,pc,buttress)
      tree <- data.frame(height=h,
                         dbh=dbh,
                         pca=pca,
                         cv=vol,
                         sba=sba,
                         sbcs=sbcs,
                         sbr=sbr,
                         sbl=sbl,
                         sbd=sbd,
                         dhr=dhr,
                         dvr=dvr,
                         vb55=vb55,
                         clvr=clvr,
                         sr=sr,
                         bar=bar,
                         rvr=rvr,
                         csh=csh,
                         ch=ch,
                         ce=ce,
                         cdhr=cdhr,
                         dmr=dmr)
      trees <- rbind(trees,tree)
    }
    m <- as.data.frame.list(colMeans(trees))
    results <- rbind(results,m)
    summary <- cbind(unique_tree_ids,results)
  }
  return(summary)
}

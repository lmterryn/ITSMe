#' Summary basic structural metrics tree point cloud
#'
#' Returns a summary data.frame containing the tree position (X,Y-coordinates),
#' tree height, diameter at breast height, diameter above buttresses, projected
#' crown area and crown volume.
#'
#' The tree position, tree height, diameter at breast height, diameter above
#' buttresses, projected crown area and crown volume are otained with
#' \code{\link{tree_position_pc}}, \code{\link{tree_height_pc}},
#' \code{\link{dbh_pc}}, \code{\link{dab_pc}},
#' \code{\link{projected_crown_area_pc}} and \code{\link{volume_crown_pc}}
#' respectively.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las". Only
#'   relevant if the tree point clouds are available.
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#' @param concavity Numeric value (default=2). Parameter of the
#'   \code{\link{projected_crown_area_pc}} function used to calculate the
#'   projected crown area.
#' @param alpha Numeric value (default=1). Parameter of the
#'   \code{\link{volume_crown_pc}} function used to calculate the crown volume.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param OUT_path A character with the path to the folder where the summary csv
#'   file should be saved or logical (default=FALSE) in this case no csv file is
#'   produced.
#' @param plot Logical (default=FALSE), indicates if summary figure for each
#'   tree point cloud is plotted. If an OUT_path is provided, the figures are
#'   saved in the OUT_path.
#'
#'
#' @return The summary of the basic structural metrics for multiple tree point
#'   clouds as a data.frame. Includes the tree height, diameter at breast
#'   height, diameter above buttresses, projected crown area and crown volume.
#'   The summary is saved in a csv file if an output folder is provided.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate the summary with default parameters and export to csv
#' # recommended for non-buttressed trees
#' summary <- summary_basic_pointcloud_metrics(PCs_path = "path/to/folder/PCs/",
#'                                             OUT_path = "path/to/out/folder/")
#' # Calculate the summary with non-default parameter values
#' # recommended for buttressed trees
#' summary <- summary_basic_pointcloud_metrics(PCs_path = "path/to/folder/PCs/",
#'                                             extension = ".ply",
#'                                             minheight = 4, buttress = TRUE)
#' }
summary_basic_pointcloud_metrics <- function(PCs_path, extension = ".txt",
                                             thresholdbranch = 1.5,
                                             minheight = 1, concavity = 2,
                                             alpha = 1, buttress = FALSE,
                                             thresholdbuttress = 0.001,
                                             maxbuttressheight = 7,
                                             OUT_path = FALSE, plot = FALSE) {
  trees <- data.frame(
    "tree_id" = character(), "X_position" = double(),
    "Y_position" = double(), "tree_height" = double(),
    "diameter_at_breast_height" = double(),
    "diameter_above_buttresses" = double(),
    "projected_crown_area" = double(), "crown_volume" = double())
  diameter_above_buttresses <- NULL
  if (buttress == FALSE){
    trees <- subset(trees, select = -diameter_above_buttresses)
  }
  filepaths <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                          full.names = TRUE)
  filenames <- list.files(PCs_path, pattern = paste("*", extension, sep = ""),
                          full.names = FALSE)
  for (i in 1:length(filenames)) {
    print(paste("processing ", filenames[i]))
    pc <- read_tree_pc(filepaths[i])
    pos <- tree_position_pc(pc)
    h <- tree_height_pc(pc)
    if (buttress){
      dab_out <- dab_pc(pc, thresholdbuttress, maxbuttressheight, plot)
    } else {
      dab <- NA
    }
    dbh_out <- dbh_pc(pc, plot)
    classify_out <- classify_crown_pc(pc, thresholdbranch, minheight, buttress,
                                      thresholdbuttress, maxbuttressheight,
                                      plot)
    pca_out <- projected_crown_area_pc(pc, concavity, thresholdbranch, minheight,
                                   buttress, thresholdbuttress,
                                   maxbuttressheight, plot)
    cv <- volume_crown_pc(pc, alpha, thresholdbranch, minheight, buttress,
                          thresholdbuttress, maxbuttressheight)
    if (plot){
     dbh <- dbh_out$dbh
     if (buttress){
       dab <- dab_out$dab
     }
     pca <- pca_out$pca
    }
    tree <- data.frame(
      "tree_id" = filenames[i], "X_position" = pos[1],
      "Y_position" = pos[2], "tree_height" = h,
      "diameter_at_breast_height" = dbh,
      "diameter_above_buttresses" = dab,
      "projected_crown_area" = pca, "crown_volume" = cv)
    if (buttress == FALSE){
      tree <- subset(tree, select = -diameter_above_buttresses)
    }
    trees <- rbind(trees, tree)
    if (plot){
      p0 <- pca_out$plot
      p0 <- p0 + ggplot2::ggtitle(label = bquote(PCA == .(round(pca,2)) ~ m^2),
                                  subtitle = bquote(CV == .(round(cv,2)) ~ m^3))
      if (buttress) {
        p1 <- ggpubr::ggarrange(dab_out$plot, p0, nrow = 2, ncol = 1,
                                widths = c(1,1))
      } else {
        p1 <- ggpubr::ggarrange(dbh_out$plot, p0, nrow = 2, ncol = 1,
                        widths = c(1,1))
      }
      p2 <- ggpubr::ggarrange(classify_out$plotXZ, classify_out$plotYZ,
                              nrow = 1, ncol = 2, heights = c(1,1),
                              common.legend = TRUE, align = "hv",
                              legend = "bottom")
      p2 <- ggpubr::annotate_figure(p2,
                              top = ggpubr::text_grob(paste("Tree height = ",
                                                            as.character(
                                                              round(h,2)),
                                                            " m", sep = "")))
      p3 <- ggpubr::ggarrange(p2, p1, nrow = 1, ncol = 2, align = "hv")
      p3 <- ggpubr::annotate_figure(p3, top = ggpubr::text_grob(paste("Summary ",
                                                strsplit(filenames[i],
                                                         extension)[[1]],
                                                sep = ""), face = "bold"))

    }
    if (is.character(OUT_path)){
      utils::write.csv(trees,paste(OUT_path,"pointcloud_metrics.csv", sep = ""),
                       row.names = FALSE)
      if (plot) {
        grDevices::jpeg(file=paste(OUT_path,"summary_figure_",
                        strsplit(filenames[i], extension)[[1]],".jpeg",
                        sep = ""), res=600, width=4800, height=3000)
        print(p3)
        grDevices::dev.off()
      }
    }
  }
  return(trees)
}


#' Summary structural metrics Terryn et al. (2020)
#'
#' Returns a summary data.frame containing all the metrics defined by Terryn et
#' al. (2020). Also contains: X and Y-position, dbh, tree height, tree volume
#' and trunk volume. When tree point clouds are provided, dbh and tree height
#' are based on the point cloud instead of the QSM.
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
#' \code{\link{dbh_minradius_ratio_qsm}}).
#'
#' @param QSMs_path A character with the path to the folder that contains the
#'   treeQSMs. These files have to be of the format xxx_000_qsm.mat (xxx is the
#'   plotname, 000 is the tree number) or xxx_000_qsm_0.mat (0 at the end is for
#'   example the n-th QSM that is made for tree 000). When multiple QSMs are
#'   present for one tree the mean of the values of the different QSMs is taken
#'   for that tree as a final value for a certain feature.
#' @param version A character indicating the version of TreeQSM that was used to
#'   produce the qsms. Default version is 2.4.0. Other possible versions are
#'   2.2.0.
#' @param sbr_normalisation Character (default="treeheight"). Normalisation
#'   parameter of \code{\link{stem_branch_radius_qsm}}.
#' @param sbl_normalisation Character (default="treeheight"). Normalisation
#'   parameter of \code{\link{stem_branch_length_qsm}}.
#' @param sbd_normalisation Character (default="no"). Normalisation parameter of
#'   \code{\link{stem_branch_distance_qsm}}.
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds. Default is NA when the point clouds are not available.
#'   The point clouds are used to determine the DBH, tree height, projected
#'   crown area and crown volume. The DBH and tree height obtained from the tree
#'   point clouds are then used for the normalisation of the other features. The
#'   point cloud files have to be of the format xxx_000_pc in order to link the
#'   tree point cloud to its' respective treeQSM.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las". Only
#'   relevant if the tree point clouds are available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if the tree point clouds are available. Only
#'   relevant if the tree point clouds are available.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant if the tree point clouds are available and
#'   buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant if the tree point clouds are available and
#'   buttress == TRUE.
#' @param OUT_path A character with the path to the folder where the summary csv
#'   file should be saved or logical (default=FALSE) in this case no csv file is
#'   produced.
#'
#' @return The summary of all metrics from Terryn et al. (2020) as a data.frame.
#'   The summary is saved in a csv file if an output folder is provided. If
#'   multiple QSMs are provided for all trees the mean values and standard
#'   deviations for each tree are also calculated and saved in 2 other csv
#'   files. In this case the function returns a list of the summaries with the
#'   means and standard deviations in the second and third element of the list
#'   respectively.
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
#' # Calculate the summary with default parameters and export to csv
#' # recommended for non-buttressed trees
#' summary <- summary_Terryn_2020(QSMs_path = "path/to/folder/QSMs/",
#'                                OUT_path = "path/to/out/folder/")
#' # also using point cloud info
#' summary <- summary_Terryn_2020(QSMs_path = "path/to/folder/QSMs/",
#'                                PCs_path = "path/to/folder/PCs/",
#'                                extension = ".txt",
#'                                OUT_path = "path/to/out/folder/")
#' # Calculate the summary with non-default parameter values
#' # recommended for buttressed trees
#' summary <- summary_Terryn_2020(QSMs_path = "path/to/folder/QSMs/",
#'                                PCs_path = "path/to/folder/PCs/",
#'                                extension = ".txt", buttress = TRUE,
#'                                OUT_path = "path/to/out/folder/")
#' }
summary_Terryn_2020 <- function(QSMs_path, version = "2.4.0",
                                sbr_normalisation = "treeheight",
                                sbl_normalisation = "treeheight",
                                sbd_normalisation = "no",
                                PCs_path = NA, extension = ".txt",
                                buttress = FALSE, thresholdbuttress = 0.001,
                                maxbuttressheight = 7, OUT_path = FALSE) {
  filenames <- list.files(QSMs_path, pattern = "*.mat", full.names = FALSE)
  unique_tree_ids <- c()
  tree_ids <- c()
  for (i in 1:length(filenames)) {
    id <- strsplit(filenames[i], "_qsm_")[[1]][1]
    if (!(id %in% tree_ids)) {
      unique_tree_ids <- append(unique_tree_ids, id)
    }
    tree_ids <- append(tree_ids, id)
  }
  df <- data.frame(X_position = double(), Y_position = double(), dbh = double(),
                   tree_height = double(), tree_vol = double(),
                   trunk_vol = double(), sba = double(), sbcs = double(),
                   sbr = double(), sbl = double(), sbd = double(),
                   dhr = double(), dvr = double(), vb55 = double(),
                   clvr = double(), sr = double(), bar = double(),
                   rvr = double(), csh = double(), ch = double(), ce = double(),
                   cdhr = double(), dmr = double())
  results <- df
  summary <- summary_means <- summary_sds <- cbind(tree_id = character(),
                                                   results)
  for (i in 1:length(unique_tree_ids)) {
    print(paste("processing ", unique_tree_ids[i]))
    qsms <- filenames[tree_ids == unique_tree_ids[i]]
    if (!is.na(PCs_path)) {
      pc <- read_tree_pc(paste(PCs_path, unique_tree_ids[i], "_pc", extension,
                               sep = ""))
    } else {
      pc <- NA
    }
    trees <- df
    id <- unique_tree_ids[i]
    for (j in 1:length(qsms)) {
      print(paste("processing ", unique_tree_ids[i], as.character(j)))
      qsm <- read_tree_qsm(paste(QSMs_path, qsms[j], sep = ""), version)
      position <- tree_position_qsm(qsm$cylinder)
      X_position <- position[1]
      Y_position <- position[2]
      dbh <- dbh(qsm$treedata, pc, buttress, thresholdbuttress,
                 maxbuttressheight)
      tree_height <- tree_height(qsm$treedata, pc)
      tree_vol <- tree_volume_qsm(qsm$treedata)
      trunk_vol <- trunk_volume_qsm(qsm$treedata)
      sba <- stem_branch_angle_qsm(qsm$branch)
      sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
      sbr <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata,
                                    sbr_normalisation, pc)
      sbl <- stem_branch_length_qsm(qsm$branch, qsm$treedata, sbl_normalisation,
                                    pc, buttress)
      sbd <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata,
                                      sbd_normalisation, pc, buttress)
      dhr <- dbh_height_ratio_qsm(qsm$treedata, pc, buttress)
      dvr <- dbh_volume_ratio_qsm(qsm$treedata, pc, buttress)
      vb55 <- volume_below_55_qsm(qsm$cylinder, qsm$treedata)
      clvr <- cylinder_length_volume_ratio_qsm(qsm$treedata)
      sr <- shedding_ratio_qsm(qsm$branch, qsm$treedata)
      bar <- branch_angle_ratio_qsm(qsm$branch)
      rvr <- relative_volume_ratio_qsm(qsm$cylinder, qsm$treedata)
      csh <- crown_start_height_qsm(qsm$treedata, qsm$cylinder, pc)
      ch <- crown_height_qsm(qsm$treedata, qsm$cylinder, pc)
      ce <- crown_evenness_qsm(qsm$cylinder)
      cdhr <- crown_diameterheight_ratio_qsm(qsm$treedata, qsm$cylinder, pc)
      dmr <- dbh_minradius_ratio_qsm(qsm$treedata, qsm$cylinder, pc, buttress)
      tree <- data.frame(X_position = X_position, Y_position = Y_position,
                         dbh = dbh, tree_height = tree_height,
                         tree_vol = tree_vol, trunk_vol = trunk_vol, sba = sba,
                         sbcs = sbcs, sbr = sbr, sbl = sbl, sbd = sbd,
                         dhr = dhr, dvr = dvr, vb55 = vb55, clvr = clvr,
                         sr = sr, bar = bar, rvr = rvr, csh = csh, ch = ch,
                         ce = ce, cdhr = cdhr, dmr = dmr)
      trees <- rbind(trees, tree)
    }
    results <- cbind(tree_id = id, trees)
    summary <- rbind(summary, results)
    if (length(qsms) > 1){
    m <- as.data.frame.list(colMeans(trees))
    s <- as.data.frame.list(sapply(trees, stats::sd, na.rm = TRUE))
    results_means <- cbind(tree_id = id, m)
    results_sds <- cbind(tree_id = id, s)
    summary_means <- rbind(summary_means, results_means)
    summary_sds <- rbind(summary_means, results_sds)
    summaries <- list("summary" = summary, "means" = summary_means,
                      "sds" = summary_sds)
    } else {
      summaries <- summary
    }
    if (is.character(OUT_path)){
      utils::write.csv(summary,paste(OUT_path,
                                           "Terryn_2020_metrics.csv",
                                           sep = ""), row.names = FALSE)
      if (length(qsms) > 1){
      utils::write.csv(summary_means,paste(OUT_path,
                                           "Terryn_2020_metrics_means.csv",
                                           sep = ""), row.names = FALSE)
      utils::write.csv(summary_sds,paste(OUT_path,"Terryn_2020_metrics_sds.csv",
                                         sep = ""), row.names = FALSE)
      }
    }
  }
  return(summaries)
}

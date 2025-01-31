#' Summary basic structural metrics tree point cloud
#'
#' Returns a summary data.frame containing the tree position (X,Y-coordinates),
#' tree height, diameter at breast height, functional diameter at breast height,
#' diameter above buttresses, functional diameter above buttresses, projected
#' (crown) area and (crown) volume.
#'
#' The tree position, tree height, diameter at breast height, functional
#' diameter at breast height, diameter above buttresses, functional diameter
#' above buttresses, projected (crown) area and (crown) volume are otained with
#' \code{\link{tree_position_pc}}, \code{\link{tree_height_pc}},
#' \code{\link{dbh_pc}}, \code{\link{dab_pc}}, \code{\link{projected_area_pc}}
#' and \code{\link{alpha_volume_pc}} respectively.
#'
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las". Only
#'   relevant if the tree point clouds are available.
#' @param dtm The digital terrain model from \code{\link{tree_height_pc}}.
#' @param r Numeric value (default=5) r which determines the range taken for the
#'   dtm from \code{\link{tree_height_pc}}. Only relevant if a dtm is provided.
#' @param crown Logical (default=FALSE), indicates if the area and volume is
#'   calculated based on the full point clouds (crown = FALSE) or only on the
#'   crown point clouds (crown = TRUE).
#' @param thresholdbranch Numeric value (default=1.5) from
#'   \code{\link{classify_crown_pc}}. Only relevant when crown == TRUE.
#' @param minheight Numeric value (default=1) from
#'   \code{\link{classify_crown_pc}}. The default value is based on
#'   non-buttressed trees. Choose a higher value (e.g. 4) for buttressed trees.
#'   Only relevant when crown == TRUE.
#' @param concavity Numeric value (default=2). Parameter of the
#'   \code{\link{projected_area_pc}} function used to calculate the projected
#'   crown area.
#' @param alpha Numeric value (default=1). Parameter of the
#'   \code{\link{alpha_volume_pc}} function used to calculate the crown volume.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant if buttress == FALSE.
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} and \code{\link{dab_pc}} functions used to calculate
#'   the diameter at breast height and above buttresses.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param functional Logical (default=FALSE), indicates if the functional
#'   diameter should be calculated.
#' @param concavity_fdiameter Numeric value (default=4) concavity for the
#'   computation of the functional diameter using a concave hull based on
#'   \code{\link[concaveman]{concaveman}}. This concavity value is used in the
#'   functions \code{\link{diameter_slice_pc}}, \code{\link{dbh_pc}},
#'   \code{\link{dab_pc}}, and \code{\link{classify_crown_pc}}.
#' @param OUT_path A character with name of the output file (including the path
#'   to the folder), where the summary csv file should be saved or logical
#'   (default=FALSE) in this case no csv file is produced.
#' @param plot Logical (default=FALSE), indicates if summary figure for each
#'   tree point cloud is plotted. If an OUT_path is provided, the figures are
#'   saved in the OUT_path.
#' @param plotcolors list of five colors for plotting. Only relevant when plot =
#'   TRUE. The stem points above buttresses, stem points at breast height,
#'   fitted circle, the concave hull and the estimated center are colored by the
#'   first, second, third, fourth and fifth element of this list respectively.
#'
#' @return The summary of the basic structural metrics for multiple tree point
#'   clouds as a data.frame. Includes the tree height, diameter at breast
#'   height, diameter above buttresses, projected (crown) area and (crown)
#'   volume. The summary is saved in a csv file if an output folder is provided.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate the summary with default parameters and export to csv
#' summary <- summary_basic_pointcloud_metrics(
#'   PCs_path = "path/to/folder/PCs/",
#'   OUT_path = "path/to/out/folder/"
#' )
#' # Calculate the summary with non-default parameter values
#' # recommended for buttressed trees
#' summary <- summary_basic_pointcloud_metrics(
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".ply", crown = TRUE,
#'   minheight = 4, buttress = TRUE
#' )
#' }
summary_basic_pointcloud_metrics <-
  function(PCs_path,
           extension = ".txt",
           dtm = NA,
           r = 5,
           crown = FALSE,
           thresholdbranch = 1.5,
           minheight = 1,
           concavity = 2,
           alpha = 1,
           buttress = FALSE,
           thresholdR2 = 0.001,
           slice_thickness = 0.06,
           thresholdbuttress = 0.001,
           maxbuttressheight = 7,
           functional = TRUE,
           concavity_fdiameter = 4,
           OUT_path = FALSE,
           plot = FALSE,
           plotcolors = c("#000000", "#808080", "#1c027a", "#08aa7c", "#fac87f")) {
    trees <- data.frame(
      "tree_id" = character(),
      "X_position" = double(),
      "Y_position" = double(),
      "tree_height_m" = double(),
      "diameter_at_breast_height_m" = double(),
      "functional_diameter_at_breast_height_m" = double(),
      "diameter_above_buttresses_m" = double(),
      "functional_diameter_above_buttresses_m" = double(),
      "projected_area_m2" = double(),
      "alpha_volume_m3" = double()
    )
    diameter_above_buttresses_m <- diameter_at_breast_height_m <- NULL
    functional_diameter_above_buttresses_m <-
      functional_diameter_at_breast_height_m <- NULL
    if (buttress == FALSE) {
      trees <- subset(
        trees,
        select = -c(
          diameter_above_buttresses_m,
          functional_diameter_above_buttresses_m
        )
      )
    } else {
      trees <- subset(
        trees,
        select = -c(
          diameter_at_breast_height_m,
          functional_diameter_at_breast_height_m
        )
      )
    }
    filepaths <- list.files(PCs_path,
                            pattern = paste("*", extension, sep = ""),
                            full.names = TRUE)
    filenames <- list.files(PCs_path,
                            pattern = paste("*", extension, sep = ""),
                            full.names = FALSE)
    for (i in 1:length(filenames)) {
      print(paste("processing ", filenames[i]))
      pc <- read_tree_pc(path = filepaths[i])
      pos <- tree_position_pc(pc = pc)
      h_out <-
        tree_height_pc(
          pc = pc,
          dtm = dtm,
          r = r,
          plot = plot,
          plotcolors = plotcolors[c(1, 4:5)]
        )
      h <- h_out$h
      if (buttress) {
        dab_out <- tryCatch({
          dab_pc(
            pc = pc,
            thresholdbuttress = thresholdbuttress,
            maxbuttressheight = maxbuttressheight,
            slice_thickness = slice_thickness,
            functional = functional,
            concavity = concavity_fdiameter,
            dtm = dtm,
            r = r,
            plot = plot,
            plotcolors = plotcolors
          )
        }, error = function(cond) {
          message(cond)
          return(list(
            "dab" = NaN,
            "R2" = NaN,
            "fdab" = NaN,
            "plot" = NaN
          ))
        })
        dab <- dab_out$dab
        fdab <- dab_out$fdab
        dbh <- fdbh <- NaN
      } else {
        dbh_out <- tryCatch({
          dbh_out <-
            dbh_pc(
              pc = pc,
              thresholdR2 = thresholdR2,
              slice_thickness = slice_thickness,
              functional = functional,
              concavity = concavity_fdiameter,
              dtm = dtm,
              r = r,
              plot = plot,
              plotcolors = plotcolors[c(1, 3:5)]
            )
        }, error = function(cond) {
          message(cond)
          return(list(
            "dbh" = NaN,
            "R2" = NaN,
            "fdbh" = NaN,
            "plot" = NaN
          ))
        })
        dbh <- dbh_out$dbh
        fdbh <- dbh_out$fdbh
        dab <- fdab <- NaN
      }
      if (crown) {
        classify_out <- tryCatch({
          classify_crown_pc(
            pc = pc,
            thresholdbranch = thresholdbranch,
            minheight = minheight,
            buttress = buttress,
            thresholdR2 = thresholdR2,
            slice_thickness = slice_thickness,
            thresholdbuttress = thresholdbuttress,
            maxbuttressheight = maxbuttressheight,
            concavity = concavity_fdiameter,
            dtm = dtm,
            r = r,
            plot = plot,
            plotcolors = plotcolors[c(4:5)]
          )
        }, error = function(cond) {
          message(
            paste(
              cond,
              "!crown classification not possible, will calculate tree area and volume",
              sep = ""
            )
          )
          return(
            list(
              "crownpoints" = pc,
              "trunkpoints" = NaN,
              "plot" = NaN,
              "plotXZ" = NaN,
              "plotYZ" = NaN
            )
          )
        })
        pc <- classify_out$crownpoints
      } else {
        classify_out <-
          classify_out_empty <- stats::setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("crownpoints", "trunkpoints"))
      }
      pa_out <- projected_area_pc(
        pc = pc,
        concavity = concavity,
        plot = plot,
        plotcolors = plotcolors[c(1, 4)]
      )
      av <- alpha_volume_pc(pc = pc, alpha = alpha)
      if (plot) {
        pa <- pa_out$pa
      } else {
        pa <- pa_out
      }
      tree <- data.frame(
        "tree_id" = filenames[i],
        "X_position" = pos[1],
        "Y_position" = pos[2],
        "tree_height_m" = h,
        "diameter_at_breast_height_m" = dbh,
        "functional_diameter_at_breast_height_m" = fdbh,
        "diameter_above_buttresses_m" = dab,
        "functional_diameter_above_buttresses_m" = fdab,
        "projected_area_m2" = pa,
        "alpha_volume_m3" = av
      )
      if (buttress == FALSE) {
        tree <- subset(
          tree,
          select = -c(
            diameter_above_buttresses_m,
            functional_diameter_above_buttresses_m
          )
        )
      } else {
        tree <- subset(
          tree,
          select = -c(
            diameter_at_breast_height_m,
            functional_diameter_at_breast_height_m
          )
        )
      }
      trees <- rbind(trees, tree)
      if (plot) {
        p0 <- pa_out$plot
        if (crown & length(classify_out$trunkpoints) > 1) {
          p0 <- p0 + ggplot2::ggtitle(label = bquote(PCA == .(round(pa, 2)) ~ m ^
                                                       2),
                                      subtitle = bquote(ACV == .(round(av, 2))
                                                        ~ m ^ 3)) +
            ggplot2::theme(text = ggplot2::element_text(size = 10))
        } else {
          p0 <- p0 + ggplot2::ggtitle(label = bquote(PA == .(round(pa, 2)) ~ m ^ 2),
                                      subtitle = bquote(AV == .(round(av, 2))
                                                        ~ m ^ 3)) +
            ggplot2::theme(text = ggplot2::element_text(size = 10))
        }
        if (buttress) {
          p <- dab_out$plot +
            ggplot2::theme(text = ggplot2::element_text(size = 10))
        } else {
          p <- dbh_out$plot +
            ggplot2::theme(text = ggplot2::element_text(size = 10))
        }
        p1 <-
          ggpubr::ggarrange(p,
                            p0,
                            nrow = 2,
                            ncol = 1,
                            widths = c(1, 1))
        if (crown) {
          p2 <- ggpubr::ggarrange(
            classify_out$plotXZ,
            classify_out$plotYZ,
            nrow = 1,
            ncol = 2,
            heights = c(1, 1),
            common.legend = TRUE,
            align = "hv",
            legend = "bottom"
          )
          p2 <- ggpubr::annotate_figure(p2, top = ggpubr::text_grob(paste(
            "H = ", as.character(round(h, 2)), " m", sep = ""
          )))
        } else {
          p2 <- ggpubr::ggarrange(
            h_out$plotXZ,
            h_out$plotYZ,
            nrow = 1,
            ncol = 2,
            heights = c(1, 1),
            common.legend = TRUE,
            align = "hv",
            legend = "bottom"
          )
          p2 <- ggpubr::annotate_figure(p2, top = ggpubr::text_grob(paste(
            "H = ", as.character(round(h, 2)), " m", sep = ""
          )))
        }
        p3 <-
          ggpubr::ggarrange(p2,
                            p1,
                            nrow = 1,
                            ncol = 2,
                            align = "hv")
        p3 <- ggpubr::annotate_figure(p3, top = ggpubr::text_grob(paste(
          "Summary ", strsplit(filenames[i], extension)[[1]], sep = ""
        ), face = "bold"))
      }
      if (is.character(OUT_path)) {
        file <- paste(OUT_path, ".csv", sep = "")
        utils::write.csv(trees, file, row.names = FALSE)
        if (plot) {
          file_plot <- paste(OUT_path,
                             "_fig_",
                             strsplit(filenames[i], extension)[[1]],
                             ".jpeg",
                             sep = "")
          grDevices::jpeg(
            file = file_plot,
            res = 600,
            width = 4800,
            height = 3000
          )
          print(p3)
          grDevices::dev.off()
        }
      }
      gc()
    }
    return(trees)
  }


#' Summary structural metrics from QSMs
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
#'   treeQSMs. These files have to be of the format xxx_qsm.mat (xxx is the
#'   unique tree id) or xxx_qsm_0.mat (0 at the end is for example the n-th QSM
#'   that is made for tree xxx). Multiple QSMs can be present in one QSM file,
#'   in this case set parameter multiple TRUE. When multiple QSMs are present
#'   for one tree the mean of the values of the different QSMs is taken for that
#'   tree as a final value for a certain feature.
#' @param version A character indicating the version of TreeQSM that was used to
#'   produce the QSMs (Default = "2.4.1"). Other possible versions are "2.4.0",
#'   "2.0", "2.3.0", "2.3.1" and "2.3.2".
#' @param multiple Logical (default = FALSE), indicates if a single .mat file
#'   for one tree holds multiple QSMs at once.
#' @param sbr_normalisation Character (default="treeheight"). Normalisation
#'   parameter of \code{\link{stem_branch_radius_qsm}}.
#' @param sbl_normalisation Character (default="treeheight"). Normalisation
#'   parameter of \code{\link{stem_branch_length_qsm}}.
#' @param sbd_normalisation Character (default="no"). Normalisation parameter of
#'   \code{\link{stem_branch_distance_qsm}}.
#' @param cylindercutoff This is the cutoff radius in meters for which cylinders
#'   are to be included in the total tree volume calculation. Default of 0
#'   includes all cylinders.
#' @param PCs_path A character with the path to the folder that contains the
#'   tree point clouds. Default is NA when the point clouds are not available.
#'   The point clouds are used to determine the DBH, tree height, projected
#'   crown area and crown volume. The DBH and tree height obtained from the tree
#'   point clouds are then used for the normalisation of the other features. The
#'   point cloud files have to be of the format xxx_pc in order to link the tree
#'   point cloud to its' respective treeQSM.
#' @param extension A character refering to the file extension of the point
#'   cloud files (default=".txt"). Can be ".txt", ".ply" or ".las". Only
#'   relevant if the tree point clouds are available.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses. Only relevant if the tree point clouds are available. Only
#'   relevant if the tree point clouds are available.
#' @param thresholdR2 Numeric value (default=0.001). Parameter of the
#'   \code{\link{dbh_pc}} function used to calculate the diameter at breast
#'   height. Only relevant if the tree point cloud is available and buttress ==
#'   FALSE.
#' @param slice_thickness Numeric value (default = 0.06). Parameter of the
#'   \code{\link{dbh_pc}} and \code{\link{dab_pc}} functions used to calculate
#'   the diameter at breast height and above buttresses. Only relevant if the
#'   tree point cloud is available.
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant if the tree point clouds are available and
#'   buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant if the tree point clouds are available and
#'   buttress == TRUE.
#' @param concavity Numeric value (default=4) concavity for the computation of
#'   the functional diameter using a concave hull based on
#'   \code{\link[concaveman]{concaveman}}. This concavity value is used in the
#'   functions \code{\link{dbh}}, \code{\link{stem_branch_length_qsm}},
#'   \code{\link{stem_branch_distance_qsm}}, \code{\link{dbh_height_ratio_qsm}},
#'   \code{\link{dbh_volume_ratio_qsm}}, and
#'   \code{\link{dbh_minradius_ratio_qsm}}. Only relevant if the tree point
#'   cloud is available.
#' @param dtm The digital terrain model from \code{\link{tree_height_pc}}.
#' @param r Numeric value (default=5) r which determines the range taken for the
#'   dtm from \code{\link{tree_height_pc}}. Only relevant if a dtm is provided.
#' @param OUT_path A character with name of the output file (including the path
#'   to the folder), where the summary csv file should be saved or logical
#'   (default=FALSE) in this case no csv file is produced.
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
#' summary <- summary_qsm_metrics(
#'   QSMs_path = "path/to/folder/QSMs/",
#'   OUT_path = "path/to/out/folder/"
#' )
#' # also using point cloud info
#' summary <- summary_qsm_metrics(
#'   QSMs_path = "path/to/folder/QSMs/",
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt",
#'   OUT_path = "path/to/out/folder/"
#' )
#' # Calculate the summary with non-default parameter values
#' # recommended for buttressed trees
#' summary <- summary_qsm_metrics(
#'   QSMs_path = "path/to/folder/QSMs/",
#'   PCs_path = "path/to/folder/PCs/",
#'   extension = ".txt", buttress = TRUE,
#'   OUT_path = "path/to/out/folder/"
#' )
#' }
summary_qsm_metrics <-
  function(QSMs_path,
           version = "2.4.1",
           multiple = FALSE,
           sbr_normalisation = "treeheight",
           sbl_normalisation = "treeheight",
           sbd_normalisation = "no",
           cylindercutoff = 0,
           PCs_path = NA,
           extension = ".txt",
           buttress = FALSE,
           thresholdR2 = 0.001,
           slice_thickness = 0.06,
           thresholdbuttress = 0.001,
           maxbuttressheight = 7,
           concavity = 4,
           dtm = NA,
           r = 5,
           OUT_path = FALSE) {
    filenames <-
      list.files(QSMs_path, pattern = "*.mat", full.names = FALSE)
    unique_tree_ids <- c()
    tree_ids <- c()
    for (i in 1:length(filenames)) {
      id <- strsplit(filenames[i], "_qsm")[[1]][1]
      if (!(id %in% tree_ids)) {
        unique_tree_ids <- append(unique_tree_ids, id)
      }
      tree_ids <- append(tree_ids, id)
    }
    df <- results <- data.frame(
      "X_position" = double(),
      "Y_position" = double(),
      "dbh_m" = double(),
      "tree_height_m" = double(),
      "tree_vol_L" = double(),
      "trunk_vol_L" = double(),
      "branch_len" = double(),
      "trunk_h" = double(),
      "sba_degrees" = double(),
      "sbcs" = double(),
      "sbr" = double(),
      "sbl" = double(),
      "sbd" = double(),
      "dhr" = double(),
      "dvr_m-2" = double(),
      "vb55" = double(),
      "clvr_m-2" = double(),
      "sr" = double(),
      "bar" = double(),
      "rvr" = double(),
      "csh" = double(),
      "ch" = double(),
      "ce" = double(),
      "cdhr" = double(),
      "dmr" = double()
    )
    summary <- summary_means <- summary_sds <- cbind(tree_id = character(), results)
    for (i in 1:length(unique_tree_ids)) {
      print(paste("processing ", unique_tree_ids[i]))
      qsms <- filenames[tree_ids == unique_tree_ids[i]]
      if (!is.na(PCs_path)) {
        pc <-
          read_tree_pc(paste(PCs_path, unique_tree_ids[i], "_pc", extension, sep = ""))
      } else {
        pc <- NA
      }
      trees <- df
      id <- unique_tree_ids[i]
      if (multiple) {
        all_qsms <-
          read_tree_qsm(paste(QSMs_path, qsms[1], sep = ""), version)
        qsms <- all_qsms
      }
      for (j in 1:length(qsms)) {
        print(paste("processing ", unique_tree_ids[i], as.character(j)))
        if (multiple) {
          qsm <- qsms[[j]]
        } else {
          qsm <- read_tree_qsm(paste(QSMs_path, qsms[j], sep = ""), version)
        }
        position <- tree_position_qsm(qsm$cylinder)
        X_position <- position[1]
        Y_position <- position[2]
        dbh <- dbh(
          treedata = qsm$treedata,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        tree_height <- tree_height(
          treedata = qsm$treedata,
          pc = pc,
          dtm = dtm,
          r = r
        )
        tree_vol <- tree_volume_qsm(
          treedata = qsm$treedata,
          cylinder = qsm$cylinder,
          cylindercutoff = cylindercutoff
        )
        trunk_vol <- trunk_volume_qsm(treedata = qsm$treedata)
        branch_len <- total_branch_length_qsm(treedata = qsm$treedata)
        trunk_height <- trunk_height_qsm(treedata = qsm$treedata)
        sba <- stem_branch_angle_qsm(branch = qsm$branch)
        sbcs <- stem_branch_cluster_size_qsm(cylinder = qsm$cylinder)
        sbr <- stem_branch_radius_qsm(
          cylinder = qsm$cylinder,
          treedata = qsm$treedata,
          normalisation = sbr_normalisation,
          pc = pc,
          dtm = dtm,
          r = r
        )
        sbl <- stem_branch_length_qsm(
          branch = qsm$branch,
          treedata = qsm$treedata,
          normalisation = sbl_normalisation,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        sbd <- stem_branch_distance_qsm(
          cylinder = qsm$cylinder,
          treedata = qsm$treedata,
          normalisation = sbd_normalisation,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        dhr <- dbh_height_ratio_qsm(
          treedata = qsm$treedata,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        dvr <- dbh_volume_ratio_qsm(
          treedata = qsm$treedata,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        vb55 <- volume_below_55_qsm(cylinder = qsm$cylinder,
                                    treedata = qsm$treedata)
        clvr <- cylinder_length_volume_ratio_qsm(treedata = qsm$treedata)
        sr <- shedding_ratio_qsm(
          branch = qsm$branch,
          cylinder = qsm$cylinder,
          treedata = qsm$treedata
        )
        bar <- branch_angle_ratio_qsm(branch = qsm$branch)
        rvr <- relative_volume_ratio_qsm(cylinder = qsm$cylinder,
                                         treedata = qsm$treedata)
        csh <- crown_start_height_qsm(
          treedata = qsm$treedata,
          cylinder = qsm$cylinder,
          pc = pc,
          dtm = dtm,
          r = r
        )
        ch <- crown_height_qsm(
          treedata = qsm$treedata,
          cylinder = qsm$cylinder,
          pc = pc,
          dtm = dtm,
          r = r
        )
        ce <- crown_evenness_qsm(cylinder = qsm$cylinder)
        cdhr <-
          crown_diameterheight_ratio_qsm(
            treedata = qsm$treedata,
            cylinder = qsm$cylinder,
            pc = pc,
            dtm = dtm,
            r = r
          )
        dmr <- dbh_minradius_ratio_qsm(
          treedata = qsm$treedata,
          cylinder = qsm$cylinder,
          pc = pc,
          buttress = buttress,
          thresholdR2 = thresholdR2,
          slice_thickness = slice_thickness,
          thresholdbuttress = thresholdbuttress,
          maxbuttressheight = maxbuttressheight,
          concavity = concavity,
          dtm = dtm,
          r = r
        )
        tree <- data.frame(
          "X_position" = X_position,
          "Y_position" = Y_position,
          "dbh_m" = dbh,
          "tree_height_m" = tree_height,
          "tree_vol_L" = tree_vol,
          "trunk_vol_L" = trunk_vol,
          "branch_len" = branch_len,
          "trunk_h" = trunk_height,
          "sba_degrees" = sba,
          "sbcs" = sbcs,
          "sbr" = sbr,
          "sbl" = sbl,
          "sbd" = sbd,
          "dhr" = dhr,
          "dvr_m-2" = dvr,
          "vb55" = vb55,
          "clvr_m-2" = clvr,
          "sr" = sr,
          "bar" = bar,
          "rvr" = rvr,
          "csh" = csh,
          "ch" = ch,
          "ce" = ce,
          "cdhr" = cdhr,
          "dmr" = dmr
        )
        trees <- rbind(trees, tree)
      }
      results <- cbind(tree_id = id, trees)
      summary <- rbind(summary, results)
      if (length(qsms) > 1) {
        m <- as.data.frame.list(colMeans(trees))
        s <-
          as.data.frame.list(sapply(trees, stats::sd, na.rm = TRUE))
        results_means <- cbind(tree_id = id, m)
        results_sds <- cbind(tree_id = id, s)
        summary_means <- rbind(summary_means, results_means)
        summary_sds <- rbind(summary_sds, results_sds)
        summaries <- list("summary" = summary,
                          "means" = summary_means,
                          "sds" = summary_sds)
      } else {
        summaries <- summary
      }
      if (is.character(OUT_path)) {
        file <- paste(OUT_path, ".csv", sep = "")
        utils::write.csv(summary, file, row.names = FALSE)
        if (length(qsms) > 1) {
          file_means <- paste(OUT_path, "_means.csv", sep = "")
          file_sds <- paste(OUT_path, "_sds.csv", sep = "")
          utils::write.csv(summary_means, file_means, row.names = FALSE)
          utils::write.csv(summary_sds, file_sds, row.names = FALSE)
        }
      }
    }
    return(summaries)
  }

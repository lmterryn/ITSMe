#' Read a TreeQSM
#'
#' Reads a TreeQSM matlab file (.mat) and returns its' components in a list and
#' optionally saves the TreeQSM components into the global environment.
#'
#' Initial reading of the .mat file uses \code{\link[R.matlab]{readMat}}.
#' TreeQSM versions older than 2.4.0 are converted to the 2.4.0 structure, where
#' the value of attributes not found in the older version is set NA.
#'
#' @param path A character with the path to the TreeQSM mat file. This file
#'   contains one QSM produced with
#'   \url{https://github.com/InverseTampere/TreeQSM} in matlab.
#' @param version A character indicating the version of TreeQSM that was used to
#'   produce the TreeQSMs. Default version is "2.4.0" Other possible versions
#'   are "2.0", "2.3.0", "2.3.1" and "2.3.2".
#' @param global Logical (default=FALSE), indicates if TreeQSM components should
#'   be read into the global environment.
#'
#' @return Returns a list with the TreeQSM components (cylinder, branch,
#'   treedata and triangulation) and optionally (global=TRUE) saves them into
#'   the global environment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a TreeQSM.mat file with default settings
#' qsm <- read_tree_qsm(QSM_path = "path/to/TreeQSM.mat")
#' # Read a TreeQSM.mat file of version 2.0
#' qsm <- read_tree_qsm(QSM_path = "path/to/TreeQSM.mat", version = "2.0")
#' # Read a TreeQSM.mat file with into the global environment
#' qsm <- read_tree_qsm(QSM_path = "path/to/TreeQSM.mat", global = TRUE)
#' }
read_tree_qsm <- function(path, version = "2.4.0", global = FALSE) {
  tree <- R.matlab::readMat(path)
  if (version == "2.0") {
    branch <- list(
      "order" = tree$BOrd, "parent" = tree$BPar, "diameter" = NA,
      "volume" = tree$BVol, "area" = NA, "length" = tree$BLen,
      "angle" = tree$BAng, "height" = NA, "azimuth" = NA,
      "zenith" = NA
    )
    cylinder <- list(
      "radius" = tree$Rad, "length" = tree$Len,
      "start" = tree$Sta, "axis" = tree$Axe,
      "parent" = tree$CPar, "extension" = tree$CExt,
      "added" = NA, "UnmodRadius" = NA, "branch" = tree$BoC[, 1],
      "SurfCov" = NA, "mad" = NA, "BranchOrder" = tree$BoC[, 2],
      "PositionInBranch" = tree$BoC[, 3]
    )
    treedata <- list(
      "TotalVolume" = tree$TreeData[1],
      "TrunkVolume" = tree$TreeData[2],
      "BranchVolume" = tree$TreeData[3],
      "TreeHeight" = tree$TreeData[4],
      "TrunkLength" = tree$TreeData[5],
      "BranchLength" = tree$TreeData[6], "TotalLength" = NA,
      "NumberBranches" = tree$TreeData[7],
      "MaxBranchOrder" = tree$TreeData[8], "TrunkArea" = NA,
      "BranchArea" = NA, "TotalArea" = tree$TreeData[9],
      "DBHqsm" = tree$TreeData[10], "DBHcyl" = NA,
      "CrownDiamAve" = NA, "CrownDiamMax" = NA,
      "CrownAreaConv" = NA, "CrownAreaAlpha" = NA,
      "CrownBaseHeight" = NA, "CrownLength" = NA,
      "CrownRatio" = NA, "CrownVolumeConv" = NA,
      "CrownVolumeAlpha" = NA, "location" = NA, "StemTaper" = NA,
      "VerticalProfile" = NA, "spreads" = NA, "VolCylDia" = NA,
      "AreCylDia" = NA, "LenCylDia" = NA, "VolCylHei" = NA,
      "AreCylHei" = NA, "LenCylHei" = NA, "VolCylZen" = NA,
      "AreCylZen" = NA, "LenCylZen" = NA, "VolCylAzi" = NA,
      "AreCylAzi" = NA, "LenCylAzi" = NA, "VolBranchOrd" = NA,
      "AreBranchOrd" = NA, "LenBranchOrd" = NA,
      "NumBranchOrd" = NA, "VolBranchDia" = NA,
      "VolBranch1Dia" = NA, "AreBranchDia" = NA,
      "AreBranch1Dia" = NA, "LenBranchDia" = NA,
      "LenBranch1Dia" = NA, "NumBranchDia" = NA,
      "NumBranch1Dia" = NA, "VolBranchHei" = NA,
      "VolBranch1Hei" = NA, "AreBranchHei" = NA,
      "AreBranch1Hei" = NA, "LenBranchHei" = NA,
      "LenBranch1Hei" = NA, "NumBranchHei" = NA,
      "NumBranch1Hei" = NA, "VolBranchAng" = NA,
      "VolBranch1Ang" = NA, "AreBranchAng" = NA,
      "AreBranch1Ang" = NA, "LenBranchAng" = NA,
      "LenBranch1Ang" = NA, "NumBranchAng" = NA,
      "NumBranch1Ang" = NA, "VolBranchAzi" = NA,
      "VolBranch1Azi" = NA, "AreBranchAzi" = NA,
      "AreBranch1Azi" = NA, "LenBranchAzi" = NA,
      "LenBranch1Azi" = NA, "NumBranchAzi" = NA,
      "NumBranch1Azi" = NA, "VolBranchZen" = NA,
      "VolBranch1Zen" = NA, "AreBranchZen" = NA,
      "AreBranch1Zen" = NA, "LenBranchZen" = NA,
      "LenBranch1Zen" = NA, "NumBranchZen" = NA,
      "NumBranch1Zen" = NA
    )
    triangulation <- list(
      "vert" = NA, "facet" = NA, "fvd" = NA, "volume" = NA,
      "SideArea" = NA, "BottomArea" = NA, "TopArea" = NA,
      "bottom" = NA, "top" = NA, "triah" = NA, "triaw" = NA,
      "cylind" = NA
    )
  } else if (version != "2.4.0") {
    if (length(tree) == 1) {
      names(tree) <- c("qsm")
    }
    qsm <- tree$qsm
    names(qsm) <- rownames(qsm)
    c <- qsm$cylinder
    cylinder <- array(append(c[, , 1], c("SurfCov" = NA, "mad" = NA)))
    names(cylinder) <- append(rownames(c), c("SurfCov", "mad"))
    b <- qsm$branch
    branch <- array(append(b[, , 1], c("area" = NA, "zenith" = NA)))
    names(branch) <- append(rownames(b), c("area", "zenith"))
    t <- qsm$treedata
    if (length(t) == 18) {
      treedata <- array(append(t[, , 1], replicate(65, NA)))
      names(treedata) <- append(
        rownames(t)[1:13],
        c(
          "VolCylDia", "LenCylDia", "VolBranchOrd",
          "LenBranchOrd", "NumBranchOrd", "TotalLength",
          "TrunkArea", "BranchArea", "CrownDiamAve",
          "CrownDiamMax", "CrownAreaConv",
          "CrownAreaAlpha", "CrownBaseHeight",
          "CrownLength", "CrownRatio", "CrownVolumeConv",
          "CrownVolumeAlpha", "VerticalProfile",
          "spreads", "AreCylDia", "VolCylHei",
          "AreCylHei", "LenCylHei", "VolCylZen",
          "AreCylZen", "LenCylZen", "VolCylAzi",
          "AreCylAzi", "LenCylAzi", "AreBranchOrd",
          "VolBranchDia", "VolBranch1Dia",
          "AreBranchDia", "AreBranch1Dia",
          "LenBranchDia", "LenBranch1Dia",
          "NumBranchDia", "NumBranch1Dia",
          "VolBranchHei", "VolBranch1Hei",
          "AreBranchHei", "AreBranch1Hei",
          "LenBranchHei", "LenBranch1Hei",
          "NumBranchHei", "NumBranch1Hei",
          "VolBranchAng", "VolBranch1Ang",
          "AreBranchAng", "AreBranch1Ang",
          "LenBranchAng", "LenBranch1Ang",
          "NumBranchAng", "NumBranch1Ang",
          "VolBranchAzi", "VolBranch1Azi",
          "AreBranchAzi", "AreBranch1Azi",
          "LenBranchAzi", "LenBranch1Azi",
          "NumBranchAzi", "NumBranch1Azi",
          "VolBranchZen", "VolBranch1Zen",
          "AreBranchZen", "AreBranch1Zen",
          "LenBranchZen", "LenBranch1Zen",
          "NumBranchZen", "NumBranch1Zen"
        )
      )
    } else {
      treedata <- array(append(t[, , 1], replicate(68, NA)))
      names(treedata) <- append(
        rownames(t)[1:18],
        c(
          "VolCylDia", "LenCylDia", "VolBranchOrd",
          "LenBranchOrd", "NumBranchOrd", "TotalLength",
          "TrunkArea", "BranchArea", "CrownDiamAve",
          "CrownDiamMax", "CrownAreaConv",
          "CrownAreaAlpha", "CrownBaseHeight",
          "CrownLength", "CrownRatio", "CrownVolumeConv",
          "CrownVolumeAlpha", "VerticalProfile",
          "spreads", "AreCylDia", "VolCylHei",
          "AreCylHei", "LenCylHei", "VolCylZen",
          "AreCylZen", "LenCylZen", "VolCylAzi",
          "AreCylAzi", "LenCylAzi", "AreBranchOrd",
          "VolBranchDia", "VolBranch1Dia",
          "AreBranchDia", "AreBranch1Dia",
          "LenBranchDia", "LenBranch1Dia",
          "NumBranchDia", "NumBranch1Dia",
          "VolBranchHei", "VolBranch1Hei",
          "AreBranchHei", "AreBranch1Hei",
          "LenBranchHei", "LenBranch1Hei",
          "NumBranchHei", "NumBranch1Hei",
          "VolBranchAng", "VolBranch1Ang",
          "AreBranchAng", "AreBranch1Ang",
          "LenBranchAng", "LenBranch1Ang",
          "NumBranchAng", "NumBranch1Ang",
          "VolBranchAzi", "VolBranch1Azi",
          "AreBranchAzi", "AreBranch1Azi",
          "LenBranchAzi", "LenBranch1Azi",
          "NumBranchAzi", "NumBranch1Azi",
          "VolBranchZen", "VolBranch1Zen",
          "AreBranchZen", "AreBranch1Zen",
          "LenBranchZen", "LenBranch1Zen",
          "NumBranchZen", "NumBranch1Zen",
          "TriaTrunkArea", "MixTrunkArea",
          "MixTotalArea"
        )
      )
    }
    tria <- qsm$triangulation
    triangulation <- array(append(tria[, , 1], c(
      "SideArea" = NA, "BottomArea" = NA,
      "TopArea" = NA
    )))
    names(triangulation) <- append(rownames(tria), c(
      "SideArea", "BottomArea",
      "TopArea"
    ))
  } else {
    if (length(tree) == 1) {
      names(tree) <- c("qsm")
    }
    qsm <- tree$qsm
    names(qsm) <- rownames(qsm)
    cylinder <- qsm$cylinder
    names(cylinder) <- rownames(cylinder)
    branch <- qsm$branch
    names(branch) <- rownames(branch)
    treedata <- qsm$treedata
    names(treedata) <- rownames(treedata)
    triangulation <- qsm$triangulation
    names(triangulation) <- rownames(triangulation)
  }
  out <- list(
    cylinder = cylinder, branch = branch,
    treedata = treedata, triangulation = triangulation
  )
  if (global) {
    list2env(out, envir = .GlobalEnv)
    r <- print("QSM has been read")
  } else {
    r <- out
  }
  return(r)
}

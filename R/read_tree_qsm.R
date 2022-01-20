#' Read a TreeQSM
#'
#' Reads a TreeQSM matlab file (.mat) and returns its' components in a list and
#' optionally saves the TreeQSM components into the global environment.
#'
#' Initial reading of the .mat file uses \code{\link[R.matlab]{readMat}}.
#'
#' @param path The path to the TreeQSM mat file. This file contains 1 QSM
#'   produced with version 2.4.0 of TreeQSM
#'   \url{https://github.com/InverseTampere/TreeQSM} in matlab.
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
#' QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
#' qsm <- read_tree_qsm(QSM_path)
read_tree_qsm <- function(path,global=FALSE) {
  tree <- R.matlab::readMat(path)
  names(tree) <- c("qsm")
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
  out <- list(cylinder=cylinder,branch=branch,
              treedata=treedata,triangulation=triangulation)
  if (global){
    list2env(out, envir = .GlobalEnv)
    r <- print("QSM has been read")
  } else {
    r <- out
  }
  return(r)
}

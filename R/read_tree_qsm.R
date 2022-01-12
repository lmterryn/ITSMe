#' Read a TreeQSM
#'
#' @param path The path to the TreeQSM mat file. This file contains 1 QSM
#' produced with version 2.4.0 of TreeQSM (https://github.com/InverseTampere/TreeQSM) in matlab.
#'
#' @return reads the QSM (tree) and its' components (qsm, cylinder, branch and treedata) into the global environement.
#' @export
#'
#' @examples
#' #QSM_path <- "path/to/test_qsm.txt"
#' #read_tree_qsm <- function(QSM_path)
read_tree_qsm <- function(path) { #input: path to QSM data; output: data from QSM
  tree <- R.matlab::readMat(path)
  qsm <- tree$qsm
  names(qsm) <- rownames(qsm)
  cylinder <- qsm$cylinder
  names(cylinder) <- rownames(cylinder)
  branch <- qsm$branch
  names(branch) <- rownames(branch)
  treedata <- qsm$treedata
  names(treedata) <- rownames(treedata)
  out <- list(tree,qsm,cylinder,branch,treedata)
  list2env(stats::setNames(out, c("tree", "qsm", "cylinder", "branch", "treedata")), envir = .GlobalEnv)
  return(print("QSM has been read"))
}

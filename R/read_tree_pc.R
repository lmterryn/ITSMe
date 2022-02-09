#' Read a tree point cloud
#'
#' Reads a tree point cloud file of txt, las or ply format and returns the tree
#' point cloud as a data.frame with 3 columns (X,Y,Z).
#'
#' Reading the txt, las and ply files is based on
#' \code{\link[data.table]{fread}}, \code{\link[lidR]{readTLSLAS}} and
#' \code{\link[Rvcg]{vcgPlyRead}} respectively. Sampling is based on
#' \code{\link[base]{sample}} and is mainly a useful tool to reduce the amount
#' of points for quicker plotting.
#'
#' @param path A character with the path to the tree point cloud file. File can
#'   be \emph{txt}, \emph{las} or \emph{ply} format. The 3D coordinates have to
#'   be in the first three columns and field separator needs to be whitespace
#'   (that is one or more spaces, tabs, newlines or carriage returns) in case of
#'   a \emph{txt} file.
#' @param samplefactor A numeric value ranging from 0 to 1 (default=1). This
#'   determines the amount of points that are sampled from the point cloud. 1 to
#'   sample 100 percent of the points and for example 0.5 to sample 50 percent
#'   of the points.
#'
#' @return The tree point cloud as a data.frame (nx3) with X, Y, and Z
#'   coordinates in the first, second and third column respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' PC_path <- "path/to/point_cloud.txt"
#' PC_path <- "path/to/point_cloud.ply"
#' PC_path <- "path/to/point_cloud.las"
#' pc <- read_tree_pc(PC_path)
#' pc <- read_tree_pc(PC_path,0.2)
#' }
read_tree_pc <- function(path, samplefactor=1) {
  extension <- utils::tail(strsplit(path, split=".", fixed=T)[[1]],1)
  if(extension == "txt") {
    txt <- data.table::fread(path)
    pc <- data.frame(txt[,1:3])
    colnames(pc) <- c("X","Y","Z")
  } else if(extension == "las") {
    las <- lidR::readTLSLAS(path)
    pc <- data.frame("X" = las$X, "Y" = las$Y, "Z" = las$Z)
  } else if(extension =="ply") {
    ply <- Rvcg::vcgPlyRead(path, updateNormals = TRUE, clean = TRUE)
    pc <- data.frame("X" = ply$vb[1,], "Y" = ply$vb[2,], "Z" = ply$vb[3,])
  } else {
    print("This extension is not recognized. Try txt, las or ply format")
  }
  if(samplefactor != 1) {
    pc <- pc[sample(nrow(pc), size = floor(nrow(pc)*samplefactor),
                    replace = FALSE, prob = NULL), ]
  }
  return(pc)
}


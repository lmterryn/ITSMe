#' Read a RCT QSM
#'
#' Reads a RayCloudTools (RCT) QSM txt file (.txt) generated using rayextract trees and treeinfo.
#' This function returns its components in a list and optionally saves the RCT QSM components into
#' the global environment. It can handle a file including one or multiple RCT QSMs.
#'
#' The input file contains the following fields:
#' Tree-level attributes:
#' - height: Total height of the tree in meters
#' - crown_radius: Approximate radius of the tree crown, calculated as mean of bounding box extents
#' - dimension: Fractal dimension of branch lengths (capped at 3.0)
#' - monocotal: Metric indicating how strongly the tree resembles a palm tree
#' - DBH: Diameter at breast height
#' - bend: Trunk bend metric (standard deviation from straight line divided by length)
#' - branch_slope: Slope characteristics of the branches
#'
#' Segment-level attributes:
#' - volume: Volume of the segment in cubic meters (for root segment: total tree volume)
#' - diameter: Segment diameter in meters (for root: maximum diameter in tree)
#' - length: Length from segment base to farthest leaf
#' - strength: Structural strength metric calculated as diameter^0.75/length
#' - min_strength: Minimum strength value between this segment and root
#' - dominance: Branch dominance ratio (a1/(a1+a2) for largest child branches)
#' - angle: Branch angle at bifurcation points (for root: mean branch angle)
#' - children: Number of child segments (for root: mean for tree)
#'
#' Optional branch data attributes (if treeinfo tree.txt --branch_data flag was used):
#' - branch: Unique branch identifier
#' - branch_order: Branch order number in hierarchy
#' - extension: Extension identifier for continuous branch segments
#' - pos_in_branch: Position index within the branch
#' - segment_length: Length of individual segment
#'
#' @param path A character with the path to the RCT QSM txt file.
#' @param global Logical (default=FALSE), indicates if RCT QSM components should be read into global environment.
#' @return Returns a list with the RCT QSM components (cylinder and treedata).
#' @export
#'
#' @examples
#' \dontrun{
#' qsm <- read_rct_qsm(path = "path/to/RCTQSM.txt")
#' cylinder_data <- qsm$cylinder
#' }
read_rct_qsm <- function(path, global = FALSE) {
  # Input validation
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }

  # Read the file contents
  lines <- readLines(path)
  if (length(lines) < 3) {
    stop("Invalid file format: file must contain at least 3 lines")
  }

  # Process headers (line 2)
  header_parts <- strsplit(trimws(lines[2]), " ")[[1]]
  # Extract tree and cylinder headers
  tree_headers <- unlist(strsplit(header_parts[1], ","))
  tree_headers <- tree_headers[tree_headers != ""] # Remove empty entries

  cylinder_parts <- paste(header_parts[-1], collapse = " ")
  cylinder_headers <- unlist(strsplit(cylinder_parts, ","))
  cylinder_headers <- cylinder_headers[cylinder_headers != ""]

  out <- list()
  names_out <- c()
  for (i in 3:length(lines)){
    # Process data (line 3)
    data_line <- trimws(lines[i])
    data_parts <- strsplit(data_line, " ")[[1]]

    # Process tree data (first segment)
    tree_data <- as.numeric(unlist(strsplit(data_parts[1], ",")))
    tree_data <- tree_data[!is.na(tree_data)]
    names(tree_data) <- tree_headers

    # Process cylinder data (remaining segments)
    cylinder_segments <- data_parts[-1]
    cylinder_data <- lapply(cylinder_segments, function(segment) {
      values <- as.numeric(unlist(strsplit(gsub(",$", "", segment), ",")))
      values[is.na(values)] <- 0 # Replace NA with 0
      return(values)
    })

    # Convert cylinder data to matrix then data frame
    cylinder_matrix <- do.call(rbind, cylinder_data)
    colnames(cylinder_matrix) <- cylinder_headers
    cylinder_df <- as.data.frame(cylinder_matrix)

    # Filter out root segments (parent_id = -1)
    cylinder_df <- cylinder_df[cylinder_df$parent_id != -1, ]

    # Create cylinder component list with geometric and structural metrics
    cylinder <- list(
      "end" = cbind(cylinder_df$x, cylinder_df$y, cylinder_df$z), # 3D coordinates of segment end
      "radius" = cylinder_df$radius, # Segment radius in meters
      "parent" = cylinder_df$parent_id + 1, # ID of parent segment (adjusted to 1-based indexing)
      "section" = cylinder_df$section_id + 1, # Section identifier
      "volume" = cylinder_df$volume, # Segment volume in cubic meters
      "diameter" = cylinder_df$diameter, # Segment diameter in meters
      "length" = cylinder_df$length, # Length to farthest leaf
      "strength" = cylinder_df$strength, # Structural strength (d^0.75/l)
      "min_strength" = cylinder_df$min_strength, # Minimum strength to root
      "dominance" = cylinder_df$dominance, # Branch dominance ratio
      "angle" = cylinder_df$angle, # Branch angle at bifurcation
      "children" = cylinder_df$children, # Number of child segments
      "branch" = cylinder_df$branch, # Branch identifier
      "BranchOrder" = cylinder_df$branch_order, # Hierarchical branch order
      "extension" = cylinder_df$extension, # Branch extension identifier
      "PositionInBranch" = cylinder_df$pos_in_branch, # Position within branch
      "segment_length" = cylinder_df$segment_length # Individual segment length
    )

    # Create treedata component list with tree-level metrics
    treedata <- list(
      "TotalVolume" = sum(cylinder_df$volume) * 1000, # Total tree volume converted to litres
      "TreeHeight" = unname(tree_data["height"]), # Total tree height in meters
      "DBHqsm" = unname(tree_data["DBH"]), # Diameter at breast height
      "CrownRadius" = unname(tree_data["crown_radius"]), # Average crown radius
      "Dimension" = unname(tree_data["dimension"]), # Fractal dimension of branches
      "Monocotal" = unname(tree_data["monocotal"]), # Palm tree similarity metric
      "Bend" = unname(tree_data["bend"]), # Trunk bend metric
      "BranchSlope" = unname(tree_data["branch_slope"]) # Branch slope characteristics
    )
    # Prepare output
    single_out <- list(
      cylinder = cylinder,
      treedata = treedata
    )
    if (length(lines) != 3){
      out[[length(out) + 1]] <- single_out
      names_out <-
        append(names_out, paste("qsm_", as.character(i-2), sep = ""))
    } else {
      out <- single_out
    }
  }
  names(out) <- names_out
  # Handle global environment option
  if (global) {
    list2env(out, envir = .GlobalEnv)
    invisible(out)
  } else {
    return(out)
  }
}

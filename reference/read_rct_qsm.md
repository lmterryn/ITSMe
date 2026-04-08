# Read a RCT QSM

Reads a RayCloudTools (RCT) QSM txt file (.txt) generated using
rayextract trees and treeinfo. This function returns its components in a
list and optionally saves the RCT QSM components into the global
environment. It can handle a file including one or multiple RCT QSMs.

## Usage

``` r
read_rct_qsm(path, global = FALSE)
```

## Arguments

- path:

  A character with the path to the RCT QSM txt file.

- global:

  Logical (default=FALSE), indicates if RCT QSM components should be
  read into global environment.

## Value

Returns a list with the RCT QSM components (cylinder and treedata).

## Details

The input file contains the following fields: Tree-level attributes:

- height: Total height of the tree in meters

- crown_radius: Approximate radius of the tree crown, calculated as mean
  of bounding box extents

- dimension: Fractal dimension of branch lengths (capped at 3.0)

- monocotal: Metric indicating how strongly the tree resembles a palm
  tree

- DBH: Diameter at breast height

- bend: Trunk bend metric (standard deviation from straight line divided
  by length)

- branch_slope: Slope characteristics of the branches

Segment-level attributes:

- volume: Volume of the segment in cubic meters (for root segment: total
  tree volume)

- diameter: Segment diameter in meters (for root: maximum diameter in
  tree)

- length: Length from segment base to farthest leaf

- strength: Structural strength metric calculated as
  diameter^0.75/length

- min_strength: Minimum strength value between this segment and root

- dominance: Branch dominance ratio (a1/(a1+a2) for largest child
  branches)

- angle: Branch angle at bifurcation points (for root: mean branch
  angle)

- children: Number of child segments (for root: mean for tree)

Optional branch data attributes (if treeinfo tree.txt –branch_data flag
was used):

- branch: Unique branch identifier

- branch_order: Branch order number in hierarchy

- extension: Extension identifier for continuous branch segments

- pos_in_branch: Position index within the branch

- segment_length: Length of individual segment

## Examples

``` r
if (FALSE) { # \dontrun{
qsm <- read_rct_qsm(path = "path/to/RCTQSM.txt")
cylinder_data <- qsm$cylinder
} # }
```

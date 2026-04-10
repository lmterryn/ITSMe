# Read a RCT QSM

Reads a RayCloudTools (RCT) QSM txt file (.txt) generated using
rayextract trees and treeinfo. This function returns its components in a
list and optionally saves the RCT QSM components into the global
environment. It can handle a file including one or multiple RCT QSMs.

## Usage

``` r
read_rct_qsm(path, global = FALSE, remove_root = FALSE)
```

## Arguments

- path:

  A character with the path to the RCT QSM txt file.

- global:

  Logical (default=FALSE), indicates if RCT QSM components should be
  read into global environment.

- remove_root:

  Logical (default=FALSE), indicates if the root segment is removed from
  the data.frame.

## Value

Returns a list of QSMs (e.g. qsm_1, qsm_2, ...), each QSM consists of a
list of 2 components (cylinder and treedata) containing:

- cylinder:

  A list containing cylinder-level attributes:

  end

  :   Matrix (n x 3). 3D coordinates (x, y, z) of the segment end
      points.

  radius

  :   Numeric vector. Segment radius in meters.

  parent

  :   Integer vector. ID of the parent segment (adjusted to 1-based
      indexing). Root segment has index 1; first segment index 2.

  section

  :   Integer vector. Section identifier.

  volume

  :   Numeric vector. Segment volume in cubic meters.

  diameter

  :   Numeric vector. Segment diameter in meters.

  length

  :   Numeric vector. Length to the farthest leaf (converted from cm to
      m).

  strength

  :   Numeric vector. Structural strength (d^0.75 / l).

  min_strength

  :   Numeric vector. Minimum strength to the root.

  dominance

  :   Numeric vector. Branch dominance ratio.

  angle

  :   Numeric vector. Branch angle at bifurcation.

  children

  :   Integer vector. Number of child segments.

  branch

  :   Integer vector. Branch identifier.

  BranchOrder

  :   Integer vector. Hierarchical branch order.

  extension

  :   Integer vector. Branch extension identifier.

  PositionInBranch

  :   Integer vector. Position within the branch.

  segment_length

  :   Numeric vector. Individual segment length.

- treedata:

  A list containing tree-level metrics:

  TotalVolume

  :   Numeric. Total tree volume in litres (sum of cylinder volumes ×
      1000).

  TreeHeight

  :   Numeric. Total tree height in meters.

  DBHqsm

  :   Numeric. Diameter at breast height.

  CrownRadius

  :   Numeric. Average crown radius.

  Dimension

  :   Numeric. Fractal dimension of branches.

  Monocotal

  :   Numeric. Palm tree similarity metric.

  Bend

  :   Numeric. Trunk bend metric.

  BranchSlope

  :   Numeric. Branch slope characteristics.

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

- length: Length from segment base to farthest leaf in m

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

The first row of the cylinder list does **not** represent an actual
cylinder. Instead, it provides the starting x and y coordinates of the
first cylinder in the model. This row should typically be excluded from
analyses that operate on true segment (e.g. volume or length
calculations).

## Examples

``` r
if (FALSE) { # \dontrun{
qsm <- read_rct_qsm(path = "path/to/RCTQSM.txt")
cylinder_data <- qsm$cylinder
} # }
```

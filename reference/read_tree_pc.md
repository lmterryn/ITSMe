# Read a tree point cloud

Reads a tree point cloud file of txt, las/laz or ply format and returns
the tree point cloud as a data.frame with 3 columns (X,Y,Z).

## Usage

``` r
read_tree_pc(path, samplefactor = 1)
```

## Arguments

- path:

  A character with the path to the tree point cloud file. File can be
  *txt*, *las*, *laz* or *ply* format. The 3D coordinates have to be in
  the first three columns in case of a *txt* file.

- samplefactor:

  A numeric value ranging from 0 to 1 (default=1). This determines the
  amount of points that are sampled from the point cloud. 1 to sample
  100 percent of the points and for example 0.5 to sample 50 percent of
  the points.

## Value

The tree point cloud as a data.frame (nx3) with X, Y, and Z coordinates
in the first, second and third column respectively.

## Details

Reading the txt, las/laz and ply files is based on
[`fread`](https://rdrr.io/pkg/data.table/man/fread.html),
[`readTLSLAS`](https://rdrr.io/pkg/lidR/man/deprecated.html) and
[`vcgPlyRead`](https://rdrr.io/pkg/Rvcg/man/vcgPlyRead.html)
respectively. Sampling is based on
[`sample`](https://rdrr.io/r/base/sample.html) and is mainly a useful
tool to reduce the amount of points for quicker plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a tree point cloud file of the txt format
pc_txt <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
# Read a tree point cloud file of the ply format
pc_ply <- read_tree_pc(PC_path = "path/to/point_cloud.ply")
# Read a tree point cloud file of the las/laz format
# and subsample to 20 percent of the points
pc_las <- read_tree_pc(PC_path = "path/to/point_cloud.las", 0.2)
} # }
```

# ITSMe development version

## DBH estimation and quality control

- Added flexible radius summarisation to DBH estimation through the `how`
  argument. The original ITSMe behaviour can be obtained with `how = "mean"`;
  `how = "median"` (new default) uses the median point-to-centre radius; and numeric values
  use a trimmed mean of the point-to-centre radii.
- Added DBH quality-control metrics to `diameter_slice_pc()` and `dbh_pc()`:
  `arc_coverage` and `inner_circle_empty`.
- Updated `summary_basic_pointcloud_metrics_pertree()` and
  `summary_basic_pointcloud_metrics()` so DBH quality-control metrics are
  included in the returned summary data frame when `"stem diameter"` is
  calculated.
- Updated the lower-trunk fallback logic in `dbh_pc()` so that a well-supported
  direct DBH slice is less likely to be overwritten because of noisy or
  incomplete lower-stem points.
- Documented the new DBH estimation options and quality-control metrics in the
  main ITSMe vignette.
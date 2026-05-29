test_that("summary basic point cloud metrics works", {
  out <- summary_basic_pointcloud_metrics(
    PCs_path = "../non_buttressed_trees/",
    pattern = ".txt",
    overwrite = TRUE
  )

  expect_s3_class(out, "data.frame")

  expect_true(all(c(
    "stem_diameter_m",
    "R2",
    "functional_stem_diameter_m",
    "height_stem_diameter_m"
  ) %in% names(out)))

  expect_true(all(c(
    "dbh_arc_coverage",
    "dbh_inner_circle_empty"
  ) %in% names(out)))

  out_buttress <- summary_basic_pointcloud_metrics(
    PCs_path = "../buttressed_trees/",
    buttress = TRUE,
    minheight = 4,
    OUT_path = "../output/",
    pattern = ".txt",
    overwrite = TRUE
  )

  expect_s3_class(out_buttress, "data.frame")

  expect_true(all(c(
    "stem_diameter_m",
    "R2",
    "functional_stem_diameter_m",
    "height_stem_diameter_m"
  ) %in% names(out_buttress)))

  expect_true(all(c(
    "dbh_arc_coverage",
    "dbh_inner_circle_empty"
  ) %in% names(out_buttress)))
})

test_that("summary qsm metrics works", {
  # output is a data frame with length 24
  expect_equal(length(
    summary_qsm_metrics(QSMs_path = "../non_buttressed_trees/single_qsm/")
  ), 36) &
    # with multiple QSMs, output is a data frame with length 3
    expect_equal(length(
      summary_qsm_metrics(QSMs_path = "../non_buttressed_trees/multiple_qsm/", multiple = TRUE)
    ), 3) &
    # with point clouds, buttress TRUE, OUT_path specified,
    # output is a data frame with length 3
    expect_equal(length(
      summary_qsm_metrics(
        QSMs_path = "../buttressed_trees/",
        buttress = TRUE,
        PCs_path = "../buttressed_trees/",
        OUT_path = "../output/summary"
      )
    ), 3)
})

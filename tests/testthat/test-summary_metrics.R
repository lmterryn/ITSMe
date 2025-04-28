test_that("summary basic point cloud metrics works", {
  # output is a data frame with length 8
  expect_equal(length(
    summary_basic_pointcloud_metrics(PCs_path = "../non_buttressed_trees/", pattern = ".txt")
  ), 10) &
    # with buttress TRUE & OUT_path specified, output is a data frame with length 8
    expect_equal(length(
      summary_basic_pointcloud_metrics(
        PCs_path = "../buttressed_trees/",
        buttress = TRUE,
        minheight =  4,
        OUT_path = "../output/",
        pattern = ".txt"
      )
    ), 10) &
    # with crown & plot TRUE, output is a data frame with length 8
    expect_equal(length(
      summary_basic_pointcloud_metrics(
        PCs_path = "../non_buttressed_trees/",
        crown = TRUE,
        plot = TRUE,
        pattern = ".txt"
      )
    ), 10)
})

test_that("summary qsm metrics works", {
  # output is a data frame with length 24
  expect_equal(length(
    summary_qsm_metrics(QSMs_path = "../non_buttressed_trees/single_qsm/")
  ), 26) &
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

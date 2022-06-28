test_that("reading txt tree point cloud works", {
  # output is a data frame with length 3 and more than 0 rows
  expect_equal(length(
    read_tree_pc(path = "../point_clouds/pc_tree.txt")), 3) &
    expect_gt(nrow(
      read_tree_pc(path = "../point_clouds/pc_tree.txt")), 0)
})

test_that("reading ply tree point cloud works", {
  # output is a data frame with length 3 and more than 0 rows
  expect_equal(length(
    read_tree_pc(path = "../point_clouds/pc_tree.ply")), 3) &
    expect_gt(nrow(
      read_tree_pc(path = "../point_clouds/pc_tree.ply")), 0)
})

test_that("reading las tree point cloud works", {
  # output is a data frame with length 3 and more than 0 rows
  expect_equal(length(
    read_tree_pc(path = "../point_clouds/pc_tree.las")), 3) &
    expect_gt(nrow(
      read_tree_pc(path = "../point_clouds/pc_tree.las")), 0)
})

test_that("reading and sampling txt tree point cloud works", {
  # output is a data frame with less rows than originally if sample factor is
  # smaller than 1
  expect_gt(
    nrow(read_tree_pc(path = "../point_clouds/pc_tree.txt", samplefactor = 1)),
    nrow(read_tree_pc(path = "../point_clouds/pc_tree.txt",
                      samplefactor = 0.5)))
})

test_that("reading non txt, ply or las does not work", {
  # output is an empty data frame when a file with unsupported extension is read
  expect_equal(length(
    read_tree_pc(path = "../point_clouds/pc_tree.pcd")), 0)
})

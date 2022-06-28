test_that("tree position calculation works", {
  # output is a list of 3
  expect_equal(length(tree_position_qsm(
    cylinder = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$cylinder)), 2)
})

test_that("total cylinder length extraction works", {
  # output is a numeric
  expect_equal(length(total_cyl_length_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("tree volume extraction works", {
  # output is a numeric
  expect_equal(length(tree_volume_qsm(
    treedata = read_tree_qsm(
      path = "../qsms/qsm_tree_v240_notria.mat")$treedata)), 1)
  # with triangulation, output is a numeric
  expect_equal(length(tree_volume_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("trunk volume extraction works", {
  # output is a numeric
  expect_equal(length(trunk_volume_qsm(
    treedata = read_tree_qsm(
      path = "../qsms/qsm_tree_v240_notria.mat")$treedata)), 1)
  # with triangulation, output is a numeric
  expect_equal(length(trunk_volume_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("total branch volume extraction works", {
  # output is a numeric
  expect_equal(length(total_branch_volume_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("total branch length extraction works", {
  # output is a numeric
  expect_equal(length(total_branch_length_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("tree height extraction works", {
  # output is a numeric
  expect_equal(length(tree_height_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

test_that("dbh extraction works", {
  # output is a numeric
  expect_equal(length(dbh_qsm(
    treedata = read_tree_qsm(
      path = "../qsms/qsm_tree_v240_notria.mat")$treedata)), 1)
  # with triangulation, output is a numeric
  expect_equal(length(dbh_qsm(
    treedata = read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")$treedata)), 1)
})

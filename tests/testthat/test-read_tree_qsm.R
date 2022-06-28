test_that("reading tree qsm version 2.4.0 works", {
  # output is a list with four elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v240_notria.mat")), 4)
})

test_that("reading multiple tree qsms version 2.4.0 works", {
  # output is a list with 10 qsms which each consist of a list of 4 elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsms_tree_v240_notria.mat")), 10) &
    expect_equal(length(
      read_tree_qsm(path = "../qsms/qsms_tree_v240_notria.mat")[[1]]), 4)
})

test_that("reading tree qsm with triangulation version 2.4.0 works", {
  # output is a list with four elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v240.mat")), 4)
})

test_that("reading multiple tree qsms with triangulation version 2.4.0 works", {
  # output is a list with 90 qsms which each consist of a list of 4 elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsms_tree_v240.mat")), 90) &
    expect_equal(length(
      read_tree_qsm(path = "../qsms/qsms_tree_v240.mat")[[1]]), 4)
})

test_that("reading tree qsm version 2.0 works", {
  # output is a list with four elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v200.mat", version = "2.0")), 4)
})

test_that("reading tree qsm version 2.3.0 works", {
  # output is a list with four elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v230_notria.mat",
                  version = "2.3.0")), 4)
})

test_that("reading multiple tree qsms version 2.3.0 works", {
  # output is a list with 4 qsms which each consist of a list of 4 elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsms_tree_v230_notria.mat",
                  version = "2.3.0")), 4) &
    expect_equal(length(
      read_tree_qsm(path = "../qsms/qsms_tree_v230_notria.mat",
                    version = "2.3.0")[[1]]), 4)
})

test_that("reading tree qsm with triangulation version 2.3.0 works", {
  # output is a list with four elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v230.mat", version = "2.3.0")), 4)
})

test_that("reading multiple tree qsms with triangulation version 2.3.0 works", {
  # output is a list with 4 qsms which each consist of a list of 4 elements
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsms_tree_v230.mat", version = "2.3.0")), 4) &
    expect_equal(length(
      read_tree_qsm(path = "../qsms/qsms_tree_v230.mat",
                    version = "2.3.0")[[1]]), 4)
})


test_that("reading tree qsm version 2.4.0 in global environment works", {
  expect_equal(length(
    read_tree_qsm(path = "../qsms/qsm_tree_v240_notria.mat", global = TRUE)), 1)
})

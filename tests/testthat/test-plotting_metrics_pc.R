test_that("plot tree height pcs works", {
  # output is a list with length 2
  expect_equal(length(
    plot_tree_height_pcs(PCs_path = "../non_buttressed_trees/",
                         OUT_path = "../output/")), 2)
})

test_that("plot diameter circle fit works", {
  # output is a list with length 2
  expect_equal(length(
    plot_circle_fit_pcs(PCs_path = "../non_buttressed_trees/",
                        OUT_path = "../output/")), 4)
})

test_that("plot dbh pcs works", {
  # output is a list with length 2
  expect_equal(length(
    plot_dbh_fit_pcs(PCs_path = "../non_buttressed_trees/",
                     OUT_path = "../output/")), 4)
})

test_that("plot dab pcs works", {
  # output is a list with length 2
  expect_equal(length(
    plot_dab_fit_pcs(PCs_path = "../buttressed_trees/",
                     OUT_path = "../output/")), 5)
})

test_that("plot crown classification works", {
  # output is a list with length 2
  expect_equal(length(
    plot_crown_classification_pcs(PCs_path = "../non_buttressed_trees/",
                                  OUT_path = "../output/")), 2)
})

test_that("plot projected area works", {
  # output is a list with length 2
  expect_equal(length(
    plot_pa_pcs(PCs_path = "../non_buttressed_trees/",
                OUT_path = "../output/")), 2) &
    # with crown TRUE, output is a list with length 2
    expect_equal(length(
      plot_pa_pcs(PCs_path = "../non_buttressed_trees/", crown = TRUE,
                  OUT_path = "../output/")), 2)
})

test_that("plot alpha volume works", {
  # output is a list with length 2
  expect_equal(length(
    plot_av_pcs(PCs_path = "../non_buttressed_trees/")), 2) &
    # with crown TRUE, output is a list with length 1
    expect_equal(length(
      plot_av_pcs(PCs_path = "../non_buttressed_trees/", crown = TRUE,
                  OUT_path = "../output/")), 2)
})

test_that("tree position calculation works", {
  # output is a list of 3
  expect_equal(length(
    tree_position_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    3)
})

test_that("tree height calculation works", {
  # output is a numeric
  expect_equal(length(
    tree_height_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    1) &
    # with plot TRUE, output is a list of 4
    expect_equal(length(
      tree_height_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
                     plot = TRUE)), 4) &
    # with dtm, output is a numeric
    expect_equal(length(
      tree_height_pc(pc = read_tree_pc(
        path = "../point_clouds/pc_tree_uav.txt"),
        dtm = read_tree_pc(path = "../point_clouds/dtm.txt"))), 1) &
    # with dtm and plot TRUE, output is a list of 4
    expect_equal(length(
      tree_height_pc(pc = read_tree_pc(
        path = "../point_clouds/pc_tree_uav.txt"),
        dtm = read_tree_pc(path = "../point_clouds/dtm.txt"), plot = TRUE)), 4)

})

test_that("diameter slice calculation works", {
  # output is a list of 5
  expect_equal(length(
    diameter_slice_pc(pc = read_tree_pc(
      path = "../point_clouds/pc_tree.txt"))), 5) &
    # with plot TRUE, output is a list of 6
    expect_equal(length(
      diameter_slice_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
                        plot = TRUE)), 6) &
    # with no points at slice height, NaN is returned
    expect_equal((diameter_slice_pc(
      pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
      slice_height = 25))[[1]], NaN) &
    # with diameter over 3 m, NaN is returned
    expect_equal((diameter_slice_pc(
      pc = read_tree_pc(path = "../point_clouds/pc_tree_buttress.txt"),
      slice_height = 0.5))[[1]], NaN) &
    # with plot TRUE, output is a list of 6
    expect_equal(length(diameter_slice_pc(
      pc = read_tree_pc(path = "../point_clouds/pc_tree_buttress.txt"),
      slice_height = 0.5, plot = TRUE)), 6) &
    # with less than 3 point within the slice, NaN is returned
    expect_equal((diameter_slice_pc(
      pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
      slice_height = 1.3, slice_thickness = 0.005))[[1]], NaN)
})

test_that("extract lower trunk method works", {
  # output is a data frame with length 3 (x, y, z)
  expect_equal(length(
    extract_lower_trunk_pc(pc = read_tree_pc(
      path = "../point_clouds/pc_tree_temp.ply"))), 3)
})

test_that("dbh calculation from a tree point cloud works", {
  # output is a list of 3
  expect_equal(length(
    dbh_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))), 3) &
    # with branches at bh, output is a list of 3
    expect_equal(length(
      dbh_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree_temp.ply"))),
      3) &
    # with plot TRUE, output is a list of 4
    expect_equal(length(
      dbh_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
             plot = TRUE)), 4)
})

test_that("dab calculation from a tree point cloud works", {
  # output is a list of 3
  expect_equal(length(
    dab_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree_buttress.txt"))),
    3) &
    # with plot TRUE, output is a list of 4
    expect_equal(length(
      dab_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree_buttress.txt"),
             plot = TRUE)), 4)
})

test_that("classify crown method works", {
  # output is a list of 2
  expect_equal(length(
    classify_crown_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    2) &
    # with plot TRUE, output is a list of 5
    expect_equal(length(
      classify_crown_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
                        plot = TRUE)), 5) &
    # with buttress TRUE, output is a list of 2
    expect_equal(length(
      classify_crown_pc(pc = read_tree_pc(
        path = "../point_clouds/pc_tree_buttress.txt"),
        minheight = 4, buttress = TRUE)), 2) &
    # with buttress TRUE, output is a list of 2
    expect_equal(length(
      classify_crown_pc(pc = read_tree_pc(
        path = "../point_clouds/pc_tree_buttress.txt"),
        minheight = 4, buttress = TRUE)), 2)
})

test_that("normalization works", {
  # normalised pc has the same length as original pc
  expect_equal(nrow(
    normalize_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    nrow(read_tree_pc(path = "../point_clouds/pc_tree.txt")))
})

test_that("projected area calculation works", {
  # output is a numeric of length 1
  expect_equal(length(
    projected_area_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    1) &
    # with plot TRUE, output is a list of 2
    expect_equal(length(
      projected_area_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
                        plot = TRUE)), 2)
})

test_that("alpha volume calculation works", {
  # output is a numeric of length 1
  expect_equal(length(
    alpha_volume_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"))),
    1) &
    # with plot TRUE, output is a list of 2
    expect_equal(length(
      alpha_volume_pc(pc = read_tree_pc(path = "../point_clouds/pc_tree.txt"),
                      plot = TRUE)), 2)
})

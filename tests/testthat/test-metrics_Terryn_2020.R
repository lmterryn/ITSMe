test_that("dbh determination works", {
  # output is a numeric (dbh from qsm)
  expect_equal(length(
    dbh(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$treedata)),
    1) &
    # with pc info, output is a numeric (dbh from pc)
    expect_equal(length(
      dbh(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$treedata,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_1_pc.txt"))),
      1) &
    # with pc info & buttresses, output is a numeric (dab from pc)
    expect_equal(length(
      dbh(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$treedata,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_1_pc.txt"),
        buttress = TRUE)),
      1)
})

test_that("tree height determination works", {
  # output is a numeric (tree height from qsm)
  expect_equal(length(
    tree_height(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$treedata)),
    1) &
    # with pc info, output is a numeric (tree height from pc)
    expect_equal(length(
      tree_height(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$treedata,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_1_pc.txt"))),
      1)
})

test_that("stem branch angle calculation works", {
  # output is a numeric (tree height from qsm)
  expect_equal(length(
    stem_branch_angle_qsm(branch = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch)),
    1)
    # qsm without stem branches outputs NaN
})

test_that("stem branch cluster size calculation works", {
  # output is a numeric (tree height from qsm)
  expect_equal(length(
    stem_branch_cluster_size_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    1)
  # qsm without stem branches outputs NaN
  expect_equal(stem_branch_cluster_size_qsm(cylinder = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$cylinder), NaN)
})

test_that("stem branch radius calculation works", {
  # output is a numeric (tree height from qsm)
  expect_equal(length(
    stem_branch_radius_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1) &
  # with pc info, outputs in a numeric
  expect_equal(length(
    stem_branch_radius_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
    1) &
  # normalisation parent_cylinder, outputs in a numeric
  expect_equal(length(
    stem_branch_radius_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      normalisation = "parentcylinder")),
    1) &
  # normalisation no_normalisation, outputs in a numeric
  expect_equal(length(
    stem_branch_radius_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      normalisation = "no")),
    1)
  # qsm without stem branches outputs NaN
  expect_equal(stem_branch_radius_qsm(cylinder = read_tree_qsm(
    path = "../qsms/qsm_trunk.mat")$cylinder,
    treedata = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$treedata), NaN)
})

test_that("stem branch length calculation works", {
  # output is a numeric
  expect_equal(length(
    stem_branch_length_qsm(branch = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      stem_branch_length_qsm(branch = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # normalisation dbh, outputs in a numeric
    expect_equal(length(
      stem_branch_length_qsm(branch = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        normalisation = "dbh")),
      1) &
    # normalisation dbh & with pc info, outputs in a
    expect_equal(length(
      stem_branch_length_qsm(branch = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        normalisation = "dbh",
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # normalisation dbh & with pc & buttresses info, outputs in a
    expect_equal(length(
      stem_branch_length_qsm(branch = read_tree_qsm(
        path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$branch,
        treedata = read_tree_qsm(
          path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$treedata,
        pc = read_tree_pc(
          path = "../buttressed_trees/buttressed_tree_1_pc.txt"),
        normalisation = "dbh", buttress = TRUE)),
      1) &
    # normalisation no_normalisation, outputs in a numeric
    expect_equal(length(
      stem_branch_length_qsm(branch = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        normalisation = "no")),
      1)
    # qsm without stem branches outputs NaN
    expect_equal(stem_branch_length_qsm(branch = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$branch,
      treedata = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$treedata), NaN)
})

test_that("stem branch distance calculation works", {
  # output is a numeric
  expect_equal(length(
    stem_branch_distance_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1) &
    # normalisation dbh, outputs in a numeric
    expect_equal(length(
      stem_branch_distance_qsm(cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        normalisation = "dbh")),
      1) &
    # normalisation dbh & with pc info, outputs in a
    expect_equal(length(
      stem_branch_distance_qsm(cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        normalisation = "dbh",
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # normalisation dbh & with pc & buttresses info, outputs in a
    expect_equal(length(
      stem_branch_distance_qsm(cylinder = read_tree_qsm(
        path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$cylinder,
        treedata = read_tree_qsm(
          path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$treedata,
        pc = read_tree_pc(
          path = "../buttressed_trees/buttressed_tree_1_pc.txt"),
        normalisation = "dbh", buttress = TRUE)),
      1)
    # qsm without stem branches outputs NaN
    expect_equal(stem_branch_distance_qsm(cylinder = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$treedata), NaN)
})

test_that("dbh height ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    dbh_height_ratio_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      dbh_height_ratio_qsm(treedata = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
          pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # with pc & buttresses info, outputs in a numeric
    expect_equal(length(
      dbh_height_ratio_qsm(treedata = read_tree_qsm(
        path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$treedata,
        pc = read_tree_pc(
          path = "../buttressed_trees/buttressed_tree_1_pc.txt"),
        buttress = TRUE)),
      1)
})

test_that("dbh volume ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    dbh_volume_ratio_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      dbh_volume_ratio_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # with pc & buttresses info, outputs in a numeric
    expect_equal(length(
      dbh_volume_ratio_qsm(treedata = read_tree_qsm(
        path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$treedata,
        pc = read_tree_pc(
          path = "../buttressed_trees/buttressed_tree_1_pc.txt"),
        buttress = TRUE)),
      1)
})

test_that("volume below 55 calculation works", {
  # output is a numeric
  expect_equal(length(
    volume_below_55_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1)
})

test_that("cylinder length volume ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    cylinder_length_volume_ratio_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1)
})

test_that("shedding ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    shedding_ratio_qsm(branch = read_tree_qsm(
      path = "../qsms/qsm_tree_v200.mat", version = "2.0")$branch,
    cylinder = read_tree_qsm(
      path = "../qsms/qsm_tree_v200.mat", version = "2.0")$cylinder,
    treedata = read_tree_qsm(
      path = "../qsms/qsm_tree_v200.mat", version = "2.0")$treedata)),
    1) &
  # no sb under a third
  expect_equal(length(
    shedding_ratio_qsm(branch = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1)
  # qsm without stem branches outputs NaN
    expect_equal(shedding_ratio_qsm(branch = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$branch,
      cylinder = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$treedata), NaN)
})

test_that("branch angle ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    branch_angle_ratio_qsm(branch = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$branch)),
    1)
  # qsm without stem branches outputs NaN
  expect_equal(branch_angle_ratio_qsm(branch = read_tree_qsm(
    path = "../qsms/qsm_trunk.mat")$branch), NaN)
})

test_that("relative volume ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    relative_volume_ratio_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
      treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata)),
    1)
})

test_that("crownset method works", {
  # output is a vector of length 16
  expect_equal(length(
    crownset_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$cylinder)),
    16)
})

test_that("crown start height calculation works", {
  # output is a numeric
  expect_equal(length(
    crown_start_height_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      crown_start_height_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        cylinder = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1)
    # qsm without stem branches outputs NaN
    expect_equal(crown_start_height_qsm(treedata = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$cylinder), NaN)
})

test_that("crown height calculation works", {
  # output is a numeric
  expect_equal(length(
    crown_height_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      crown_height_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        cylinder = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1)
    # qsm without stem branches outputs NaN
    expect_equal(crown_height_qsm(treedata = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../qsms/qsm_trunk.mat")$cylinder), NaN)
})

test_that("crown evenness calculation works", {
  # output is a vector of length 26
  expect_equal(length(
    crown_evenness_qsm(cylinder = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_1_qsm.mat")$cylinder)),
    1)
  # qsm without stem branches outputs NaN
  expect_equal(crown_evenness_qsm(cylinder = read_tree_qsm(
      path = "../qsms/qsm_trunk.mat")$cylinder), 0)
})

test_that("vertical bin radii method works", {
  # output is a list of 3
  expect_equal(length(
    vertical_bin_radii_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    3)
})

test_that("crown diameter height ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    crown_diameterheight_ratio_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      crown_diameterheight_ratio_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        cylinder = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1)
})

test_that("dbh minimum tree radius ratio calculation works", {
  # output is a numeric
  expect_equal(length(
    dbh_minradius_ratio_qsm(treedata = read_tree_qsm(
      path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
      cylinder = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder)),
    1) &
    # with pc info for tree height, outputs in a numeric
    expect_equal(length(
      dbh_minradius_ratio_qsm(treedata = read_tree_qsm(
        path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$treedata,
        cylinder = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        pc = read_tree_pc(path = "../non_buttressed_trees/tree_2_pc.txt"))),
      1) &
    # with pc & buttresses info, outputs in a numeric
    expect_equal(length(
      dbh_minradius_ratio_qsm(treedata = read_tree_qsm(
        path = "../buttressed_trees/buttressed_tree_1_qsm_1.mat")$treedata,
        cylinder = read_tree_qsm(
          path = "../non_buttressed_trees/single_qsm/tree_2_qsm.mat")$cylinder,
        pc = read_tree_pc(
          path = "../buttressed_trees/buttressed_tree_1_pc.txt"),
        buttress = TRUE)),
      1)
})

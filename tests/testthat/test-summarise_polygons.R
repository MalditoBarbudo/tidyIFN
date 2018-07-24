context("summarise_polygons")

db_con <- ifn_connect()
sig <- data_sig('ifn3', db_con)
clima_plots <- sig %>% dplyr::pull(idparcela)
core <- data_core(sig, 'ifn3', 'genere', db_con, clima_plots)

test_that("summarise_polygons returns a tbl, not a tbl con", {
  expect_s3_class(
    summarise_polygons(core, polygon_group = 'provincia'), 'tbl_df'
  )
  expect_s3_class(
    summarise_polygons(core, polygon_group = 'provincia', func_group = 'idgenere'),
    'tbl_df'
  )
})

test_that("number of rows are the correct", {
  expect_equal(
    summarise_polygons(core, polygon_group = 'provincia') %>% nrow(), 4
  )
})

pool::poolClose(db_con)

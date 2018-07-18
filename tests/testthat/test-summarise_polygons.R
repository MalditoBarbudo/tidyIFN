context("summarise_polygons")

db_con <- ifn_connect()
sig <- data_sig('ifn3', db_con)
clima_plots <- sig %>% pull(idparcela)
core <- data_core(sig, 'ifn3', 'genere', db_con, clima_plots)

test_that("summarise_polygons returns a tbl, not a tbl con", {
  expect_s3_class(
    summarise_polygons(core, 'provincia'), 'tbl_df'
  )
  expect_s3_class(
    summarise_polygons(core, 'provincia', 'idgenere'), 'tbl_df'
  )
})

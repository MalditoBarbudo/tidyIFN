context("data_tests")
library(dplyr)

test_that("db connection works", {
  expect_s3_class(ifn_connect(), 'Pool')
})

test_that("data_sig returns a tbl con", {
  ifn <- "ifn3"
  db <- ifn_connect()
  expect_s3_class(data_sig(ifn, db), 'tbl_sql')
  expect_s3_class({
    data_sig(ifn, db, provincia == 'Barcelona')
  }, 'tbl_sql')
  expect_s3_class({
    data_sig(ifn, db, provincia == 'Barcelona', nomxarxa2000 == 'SenseXarxa')
  }, 'tbl_sql')
})

test_that("data_sig filters really work", {
  ifn <- "ifn3"
  db <- ifn_connect()
  expect_lt(
    {
      data_sig(ifn, db, provincia == 'Barcelona') %>% collect() %>% nrow()
    },
    {
      data_sig(ifn, db) %>% collect() %>% nrow()
    }
  )
  expect_lt(
    {
      data_sig(
        ifn, db, provincia == 'Barcelona', nomxarxa2000 == 'SenseXarxa'
      ) %>% collect() %>% nrow()
    },
    {
      data_sig(ifn, db, provincia == 'Barcelona') %>% collect() %>% nrow()
    }
  )
})

test_that("core_data returns a tbl con", {
  ifn <- 'ifn3'
  grup_func <- c('parcela', 'genere')
  db <- ifn_connect()
  data_sig <- data_sig(ifn, db)

  expect_s3_class(data_core(data_sig, ifn, grup_func[1], db), 'tbl_sql')
  expect_true(
    all(
      {data_core(data_sig, ifn, grup_func[1], db) %>% pull(idparcela)} %in%
        {data_sig %>% pull(idparcela)}
    )
  )
})

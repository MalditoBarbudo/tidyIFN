context("data_tests")
library(dplyr)

test_that("db connection works", {
  db <- ifn_connect('malditobarbudo')
  expect_s3_class(db, 'Pool')
  pool::poolClose(db)
})

test_that("data_sig returns a tbl con", {
  ifn <- "ifn3"
  db <- ifn_connect('malditobarbudo')
  expect_s3_class(data_sig(ifn, db), 'tbl_sql')
  expect_s3_class({
    data_sig(ifn, db, provincia == 'Barcelona')
  }, 'tbl_sql')
  expect_s3_class({
    data_sig(ifn, db, provincia == 'Barcelona', nomxarxa2000 == 'SenseXarxa')
  }, 'tbl_sql')

  pool::poolClose(db)
})

test_that("data_sig filters really work", {
  ifn <- "ifn3"
  db <- ifn_connect('malditobarbudo')
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
  pool::poolClose(db)
})

test_that("data_clima returns a tbl con", {
  ifn <- "ifn3"
  db <- ifn_connect('malditobarbudo')
  data_sig <- data_sig(ifn, db)
  data_sig_filtered <- data_sig(ifn, db, provincia == 'Barcelona')
  expect_s3_class(data_clima(data_sig, ifn, db), 'tbl_sql')
  expect_s3_class(data_clima(data_sig_filtered, ifn, db), 'tbl_sql')
  expect_s3_class({
    data_clima(data_sig, ifn, db, radiacioanual < 1000)
  }, 'tbl_sql')
  expect_s3_class({
    data_clima(
      data_sig, ifn, db, radiacioanual < 1000, temperaturamitjanaanual < 18
    )
  }, 'tbl_sql')
  pool::poolClose(db)
})

test_that("data_clima filters work", {
  ifn <- "ifn3"
  db <- ifn_connect('malditobarbudo')
  data_sig <- data_sig(ifn, db)
  data_sig_filtered <- data_sig(ifn, db, provincia == 'Barcelona')

  expect_lt(
    {
      data_clima(data_sig, ifn, db, radiacioanual < 1000) %>% collect() %>% nrow()
    },
    {
      data_clima(data_sig, ifn, db) %>% collect() %>% nrow()
    }
  )

  expect_lt(
    {
      data_clima(data_sig_filtered, ifn, db, radiacioanual < 1000) %>% collect() %>% nrow()
    },
    {
      data_clima(data_sig_filtered, ifn, db) %>% collect() %>% nrow()
    }
  )
  pool::poolClose(db)
})

test_that("data_core returns a tbl con", {
  ifn <- 'ifn3'
  grup_func <- c('parcela', 'genere')
  db <- ifn_connect('malditobarbudo')
  data_sig <- data_sig(ifn, db)
  clima_plots <- data_clima(data_sig, ifn, db) %>% pull(idparcela)

  expect_s3_class(data_core(data_sig, ifn, grup_func[1], db, clima_plots), 'tbl_sql')
  expect_true(
    all(
      {data_core(data_sig, ifn, grup_func[1], db, clima_plots) %>% pull(idparcela)} %in%
        {data_sig %>% pull(idparcela)}
    )
  )
  expect_s3_class(data_core(data_sig, ifn, grup_func[2], db, clima_plots), 'tbl_sql')
  expect_true(
    all(
      {data_core(data_sig, ifn, grup_func[2], db, clima_plots) %>% pull(idparcela)} %in%
      {data_sig %>% pull(idparcela)}
    )
  )
  pool::poolClose(db)
})


test_that("data_core with diameter_classes TRUE works on", {
  ifn <- 'ifn3'
  grup_func <- c('parcela', 'genere')
  db <- ifn_connect('malditobarbudo')
  data_sig <- data_sig(ifn, db)
  clima_plots <- data_clima(data_sig, ifn, db) %>% pull(idparcela)

  expect_s3_class(
    data_core(
      data_sig, ifn, grup_func[1], db, clima_plots, diameter_classes = TRUE
    ), 'tbl_sql'
  )
  expect_true(
    "idcd" %in% names(data_core(
      data_sig, ifn, grup_func[1], db, clima_plots, diameter_classes = TRUE
    ) %>% collect())
  )
  expect_s3_class(
    data_core(
      data_sig, ifn, grup_func[2], db, clima_plots, diameter_classes = TRUE
    ), 'tbl_sql'
  )
  expect_true(
    "idcd" %in% names(data_core(
      data_sig, ifn, grup_func[2], db, clima_plots, diameter_classes = TRUE
    ) %>% collect())
  )
  pool::poolClose(db)
})

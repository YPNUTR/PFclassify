test_that("ReFormatDF creates standard variables without recoding", {

  df <- data.frame(
    cycle_old = c("A", "B"),
    food_old = c("11111111", "22222222"),
    wt_old = c("10", "20")
  )

  result <- ReFormatDF(
    infile = df,
    var_cycle = "cycle_old",
    var_Food_code = "food_old",
    var_weight = "wt_old",
    your_cycle = c(),
    out_cycle = c()
  )

  expect_true("cycle" %in% names(result))
  expect_true("Food_code" %in% names(result))
  expect_true("Food_code_weight" %in% names(result))

  expect_equal(result$cycle, c("A", "B"))
  expect_equal(result$Food_code, c(11111111, 22222222))
  expect_equal(result$Food_code_weight, c(10, 20))

  expect_false("cycle_old" %in% names(result))
  expect_false("food_old" %in% names(result))
  expect_false("wt_old" %in% names(result))
})


test_that("ReFormatDF recodes cycle labels", {

  df <- data.frame(
    cycle_old = c("A", "B"),
    food_old = c("11111111", "22222222"),
    wt_old = c("10", "20")
  )

  result <- ReFormatDF(
    infile = df,
    var_cycle = "cycle_old",
    var_Food_code = "food_old",
    var_weight = "wt_old",
    your_cycle = c("A", "B"),
    out_cycle = c("2017-2018", "2019-2020")
  )

  expect_equal(
    result$cycle,
    c("2017-2018", "2019-2020")
  )
})


test_that("ReFormatDF errors when cycle vectors have different lengths", {

  df <- data.frame(
    cycle_old = "A",
    food_old = "11111111",
    wt_old = "10"
  )

  expect_error(
    ReFormatDF(
      infile = df,
      var_cycle = "cycle_old",
      var_Food_code = "food_old",
      var_weight = "wt_old",
      your_cycle = c("A"),
      out_cycle = c("2017-2018", "2019-2020")
    ),
    "same length"
  )
})


test_that("ReFormatDF rejects food codes with fewer than 8 digits", {

  df <- data.frame(
    cycle_old = "A",
    food_old = "1234567",
    wt_old = "10"
  )

  expect_error(
    ReFormatDF(
      infile = df,
      var_cycle = "cycle_old",
      var_Food_code = "food_old",
      var_weight = "wt_old",
      your_cycle = c(),
      out_cycle = c()
    ),
    "8-digit USDA food code"
  )
})


test_that("ReFormatDF rejects food codes with more than 8 digits", {

  df <- data.frame(
    cycle_old = "A",
    food_old = "123456789",
    wt_old = "10"
  )

  expect_error(
    ReFormatDF(
      infile = df,
      var_cycle = "cycle_old",
      var_Food_code = "food_old",
      var_weight = "wt_old",
      your_cycle = c(),
      out_cycle = c()
    ),
    "8-digit USDA food code"
  )
})


test_that("ReFormatDF rejects non-numeric food codes", {

  df <- data.frame(
    cycle_old = "A",
    food_old = "ABCDEFGH",
    wt_old = "10"
  )

  expect_warning(
    expect_error(
      ReFormatDF(
        infile = df,
        var_cycle = "cycle_old",
        var_Food_code = "food_old",
        var_weight = "wt_old",
        your_cycle = c(),
        out_cycle = c()
      ),
      "8-digit USDA food code"
    ),
    "NAs introduced by coercion"
  )
})

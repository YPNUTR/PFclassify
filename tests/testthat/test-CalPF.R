test_that("CalPF returns data frame for valid minimal input", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  result <- CalPF(df)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1)
})


test_that("CalPF keeps Food_description when fdcd_description = TRUE", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  result <- CalPF(df, fdcd_description = TRUE)

  expect_true("Food_description" %in% names(result))
})


test_that("CalPF removes Food_description when fdcd_description = FALSE", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  result <- CalPF(df, fdcd_description = FALSE)

  expect_false("Food_description" %in% names(result))
})


test_that("CalPF errors when required columns are missing", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1]
  )

  expect_error(
    CalPF(df),
    "infile must contain variables"
  )
})


test_that("CalPF errors when Food_code is not numeric", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = "12345678",
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df),
    "Food_code must be numeric"
  )
})


test_that("CalPF errors for invalid 8-digit Food_code values", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = 1234567,   # 7-digit invalid
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df),
    "8-digit USDA food codes"
  )
})


test_that("CalPF errors when Food_code_weight is not numeric", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = "100"
  )

  expect_error(
    CalPF(df),
    "Food_code_weight must be numeric"
  )
})


test_that("CalPF errors when cycle is invalid", {

  df <- data.frame(
    cycle = "INVALID_CYCLE",
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df),
    "cycle contains values not supported"
  )
})


test_that("CalPF validates option_subtotal input", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df, option_subtotal = "fish"),
    "option_subtotal must be a subset"
  )
})


test_that("CalPF validates option_meat input", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df, option_meat = "fish"),
    "option_meat must be a subset"
  )
})


test_that("CalPF validates option_poult input", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df, option_poult = "duck"),
    "option_poult must be a subset"
  )
})


test_that("CalPF validates option_curedmeat input", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = PF_fc2pf$Food_code[1],
    Food_code_weight = 100
  )

  expect_error(
    CalPF(df, option_curedmeat = "duck"),
    "option_curedmeat must be a subset"
  )
})


test_that("CalPF handles non-matching Food_code gracefully", {

  df <- data.frame(
    cycle = PF_fc2pf$cycle[1],
    Food_code = 10000000,
    Food_code_weight = 100
  )

  result <- CalPF(df)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1)
})

#' Reformat a data frame with recoded cycle values
#'
#' This function takes an input data frame and three variable names
#' (cycle, food code, weight). It recodes the cycle variable according
#' to a mapping of `your_cycle` → `out_cycle`, converts food code and
#' weight to numeric, removes the old variables, and returns the result.
#'
#' @param infile A data frame containing the variables to be reformatted.
#' @param var_cycle Character string, the name of the cycle variable in `infile`.
#' @param var_Food_code Character string, the name of the food code variable in `infile`.
#' @param var_weight Character string, the name of the weight variable in `infile`.
#' @param your_cycle A vector of cycle values to be recoded.
#' @param out_cycle A vector of new values that will replace those in `your_cycle`.
#'
#' @return A data frame with:
#' * `cycle` (recoded),
#' * `Food_code` (numeric),
#' * `Food_code_weight` (numeric),
#' and the original cycle/food/weight columns removed.
#'
#' @examples
#' data(PF_sample_1)
#' ReFormatDF(
#'   infile = PF_sample_1,
#'   var_cycle = "year",
#'   var_Food_code = "fdcd",
#'   var_weight = "fdwt",
#'   your_cycle = c(),
#'   out_cycle = c()
#' )
#'
#' data(PF_sample_2)
#' ReFormatDF(
#'   infile = PF_sample_2,
#'   var_cycle = "year",
#'   var_Food_code = "fdcd",
#'   var_weight = "fdwt",
#'   your_cycle = c("cycle1314"),
#'   out_cycle = c("1314")
#' )
#'
#' data(PF_sample_3)
#' ReFormatDF(
#'   infile = PF_sample_3,
#'   var_cycle = "year",
#'   var_Food_code = "fdcd",
#'   var_weight = "fdwt",
#'   your_cycle = c("cycle0708", "cycle0910", "cycle1112"),
#'   out_cycle = c("0708", "0910", "1112")
#' )
#'
#' @export
ReFormatDF <- function(infile,
                       var_cycle, # char: name of cycle variable
                       var_Food_code, # char: name of food code variable
                       var_weight, # char: name of weight variable
                       your_cycle, # vector
                       out_cycle # vector
) {
  # check length
  if (length(your_cycle) != length(out_cycle)) {
    stop("your_cycle and out_cycle must be the same length")
  }

  # turn variable names into symbols
  cycle_sym <- rlang::sym(var_cycle)
  food_sym <- rlang::sym(var_Food_code)
  weight_sym <- rlang::sym(var_weight)

  # when no need to edit cycle label
  if (length(your_cycle) == 0) {
    outfile <- infile |>
      dplyr::mutate(
        cycle = !!cycle_sym,
        Food_code = as.numeric(!!food_sym),
        Food_code_weight = as.numeric(!!weight_sym)
      ) |>
      dplyr::select(-!!cycle_sym, -!!food_sym, -!!weight_sym)
  }

  # when need to edit cycle label
  else {
    # build case_when expressions programmatically
    conditions <- purrr::map2(your_cycle, out_cycle, ~ rlang::expr(!!cycle_sym == !!.x ~ !!.y))
    conditions <- c(conditions, rlang::expr(TRUE ~ NA_character_))

    outfile <- infile |>
      dplyr::mutate(
        cycle = dplyr::case_when(!!!conditions),
        Food_code = as.numeric(!!food_sym),
        Food_code_weight = as.numeric(!!weight_sym)
      ) |>
      dplyr::select(-!!cycle_sym, -!!food_sym, -!!weight_sym)
  }

  if (any(is.na(outfile$Food_code)) ||
      any(outfile$Food_code < 10000000 |
          outfile$Food_code > 99999999)) {
    stop("Your food codes are not 8-digit USDA food code")
  }

  return(outfile)
}

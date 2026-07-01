#' Link USDA Food Code with Protein Food (PF) Groups
#'
#' This function takes a formatted data frame (produced by
#' \code{\link{ReFormatDF}}), joins it with the internal PF lookup
#' table (\code{PF_fc2pf}), applies weighting by food code weight,
#' and extracts PF-related variables based on user-specified options.
#'
#' @param infile A data frame that has been processed by
#'   \code{\link{ReFormatDF}}.
#' @param var_id Character string, the name of the participant ID variable in `infile`.
#' @param fdcd_description Logical. If \code{TRUE}, keep the food
#'   description column from the PF lookup table. If \code{FALSE}, drop it.
#' @param option_subtotal Character vector of subtotal PF groups to include.
#'   Must be a subset of \code{c("meat", "poult", "curedmeat")}.
#' @param option_meat Character vector of (red) meat PF subgroups to include.
#'   Must be a subset of \code{c("beef", "pork", "other")}.
#' @param option_poult Character vector of poultry PF subgroups to include.
#'   Must be a subset of \code{c("chick", "turkey", "other")}.
#' @param option_curedmeat Character vector of cured meat PF subgroups to include.
#'   Must be a subset of \code{c("beef", "pork", "chick", "turkey", "other")}.
#'
#' @return A data frame with:
#' \itemize{
#'   \item cycle and Food_code columns,
#'   \item weighted PF subtotal columns as defined by the options.
#' }
#'
#' @details
#' This function requires an internal dataset \code{PF_fc2pf}
#' that maps cycles and food codes to PF variables. All PF variables
#' are multiplied by \code{Food_code_weight} before subtotals are selected.
#'
#' The option arguments are validated: each must be a subset of the
#' allowed categories. Empty vectors (\code{c()}) are permitted,
#' which will result in no columns being selected for that option.
#'
#' @examples
#' # Example 1 using PF_sample_1
#' # Keep subtotal of lean (red) meat, lean beef, lean pork, lean poultry and lean chicken
#' PF_sample_1_fmt <- ReFormatDF(
#'   infile = PF_sample_1,
#'   var_cycle = "year",
#'   var_Food_code = "fdcd",
#'   var_weight = "fdwt",
#'   your_cycle = c(),
#'   out_cycle = c()
#' )
#'
#' PF_sample_1_PF_break <- CalPF(
#'   infile = PF_sample_1_fmt,
#'   fdcd_description = TRUE,
#'   var_id = "id",
#'   option_subtotal = c("meat", "poult"),
#'   option_meat = c("beef", "pork"),
#'   option_poult = c("chick"),
#'   option_curedmeat = c()
#' )
#'
#' colnames(PF_sample_1_PF_break)
#'
#' # Example 2 using PF_sample_3
#' # Keep subtotal of cured meat, with its components: cured pork and cured chicken
#'
#' PF_sample_3_fmt <- ReFormatDF(
#'   infile = PF_sample_3,
#'   var_cycle = "year",
#'   var_Food_code = "fdcd",
#'   var_weight = "fdwt",
#'   your_cycle = c("cycle0708", "cycle0910", "cycle1112"),
#'   out_cycle = c("0708", "0910", "1112")
#' )
#'
#' PF_sample_3_PF_break <- CalPF(
#'   infile = PF_sample_3_fmt,
#'   fdcd_description = TRUE,
#'   var_id = "id",
#'   option_subtotal = c("curedmeat"),
#'   option_meat = c(),
#'   option_poult = c(),
#'   option_curedmeat = c("pork", "chick")
#' )
#'
#' colnames(PF_sample_3_PF_break)
#'
#' @export


CalPF <- function(infile,
                  var_id = "SEQN",
                  fdcd_description = TRUE,
                  option_subtotal = c("meat", "poult", "curedmeat"),
                  option_meat = c("beef", "pork", "other"),
                  option_poult = c("chick", "turkey", "other"),
                  option_curedmeat = c("beef", "pork", "chick", "turkey", "other")) {

  # define allowed vocabularies
  allowed_subtotal <- c("meat", "poult", "curedmeat")
  allowed_meat <- c("beef", "pork", "other")
  allowed_poult <- c("chick", "turkey", "other")
  allowed_curedmeat <- c("beef", "pork", "chick", "turkey", "other")

  # validate user input
  if (!all(option_subtotal %in% allowed_subtotal)) {
    stop("option_subtotal must be a subset of: ", paste(allowed_subtotal, collapse = ", "))
  }
  if (!all(option_meat %in% allowed_meat)) {
    stop("option_meat must be a subset of: ", paste(allowed_meat, collapse = ", "))
  }
  if (!all(option_poult %in% allowed_poult)) {
    stop("option_poult must be a subset of: ", paste(allowed_poult, collapse = ", "))
  }
  if (!all(option_curedmeat %in% allowed_curedmeat)) {
    stop("option_curedmeat must be a subset of: ", paste(allowed_curedmeat, collapse = ", "))
  }

  # check infile data (1) with all variables needed?
  required_vars <- c("cycle", "Food_code", "Food_code_weight")
  if (!all(required_vars %in% names(infile))) {
    stop(
      "infile must contain variables: ",
      paste(required_vars, collapse = ", ")
    )
  }

  # check infile data (2) Food_code in correct format?
  if (!is.numeric(infile$Food_code)) {
    stop("Food_code must be numeric")
  }
  if (any(is.na(infile$Food_code)) ||
      any(infile$Food_code < 10000000 |
          infile$Food_code > 99999999)) {
    stop("Food_code must only contain valid 8-digit USDA food codes")
  }

  # check infile data (3) Food_code_weight in correct format?
  if (!is.numeric(infile$Food_code_weight)) {
    stop("Food_code_weight must be numeric")
  }

  # check infile data (4) cycle are all allowed values like "1516" not others
  allowed_cycles <- unique(PF_fc2pf$cycle)
  if (!all(infile$cycle %in% allowed_cycles)) {
    stop(
      "cycle contains values not supported by the package, use ReFormatDF to format your infile data"
    )
  }

  # join with lookup table, optionally dropping Food_description
  if (fdcd_description) {
    temp <- infile |>
      dplyr::left_join(PF_fc2pf, by = c("cycle", "Food_code"))
  } else {
    temp <- infile |>
      dplyr::left_join(
        dplyr::select(
          PF_fc2pf,
          -dplyr::all_of("Food_description")
        ),
        by = c("cycle", "Food_code")
      )
  }

  # build list of subtotal symbols based on user’s options
  ### use make_syms function to build four lists
  ### if user put in c(), it will not build a variable to select
  make_syms <- function(prefix, opts) {
    if (length(opts) > 0) {
      rlang::syms(paste0(prefix, opts, "_sum"))
    } else {
      list()
    }
  }

  subtotal_syms_1 <- make_syms("PF_", toupper(option_subtotal))
  subtotal_syms_2 <- make_syms("PF_MEAT_", tolower(option_meat))
  subtotal_syms_3 <- make_syms("PF_POULT_", tolower(option_poult))
  subtotal_syms_4 <- make_syms("PF_CUREDMEAT_", tolower(option_curedmeat))

  temp_varnames <- colnames(temp)
  temp_varnames <- temp_varnames[!startsWith(temp_varnames, "PF_")]

  # main calculation: weight all PF_ variables
  outfile <- temp |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("PF_"), ~ .x * Food_code_weight)) |>
    dplyr::select(
      dplyr::all_of(temp_varnames),
      !!!subtotal_syms_1, !!!subtotal_syms_2, !!!subtotal_syms_3, !!!subtotal_syms_4
    )

  return(outfile)
}

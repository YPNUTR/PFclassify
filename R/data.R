#' Example Dataset 1
#'
#' The 1st practice dataset for demonstrating the \code{\link{ReFormatDF}} function.
#' This example only has one cycle, and all the variable formats match the need of classification function.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{year}{character cycle identifier}
#'   \item{id}{integer participant ID}
#'   \item{fdcd}{numeric USDA Food Code}
#'   \item{fdwt}{numeric intake weight}
#' }
#'
#' @examples
#' data(PF_sample_1)
#' head(PF_sample_1)
"PF_sample_1"

#' Example Dataset 2
#'
#' The 2nd practice dataset for demonstrating the \code{\link{ReFormatDF}} function.
#' This example only has one cycle, and some variable formats don't match the need of classification function.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{year}{character cycle identifier}
#'   \item{id}{integer participant ID}
#'   \item{fdcd}{character USDA Food Code}
#'   \item{fdwt}{numeric intake weight}
#' }
#'
#' @examples
#' data(PF_sample_2)
#' head(PF_sample_2)
"PF_sample_2"

#' Example Dataset 3
#'
#' The 3rd practice dataset for demonstrating the \code{\link{ReFormatDF}} function.
#' This example only has multiple cycles, and some variable formats don't match the need of classification function.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{year}{character cycle identifier}
#'   \item{id}{integer participant ID}
#'   \item{fdcd}{character USDA Food Code}
#'   \item{fdwt}{numeric intake weight}
#' }
#'
#' @examples
#' data(PF_sample_3)
#' head(PF_sample_3)
"PF_sample_3"

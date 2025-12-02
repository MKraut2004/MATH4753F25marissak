# Save dataset (run interactively)
#fire <- read.csv("FIREDAM.csv")
#usethis::use_data(fire, overwrite = TRUE)

# Documentation (put in R/fire.R)
#' FIREDAM fire dataset
#'
#' A dataset containing fire-related data from XYZ source.
#'
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#'   \item{DISTANCE}{Distance of fire from some reference point}
#'   \item{DAMAGE}{Damage in monetary units or scale}
#' }
#' @source <URL or reference for the data>
#' @examples
#' NULL
"fire"

# globals.R
# This tells R that 'x' is a global variable to avoid check notes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("x"))
}

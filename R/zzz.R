#' @keywords internal
#' @noRd

#message when loading package
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname),
                        " of ", pkgname, '.',
                        '\n', pkgname, ' is BETA software! Please report any bugs.')
}

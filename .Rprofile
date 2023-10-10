# When starting a new R session, a specific directory is added to the libPath.
# It's called libWin resp. libLinux. As it is on the first libPath position,
# packages are installed into this directory by default. This enables working in
# a sandbox.

.First <- function() {
  options(repos = c(
    CRAN = "https://packagemanager.posit.co/cran/2020-12-31",
    PANDORA = "https://Pandora-IsoMemo.github.io/drat/"
  ))

  .libPaths(new = c(paste(getwd(), "lib", sep = "/"), .libPaths()))
}

.First()

# When starting a new R session, a specific directory is added to the libPath.
# It's called libWin resp. libLinux. As it is on the first libPath position,
# packages are installed into this directory by default. This enables working in
# a sandbox.

.First <- function() {
  options(repos = c(
    CRAN = "https://mran.microsoft.com/snapshot/2021-01-01",
    INWT = "https://inwt-vmeh2.inwt.de/r-repo"
  ))

  .libPaths(new = c(paste(getwd(), "lib", sep = "/"), .libPaths()))
}

.First()

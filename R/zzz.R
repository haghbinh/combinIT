.onUnload <- function(libpath) {
  library.dynam.unload("combinIT", libpath)
  invisible()
  .pkg_envir[["on_windows"]] <- this_os() == "windows"
}

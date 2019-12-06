### Actually want to create a generic which will track an object

megaTrack <- function(arg1) UseMethod("metaTrack")
# megaTrack.default <- function(arg1) "not defined for this class"


megaTrack.list <- function(a_list, level = "default"){
  writeDict(a_list, level = level)
}

megaTrack.data.frame <- function(a_df, identified_by){
  writeDict(a_df, level = identified_by)
}

writeDict <- function(element, level) {
  # wrapper to abstract the writing decisions into a writer
  print("writing a base dict")
}


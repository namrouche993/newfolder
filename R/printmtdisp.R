#' Title
#'
#' @export
#'
printmt0 <- function(){
  print(mt$disp)
}

#' print external data
#'
#' @export
#'
print_external_data<-function(){
  print(newfolder::dat)
}

#' print_rnorm_systime
#'
#' @export
#'
print_rnorm_systime <- function(){
  paste0(rnorm(5,0,5),"- - - - - ",Sys.time())
}




#                                           ##### THIS IS WRONG #####
# #' print external data                    ##### THIS IS WRONG #####
# #'                                        ##### THIS IS WRONG #####
# #' @export                                ##### THIS IS WRONG #####
# #'                                        ##### THIS IS WRONG #####
# print_external_data2<-function(){         ##### THIS IS WRONG #####
#   print(dat[1:2,1:2])                     ##### THIS IS WRONG #####
# }                                         ##### THIS IS WRONG #####
#                                           ##### THIS IS WRONG #####
#                                           ##### THIS IS WRONG #####

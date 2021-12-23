student1=list(name="pablo",age=21,localisation="londonn")
student2=list(name="samir",age=24,localisation="paris")
class(student1)="elevesinfo"
class(student2)="elevesinfo"

contact <- function(object) {
  UseMethod("contact")
}
contact.elevesinfo=function(object){
  cat("this name is",object$name,"and he is",object$age,"from",object$localisation,"\n")
}

contact(student1)
contact(student2)

contact.numeric=function(object){
  object+500
}


contact.character=function(object){
  unlist(strsplit(object,"a"))
}
contact(60)
contact("usmalger")

######################### S4 ##################

setClass("employee", slots=list(name="character", id="numeric", contact="character"))
obj <- new("employee",name="Steven", id=1002, contact="West Avenue")
setMethod("show",
          "employee",
          function(object) {
            cat("Name:",object@name, "\n")
            cat("Id:",object@id, "\n")
            cat("Contact:", object@contact, "\n")
          }
)
obj
obj@name


###################### R6 : #########################
library(R6)

Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x
    invisible(self)
  })
)
xa=Accumulator$new()
xa$sum
xa$add(25)
xa$sum
xa$add(1000)
xa$sum



AccumulatorChatty <- R6Class("AccumulatorChatty",
                             inherit = Accumulator,
                             public = list(
                               add = function(x = 1) {
                                 cat("Adding ", x, "\n", sep = "")
                                 super$add(x = x)
                               }
                             )
)


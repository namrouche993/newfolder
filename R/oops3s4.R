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

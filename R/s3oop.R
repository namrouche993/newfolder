#https://www.datacamp.com/community/tutorials/r-objects-and-classes
studentBio1 <- list(studentName = "Harry Potter", studentAge = 19, studentContact="London")
studentBio2 <- list(studentName = "Mourad boualem", studentAge = 49, studentContact="Oran")

class(studentBio1) <- "StudentInfo"
class(studentBio2) <- "StudentInfo"

studentBio1
studentBio2

contact <- function(object) {
  UseMethod("contact")
}

contact.StudentInfo <- function(object) {
  cat("Your contact is", object$studentContact, "\n")
  cat("your contact is",object$studentAge,"years old \n")
}

#  WARNING : BELOW IS WRONG : NOOOOO 
# contact.age <-function(object) {
#   cat("your contact is",object$studentAge,"\n")
# }

contact(studentBio1)
contact(studentBio2)



contact.numeric <-function(object) {
  object+20
}
contact(50)

# print.StudentInfo <- function(x){
#   cat("bonjour",x$studentName)
# }


################### S4 :
setClass("employee", slots=list(name="character", id="numeric", contact="character"))
obj <- new("employee",name="Steven", id=1002, contact="West Avenue")

setMethod("show",
          "employee",
          function(object) {
            cat("Name:",object@name, "\n")
            cat("Id:",object@id, "\n")
            cat("Contactez:", object@contact, "\n")
            plot(mtcars$mpg)
          }
)
obj



#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#methods(print)

x <- list(name ="Arjun", account_no = 1234,
          saving = 1500, withdrawn = 234,
          math=20,
          arabe=16,
          phy=9
          )
class(x)<-"bank"
print.bank<-function(obj)
{
  cat("Name is ", obj$name, "\n")
  cat(obj$account_no, " is the Acc no of the holder\n ")
  cat(obj$saving, " is the amount of saving in the account \n ")
  cat(obj$withdrawn, " is the withdrawn amount\n")
}


mean.bank<-function(obj) {
  cat( (obj$math*5+obj$arabe*5+obj$phy*2)/12)
  #UseMethod("sum")
}

mean(x)

#########################################################
#########################################################



bar <- function(x) UseMethod("bar", x)

bar.numeric=function(x){
  x+100
}
bar.character=function(x){
  strsplit(x,"a")
}

bar.factor=function(x){
  as.vector(x[1])
}

bar(51)
bar("mcalger")

f=factor(c("h","mca","usmal"))
bar(f)


#############

NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## The Methods for this class normally go here but are discussed
    ## below. A simple placeholder is here to give you a teaser....
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"NordAmericain")
  return(me)
}
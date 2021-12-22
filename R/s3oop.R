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
bar(51)
bar("mcalger")
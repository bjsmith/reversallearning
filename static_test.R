#I'm testing building C-like static variables
#as in https://www.r-bloggers.com/emulating-local-static-variables-in-r/

myFunc <- function(a){
  #curr_state<-attr(myFunc,"my_curr_state")
  if(is.null(attr(myFunc, "my_curr_state"))){
    attr(myFunc, "my_curr_state")<<-"ON"
    print("starting up!")
  }
  
  print("This function loves you <3 ...is that cute or creepy?")
  print(a)
}

myFunc("hello")
myFunc("hello")

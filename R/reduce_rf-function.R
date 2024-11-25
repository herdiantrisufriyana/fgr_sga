#' Reduce caret object size like random forest
#'
#' Reduce caret object size like random forest.
#'
#' @param object Caret object, a caret object like random forest.
#'
#' @details
#' None.
#' 
#' @return Caret object, a caret object like random forest.
#' 

reduce_rf=function(object){
  object$trainingData=NULL
  object$control=NULL
  object$finalModel$training=NULL 
  object$finalModel$validation=NULL
  object$finalModel$xNames=NULL
  
  attr(object$terms,".Environment")=c()
  attr(object$formula,".Environment")=c()
  
  object
}
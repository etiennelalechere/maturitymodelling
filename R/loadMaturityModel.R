#' @title Load maturity model
#' @description Load maturity and maturity attribute models
#' @usage loadMaturityModel(modelName)
#' @param modelName character, "initModel" referred to he calibrated model from the initial dataset (660 plots), metrics calculated within a 35m radius, to make a prediction in 60m side pixels.
#' @return Return the random forest model.
#' @export
#' @import randomForest
#' @examples loadMaturityModel(modelName="initModel")
loadMaturityModel<-function(modelName){
  if(modelName=="initModel"){
    initModel_IMAT=initModel_IMAT
  }else{
    initModel_IMAT="NA, try another model name."
  }
  return(initModel_IMAT)
}


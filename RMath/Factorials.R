#------------------------------------------------------------------------------
# File Name: Factorials.R
#
# Description:
#   R script file made to perform factorial calculations.
#   Take a R-Vector data type and performs a calculation. User is returned a
#   data frame with the given base values and their factorial outputs.
#
# Note:
#   Call the 'DoFactorial' function and pass it the vector argument. 
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
ValidateVector <- function(vect){
  #Check: Vector is length > 0 and it is numeric
  if (length(vect) == 0 | (!is.numeric(vect))){
    return(FALSE)
  }
  
  #Check: Vector does not contain a NA
  if (TRUE %in% is.na(vect)){
    return(FALSE)
  }
  
  #Check: Vector only contains positive numerical values
  if (FALSE %in% (vect > 0)){
    return(FALSE)  
  } else {return(TRUE)}
}
#------------------------------------------------------------------------------
DoBaseFactorial <- function(n){
  outputFact = 1
  for (i in 1:n){
    outputFact = outputFact * i
  }
  return(outputFact)
}
#------------------------------------------------------------------------------
DoVectorFactorial <- function(baseVect){
  outputVect = vector("integer", length(baseVect))
  for (i in 1:length(baseVect)){
    if(baseVect[i] == 0){
      outputVect[i] = 1
    } else {
      outputVect[i] = DoBaseFactorial(baseVect[i])
    }
  }
  return(outputVect)
}
#------------------------------------------------------------------------------
DoFactorial <- function(baseVect){
  if(!ValidateVector(baseVect)){
    return(print("Error - Vector Contains Invalid Data Type"))
  }
  baseVect = as.integer(round(baseVect, 0))
  
  dfFactorial = data.frame(Base = baseVect,
                           Factorial = DoVectorFactorial(baseVect))
  return(dfFactorial)
}
#------------------------------------------------------------------------------
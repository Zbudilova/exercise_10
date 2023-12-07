

array <- "CARACAS$"
pattern <- "CA"

SuffixArray <- function(array){
  list_of_arrays <- c()
  list_of_arrays <- c(list_of_arrays,array)
  
  for (i in (2:nchar(array))){
    sub_arr <- substr(array,i,nchar(array))
    list_of_arrays <- c(list_of_arrays,sub_arr)
  }
  
  sort_list_of_arrays <- sort(list_of_arrays)

  vector <- c()
  
  for (j in sort_list_of_arrays){
    index <- which(list_of_arrays==j)
    vector <- c(vector,index)
  }
  return(vector)
  
}

SA <- SuffixArray(array)


InverseSuffixArray <- function(sorted_array){
  vector <- c()
  for (num in (1:length(sorted_array))){
    index <- which(sorted_array==num)
    vector <- c(vector,index)
  }
  
  return(vector)
}

inverse_SA <- InverseSuffixArray(SA) 



LCPArray <- function(array,SA,inverse_SA){
  LCP <- rep(0,length(SA)+1)
  LCP[1]<- -1
  LCP[length(LCP)]<- -1
  m <- length(SA)
  l <- 0
  for (i in (1:m)){
    j <- inverse_SA[i]
    if (j>1){
      k <- SA[j-1]
      while ((substr(array,k+l,k+l)) == (substr(array,i+l,i+l))){
        l <- l+1
      }
      LCP[j] <- l
      l <- max((l-1),0)
    }
  }
  return(LCP)
  
}

LCPArray(array,SA,inverse_SA)



porovnej_stringy <- function(string1,string2){
  if (startsWith(string1,string2)){
    return(TRUE)
  }
  else if(string1 < string2){
    return(TRUE)
    
  }
  else{
    return(FALSE)
  }
}

BinarySearchSA <- function(pattern,array,SA){
  minIndex <- 1
  maxIndex <- nchar(array)
  while (minIndex < maxIndex){
    middleIndex <- floor((minIndex + maxIndex) / 2)
    new_array <- substr(array,SA[middleIndex],nchar(array))
    
    if (porovnej_stringy(pattern,new_array)){
      maxIndex <- middleIndex
    }
    else{
      minIndex <- middleIndex+1
    }
  }
  first <- minIndex
  maxIndex <- nchar(array)
  while (maxIndex > minIndex){
    middleIndex <- floor((minIndex+maxIndex)/2)
    new_array <- substr(array,SA[middleIndex],nchar(array))
    if (porovnej_stringy(new_array,pattern)){
      minIndex <- middleIndex+1
    }
    else{
      maxIndex <- middleIndex
    }
  }
  last <- maxIndex-1
  if (last < first){
    return("Patter does not appear in text")
  }
  else{
    return(c(first,last))
  }
}



BinarySearchSA(pattern, array,SA)






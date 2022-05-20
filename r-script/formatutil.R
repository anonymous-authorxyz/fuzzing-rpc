measureA <- function(a,b){
  
  if(length(a)==0 & length(b)==0){
    return(0.5)
  } else if(length(a)==0){
    ## motivation is that we have no data for "a" but we do for "b".
    ## maybe the process generating "a" always fail (eg out of memory)
    return(0)
  } else if(length(b)==0){
    return(1)
  }
  
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])
  
  m = length(a)
  n = length(b)
  A = (r1/m - (m+1)/2)/n
  
  return(A)
}


## (a-b)/b
relative <- function(a, b){
  relativev="Inf"
  if(b >0){
    relative = (mean(a) - mean(b))/mean(b) 
    indicator <- ""
    if(relative >= 0)
      indicator <- "+"
    relativev = paste(indicator,formatC( relative *100, digits = 2, format = "f"), "\\%",sep = "")
  }
  return(relativev)
}

isBest <- function(c, vs, moreBetter){
  if(is.null(vs) || is.nan(vs) || is.na(vs) || is.null(c) || is.nan(c) || is.na(c))
    return(FALSE)
  
  if(moreBetter)
    return(c >= max(vs[!is.nan(vs)]))
  return(c <= min(vs[!is.nan(vs)]))
}

highlighBest <- function(value,c, vs, total=NULL, includeRank=TRUE, moreBetter=TRUE, usePhantom=TRUE){
  rankValue = NULL
  fv <- value
  if(is.null(fv)){
    fv <- paste(formatC(c, digits = 1, format = "f"), sep = "")
    if(!is.null(total)){
      fv <- paste(formatC( (c / total)*100, digits = 1, format = "f"), "\\%",sep = "")
    }else{
      ph <- appendPhantom(usePhantom)
      fv <- paste(fv, ph, sep = "")
    }
  }
  if(includeRank)
    rankValue = getRank(c, vs)
  if(isBest(c, vs, moreBetter))
    return(paste("\\textbf{",appendRankValue(fv, rankValue),"}", sep = ""))
  return(appendRankValue(fv, rankValue))
}

boldValue <- function(value){
  return(paste("\\textbf{", value,"}", sep=""))
}

getRank <- function(c, vs){
  return(rank(-vs,ties.method= "min")[which(c==vs)[1]])
}


formatedValue <- function(value, a12, p){
  if(is.nan(p) | p >= 0.05)
    return(value)
  if(a12 > 0.5)
    return(boldValue(value))
  return(paste("\\textcolor{red}{", value,"}", sep=""))
}


appendRankValue <- function(value, rankValue=NULL){
  if(is.null(rankValue)) return(value)
  return(paste(value," (",rankValue,")", sep=""))
}


lastColumnAppend <- function(){
  return(" \\\\ \n")
}

formatValue <- function(value){
  if(identical(value, "coveredTargets"))
    return("\\#Targets")
  else if(identical(value, "coveredLines"))
    return("\\%Lines")
  else if(identical(value, "coveredBranches"))
    return("\\%Branches")
  else if(identical(value, "faults"))
    return("\\#Faults")
  return(value)
}

printA <- function(x, y){
  
  w = wilcox.test(x, y)
  p = w$p.value
  
  significant = p <= 0.05
  
  if (significant) {cat("{\\bf ")}
  cat(formatC(measureA(x, y), digits = 2, format = "f"), sep = "")
  if (significant) {cat("}")}
}

appendPhantom <- function(usePhantom=TRUE){
  if(usePhantom)
    return("$\\phantom{\\%}$")
  else
    return("")
}

boolean <- function(s){
  if (s == "true") {
    return(T)
  } else if (s == "false") {
    return(F)
  } else {
    return("ERROR")
  }
}

get.cvPER <- function(BigData, verbose = T){
  if(verbose) cat('Getting CV per 10s... ')

  inds <-
    sapply(seq_along(BigData), function(x) sapply(6:1, function(y) ((x-y):(x-y+6-1))+1), simplify = F)

  CVS <-
    do.call(rbind,
      lapply(inds, function(x){
        values <-
          sapply(data.frame(x), function(y){Y <- y[y>0 & y<=length(BigData)]
          if(length(y)!=length(Y)){
            data.frame(CV = NA)
          } else{
            data.frame(CV = cv(BigData[Y]))
          }
          }, simplify = F)

        CV <- sapply(do.call(rbind, values), min, na.rm = T)
        return(CV)
      }))

  if(verbose) cat('Done!\n')
  return(CVS)
}

get.adult.cvPER <- function(BigData, verbose = T){
  if(verbose) cat('Getting CV per 10s... ')

  inds <-
    sapply(seq_along(BigData), function(x) sapply(6:1, function(y) ((x-y):(x-y+6-1))+1), simplify = F)

  CVS <-
    do.call(rbind,
      lapply(inds, function(x){
        values <-
          sapply(data.frame(x), function(y){Y <- y[y>0 & y<=length(BigData)]
          if(length(y)!=length(Y)){
            data.frame(CV = NA)
          } else{
            data.frame(CV = cv(BigData[Y]))
          }
          }, simplify = F)

        CV <- sapply(do.call(rbind, values), min, na.rm = T)
        return(CV)
      }))

  if(verbose) cat('Done!\n')
  return(CVS)
}

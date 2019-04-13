# define data
dat <- c(1.0000000000, 0.001996328, 0.000176308, 0.0002305861, 0.1514324000,
          0.0019963281, 1.000000000, 0.007106454, 0.409054300, 0.001210349,
          0.0001763080, 0.007106454, 1.000000000, 0.217609400, 0.185434400,
          0.0002305861, 0.409054269, 0.217609401, 1.000000, 1.972118e-09,
          0.1514324468, 0.001210349, 0.185434396, 1.972118e-09, 1.000000)

# transform into matrix and set colnames
m <- matrix(dat, nrow=5, ncol=5)
columns <- c("A", "B", "C", "D", "E")
rownames(m) <- columns
colnames(m) <- columns


# function to filter by tolerance
toler_filter <- function(m, toler){
  keep <- (colSums(abs(m) < toler) > 0)
  return(m[keep, keep])
}

toler_filter(m, 0.001)

# filter matrix to find the top values by matrix size
# function to filter by tolerance
size_filter <- function(m, size){
  # return if size is >= matrix passed
  if (size == 1){
    return(m)
    
  } else if (size >= dim(m)[1]){
    return(m)
    
  } else {
    # alter diagonal values
    diag_values <- diag(m)
    diag(m) <- 0.0
    
    # sort unqiue values from highest to lowest
    uni_sort = sort(unique(as.vector(m)), decreasing = TRUE)
    
    for (i in uni_sort){
      keep <- (colSums(abs(m) >= i) > 0)
      if (sum(keep)>=size){
        break
      }
    }
    # return diagonal to original
    diag(m) <- diag_values
    return(m[keep, keep])
  }
}

size_filter(m, 3)

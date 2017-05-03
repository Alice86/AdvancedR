# 修改combn2,出matrix
# 逐条运行调试，学习算法思想
combn2<-function (x, m, FUN = NULL, simplify = TRUE, ...) 
{ # x must be a vector of integers
  # m must be a integer greater than 0 & less than the length of x
       count <- as.integer(round(choose(n, m))) # ncol=no.of binomial coefficients
       n <- length(x) # 1:n are SN for elements in x
       # idea: use SN to compute combn methods in output, 
       #       then subset

       ## initialize
       a <- seq_len(m) # ? 1:m are SN in first combn
       r <- x[a] # use SN a to subset the combn
       out <- matrix(r, nrow = m, ncol = count) # first row and determine space
       h <- m # how many new elements are assigned to the previous a
       e <- 0
       i <- 2L # ith row in 'out'
       
       ## loop
       nmmp1 <- n - m + 1L  # largest possible SN to start an 'a'  
       while (a[1L] != nmmp1) { 
                     # e+h < n  i.e.  last SN in a < largest possible SN 
                     if (e < n - h) {
                            h <- 1L # change only the last one
                            e <- a[m] 
                            j <- 1L  # the last SN add 1
                     }
                     else {
                            e <- a[m - h] 
                            h <- h + 1L # change h elements
                            j <- 1L:h # change the last h elements
                     }
                     a[m - h + j] <- e + j # change elements in 'a', in sequence
                     r <- x[a] # turn series into elements in x
                     out[, i] <- r
                     i <- i + 1L
              }
       }
       out
}


# original codes in combn
combn<-function (x, m, FUN = NULL, simplify = TRUE, ...) 
{
       stopifnot(length(m) == 1L, is.numeric(m)) #stop if not true
       if (m < 0) 
              stop("m < 0", domain = NA)
       # m is a integer greater than 0 & less than the length of x
       if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == 
           x) 
              x <- seq_len(x)
       # x is a vector of integers
       n <- length(x)
       if (n < m) 
              stop("n < m", domain = NA)
       # m is a integer less than the length of x
       
       x0 <- x # reserve the original attributes if x is not integer
       if (simplify) {
              if (is.factor(x)) # if x is not integer, simplify it
                     x <- as.integer(x)
       }
       
       m <- as.integer(m)
       e <- 0
       h <- m
       a <- seq_len(m)
       nofun <- is.null(FUN)
       if (!nofun && !is.function(FUN)) 
              stop("'FUN' must be a function or NULL")
       # check the 'FUN' argument
       len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
       count <- as.integer(round(choose(n, m)))
       if (simplify) {
              dim.use <- 
                     if (nofun) 
                            c(m, count)
              else {
                     d <- dim(r)
                     if (length(d) > 1L) 
                            c(d, count)
                     else if (len.r > 1L) 
                            c(len.r, count)
                     else c(d, count)
              }
       }
       if (simplify) 
              out <- matrix(r, nrow = len.r, ncol = count)
       else {
              out <- vector("list", count)
              out[[1L]] <- r
       }
       if (m > 0) {
              i <- 2L
              nmmp1 <- n - m + 1L
              while (a[1L] != nmmp1) {
                     if (e < n - h) {
                            h <- 1L
                            e <- a[m]
                            j <- 1L
                     }
                     else {
                            e <- a[m - h]
                            h <- h + 1L
                            j <- 1L:h
                     }
                     a[m - h + j] <- e + j
                     r <- if (nofun) 
                            x[a]
                     else FUN(x[a], ...)
                     if (simplify) 
                            out[, i] <- r
                     else out[[i]] <- r
                     i <- i + 1L
              }
       }
       if (simplify) {
              if (is.factor(x0)) {
                     levels(out) <- levels(x0) 
                     class(out) <- class(x0)  # return to the oringinal attributes
              }
              dim(out) <- dim.use
       }
       out
}
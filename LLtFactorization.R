
A <- matrix(c(4,1,-1,
              -1,4.25,2.75,
              1,2.75,3.75), ncol = 3, nrow = 3)
b <- c(1,1,1)

LDLt <-  function(A,b)
{
  X <- rep(0,n)
  Y <- rep(0,n)
  Z <- rep(0,n)
  n <- nrow(A)
  L <- matrix(c(0), nrow = n, ncol = n )
  diag(L) <- 1
  D <- rep(0,n) #D <- matrix(c(0), n,n)
  D[1] <- L[1,1]
  L[,1] <- A[,1] / D[1]
  #Adım1
  for(i in 1:(n-1))
  {
    v <-c() #v <- rep(0,i-1)
    #Adım2
    #for(j in 1:(i-1))
    #{
    
    #}
    v <- L[i,1:(n-1)]*D[1:(i-1)]
    #Adım3
    
    D[i] <- A[i,i] - sum(L[i,1:(i-1)]*v[1:(i-1)])
    #Adım4
    for(j in (i+1):n)
    {
      L[j,i] <- (A[j,i] - sum(L[j, 1:(i-1)] * v[1:(i-1)])) / D[i]
    }
  }
  #Adım5
  v <- L[n, 1:(n-1)]*D[1:(n-1)]
  D[n] <- A[n,n]-sum(L[n, 1:(n-1)] * v[1:(n-1)])
  
  #Adım6
  Y <- b[1]
  #Adım7
  for(i in 2:n)
  {
    Y[i] <- (b[i] - sum(L[i,1:(i-1)] * Y[1:(i-1)]))
  }
  #adım8
  for(i in 1:n)
  {
    Z[i] = Y[i]/D[i]
  }
  #Adım9
  X[n] <- Z[n]
  
  #Adım10
  for(i in (n-1):1)
  {
    X[i] <- Z[i] - sum(L[(i+1):n]*X[(i+1):n])
  }
  
  #Adım11
  list(X=X, L=L,D=diag(D))
  
  
  #list(L=L,D=diag(D))
}
LDLt(A,b)
#sayfa 425
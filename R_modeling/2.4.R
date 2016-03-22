hibert <- function(n) 
{
  if (n<=0) return(NA)
  h = matrix(0, nrow=n, ncol=n)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      h[i, j] <- 1/(i+j-1)
    }
  }
  return(h)
}
h = hibert(5)
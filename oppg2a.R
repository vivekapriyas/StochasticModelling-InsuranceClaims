
n1 = 1000 #total simulations to estimate P{X(t)<100}
n2 = 10 #simulations to plot
lambda = 1.5 #rate parameter
t = 59 #days until March 1st
mu = lambda*t

#part 1
#vector to store number of claims
Xt = vector('numeric',length = n1)

for (i in 1:n1){
  xt = rpois(1,lambda=mu) #number of claims is Poisson distributed
  Xt[i]=xt
   
}

#estimates P{X(59)>100} as proportion of simulations where Xt[i]>100
P = length(Xt[Xt>100])/n1
print(c("EstimatedP{X(59)>100}:",P))


#part 2
#plots 10 realizations of {X(t):t=0,...,59}
plot(NULL, NULL, xlim = c(0, t), ylim = c(0, 110), xlab = "Days", ylab = "Number of claims", main = "X(t) : t=0,...,59", lwd = 2)

x <- c(0, t)

#boundaries for confidence bound, 2 times standard deviation
sdplus <- c(0, mu+2*sqrt(mu))
sdminus <- c(0, mu-2*sqrt(mu))
polygon(c(x, rev(x)), c(sdminus, rev(sdplus)), col = "snow2", border = NA)


colors = c('antiquewhite3','aquamarine2','chartreuse3','cornflowerblue','darkmagenta','darkorange3','darkslategray','gray15','hotpink1')

for (j in 1:n2){
  xt <- rpois(1,lambda = mu) 
  W <- sort(runif(n = xt, min = 0, max = t)) #waiting times are uniformly distributed, sorted in ascending order
  x_val <- c(0:xt, xt) 
  
  for (k in 1:(xt-1)){
    lines(W[k:(k+1)], rep(x_val[k],2), col = colors[j], lwd = 2)
  }
  lines(c(W[xt], t),rep(x_val[xt],2), col = colors[j], lwd = 2)
}


#mean and legend
lines(x, c(0, mu), col = "red", lwd = 2)
legend('topleft', col = c("red", "snow2"), legend = c("Mean", "Condfidence bound, 2 SD"), lwd = 2) 


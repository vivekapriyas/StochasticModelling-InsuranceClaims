
n = 1000 #simulations
t = 59 #days
lambda = 1.5 #rate of Poisson
mu = lamba*t
gamma = 10 #rate of exponential

#vector to store total claim amounts
Z = vector('numeric',length=n)

for (i in 1:n){
  xt = rpois(1,lambda=mu)
  c = rexp(xt,rate=gamma)
  Z[i] = sum(c)
}

E_Z = mean(Z)
Var_Z = var(Z)

print(c('Expected total claim amount by March 1st:',round(E_Z,3)))
print(c('Variance of total claim amount by March 1st:',round(Var_Z,3)))

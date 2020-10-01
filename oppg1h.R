
days <- 300 #timesteps
gamma <- 0.1 #P_(23)
alpha <- 0.01 #P_(31)
N <- 1000 #population size
n <- 1000 #simulations

#vectors to store maximum infected value and timesteps until it is reached
maxIn = vector('numeric',length=n)
tToMaxI = vector('numeric',length=n)


for (j in 1:n){
  #initializes vector with starting distribution
  Y_n <- vector("numeric", length = 3)
  Y_n[1] <- 950 
  Y_n[2] <- 50 
  
  #initializes maxI and timesteps
  maxI = 50
  tsteps = 0
  
  for (i in 1:(days-1)){
    beta = Y_n[2]/(2*N) #P_(12)
    
    StoI = rbinom(1,Y_n[1],beta)
    ItoR = rbinom(1,Y_n[2],gamma)
    RtoS = rbinom(1,Y_n[3],alpha)
    
    Y_n[1] = Y_n[1] - StoI + RtoS
    Y_n[2] = Y_n[2] - ItoR + StoI
    Y_n[3] = Y_n[3] - RtoS + ItoR
    
    #checks and updates max and timesteps
    if (maxI < Y_n[2]){
      maxI = Y_n[2]
      tsteps = i
    }
  }
  
  #stores values for current simulation
  maxIn[j]=maxI
  tToMaxI[j]=tsteps
}

#Mean as estimates of expected values
E_MaxI = mean(maxIn)
E_tToMaxI = mean(tToMaxI)

print(c('Expected maximum number of individuals:',round(E_MaxI,2)))
print(c('Expected time until maximum number of individuals is reached:',round(E_tToMaxI,2)))
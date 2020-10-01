
n = 36500 #days
alpha = 0.01 #P_(31)
gamma = 0.1 #P_(23)
N = 1000 #population size

#initializes matrix to store states with Y_o. (S,I,R)=(1,2,3)
Y_n = matrix(0,nrow=3,ncol=n+1)
Y_n[1,1]=950
Y_n[2,1]=50

for (i in 1:n){
  beta = Y_n[2,i]/(2*N) #P_(12)
  
  StoI = rbinom(1,Y_n[1,i],beta)
  ItoR = rbinom(1,Y_n[2,i],gamma)
  RtoS = rbinom(1,Y_n[3,i],alpha)
  
  Y_n[1,i+1] = Y_n[1,i] - StoI + RtoS
  Y_n[2,i+1] = Y_n[2,i] - ItoR + StoI
  Y_n[3,i+1] = Y_n[3,i] - RtoS + ItoR
}

#finds proportion in each state in the long run
#first 50 timesteps omitted because of transient phase
S = mean(Y_n[1,50:n+1])/N
I = mean(Y_n[2,50:n+1])/N
R = mean(Y_n[3,50:n+1])/N

print(c('Proportion of individuals in state S in the long run:',round(S,3)))
print(c('Proportion of individuals in state I in the long run:',round(I,3)))
print(c('Proportion of individuals in state R in the long run:',round(R,3)))
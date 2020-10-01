
n = 300 #days
N = 1000 #total population size
gamma = 0.1 #P_(23)
alpha = 0.01 #P_(31)

Y_n = matrix(0,nrow=3,ncol=n+1)
Y_n[1,1]=950
Y_n[2,1]=50

for (i in 1:n){
  beta = Y_n[2,i]/(2*N)
  
  StoI = rbinom(1,Y_n[1,i],beta)
  ItoR = rbinom(1,Y_n[2,i],gamma)
  RtoS = rbinom(1,Y_n[3,i],alpha)
  
  Y_n[1,i+1] = Y_n[1,i] - StoI + RtoS
  Y_n[2,i+1] = Y_n[2,i] - ItoR + StoI
  Y_n[3,i+1] = Y_n[3,i] - RtoS + ItoR
}

rownames(Y_n)=c('S','I','R')
df <- reshape2::melt(Y_n) 
colnames(df)=c('State','Day','Individuals')

ggplot(df, aes(x = Day, y = Individuals, bye = State, col = State)) + geom_point() + ggtitle("Number of individuals in each state for 300 timesteps")

                      
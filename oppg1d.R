
days = 365 #days
yrs= 50
n=days*yrs
alpha = 0.01 #P_(31)
beta = 0.05 #P_(12)
gamma = 0.1 #P_(23)

#initalizes transition matrix P:
P = matrix(0,nrow= 3,ncol = 3)
P[1,1]=1-beta
P[1,2]=beta
P[2,2]=1-gamma
P[2,3]=gamma
P[3,1]=alpha
P[3,3]=1-alpha


x = vector('numeric',length=2) #vector to save current and next state
x[1]=1

#vectors to store timesteps between occurrences
tToI = vector('numeric',length=n)
tToR= vector('numeric',length=n)
tToS = vector('numeric',length=n)

c = 1 #number of current cycle 

for (i in 1:n){
  p = runif(1)
  
  #finds next state
  if(p <= P[x[1],1]){
    x[2] = 1
  }
  else if(p> (1-P[x[1],2])){
    x[2]=2
  }
  else{
    x[2] = 3
  }
  
  #counts timesteps depending on state
  if(x[1] == 1){
    tToI[c] = tToI[c]+1
    tToR[c] = tToR[c]+1
    tToS[c] = tToS[c]+1
  }
  if(x[1]==2){
    tToR[c] = tToR[c]+1
    tToS[c] = tToS[c]+1
  }
  if(x[1]==3){
    tToS[c] = tToS[c]+1
    
    if(x[2]==1){
      c = c+1 #updates number of current cycle when individual reaches state 1 
    }  
  }
  x[1]=x[2]
}



#estimates expected values by mean
E_tToI = mean(tToI[tToI>0])
E_tToR = mean(tToR[tToR>0])
E_tToS = mean(tToS[tToS>0])

print(c('Estimated time til X_n=I | X_0=S:',round(E_tToI,2)))
print(c('Estimated time til X_n=R | X_0=S:',round(E_tToR,2)))
print(c('Estimated time til X_n=S | X_(n-1)=R,X_0=S:',round(E_tToS,2)))



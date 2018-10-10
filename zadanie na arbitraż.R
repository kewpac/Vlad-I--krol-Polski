#zadanie na arbitra¿
#min k0
cnf2=function(S,sigma,N,K,r,T){
  #browser()
  h=T/N 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h))/(u-d)
  p=1-q
  k0=floor((log(K/S)-N*log(d))/(log(u/d)))+1
  A=exp(-r*T)*(1-pbinom(k0-1,N,prob =1-(exp(r*h)-d)/(u-d)))
  B=exp(-r*T)*(1-pbinom(k0-1,N,prob=1-((exp(r*h)-d)/(u-d))*u))  
  A*S-K*exp(-r*T)*B
}


cnf3=function(S,sigma,N,K,r,T){
#browser()
  h=T/N 
u=exp(sigma*sqrt(h))
d=exp(-sigma*sqrt(h))
q=(exp(r*h)-d)/(u-d)
k0=floor((log(K/S)-N*log(d))/(log(u/d)))+1
A=0
for(i in k0:N){
  A=A+dbinom(i,N,q*u)}#^i*(1-q)^(N-i)}
A=A*exp(-r*T)
B=0
for(i in k0:N){
  B=B+dbinom(i,N,q)}
B=B*exp(-r*T)
A*S-K*exp(-r*T)*B
}
cnf3(100,0.2,100,90,0.1,0.5)
bermudacall(100,0.2,4,90,0.1,0.5,5)

pnf3=function(S,sigma,N,K,r,T){
  #browser()
  h=T/N 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  k0=floor((log(K/S)-N*log(d))/(log(u/d)))+1
  A=0
  for(i in k0:N){
    A=A+dbinom(i,N,q*u)}#^i*(1-q)^(N-i)}
  A=A*exp(-r*T)
  B=0
  for(i in k0:N){
    B=B+dbinom(i,N,q)}
  B=B#*exp(-r*T)
  (A-1)*S+K*exp(-r*T)*(1-B)
}
pnf3(100,0.2,100,90,0.1,0.5)

cnf3(100,0.2,100,90,0.1,0.5)-pnf3(100,0.2,100,90,0.1,0.5)
c=NULL
for(i in 1:20){c[i]=cnf3(100,0.2,i,90,0.1,0.5)}
plot(c)
lines(c)
bermudacall(100,0.2,50,90,0.1,0.5)
cnf3(100,0.2,100,90,0.1,0.5)


?pbinom





?pbinom()

cnf2=function(S,sigma,N,K,r,T){
  #browser()
  h=T/N 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  p=1-q
  k0=floor((log(K/S)-N*log(d))/(log(u/d)))+1
  A=exp(-r*T)*(1-pbinom(k0,N,prob=1-q))
  B=exp(-r*T)*(1-pbinom(k0,N,prob=1-q*u))  
  A*S-K*exp(-r*T)*B
}

#according to call-put parity
#C-P=S0-Kexp(-rT)
#P=C-S0+Kexp(-rT)
#C=AS0-Kexp(-rT)B
#P=(A-1)S0-Kexp(-rT)(1-B)

pnf2=function(S,sigma,N,K,r,T){
  #browser()
  h=T/N 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h))/(u-d)
  p=1-q
  k0=floor((log(K/S)-N*log(d))/(log(u/d)))+1
  A=exp(-r*T)*(1-pbinom(k0,N,prob =1-(exp(r*h)-d)/(u-d)))
  B=exp(-r*T)*(1-pbinom(k0,N,prob=1-((exp(r*h)-d)/(u-d))*u))  
  (A-1)*S+K*exp(-r*T)*(1-B)
}

#wartoœæ call americano w chwili t to St-K, w momencie T jest to max(ST-K,0)
# co z putem? mo¿emy za³o¿yæ ¿e osoba posiadaj¹ca put wykona j¹ gdy wartoœæ Puta bêdzie powy¿ej 
#oczekiwanej wartoœci od t do T
#okej zatem tworzymy wektor który bêdzie lecia³ od koñca, 
#jesli jest T kroków to wiemy, ¿e w T wartoœæ jest max ST-K, 0
#w T-1 mamy max z dzisiejszec ceny akcji i wartoœci oczekiwanej tego co bêdzie,
# zatem maj¹c wartosc w i tym okresie wartoœæ w i-1 to max wartoœæ oczekiwana z itych
#i z tego co jest w tym momencie

# mam funkcje licz¹c¹ wartoœæ calla policzê go na koniec okresu 
# u i d mam (bêdê mia³), wiêc na samym koñcu bêdze 


#ostatnich bêdzie T
# bêd¹ to u^t, u^t(T-1)d, u^(T-2)d^2,...., d^T

amerikanocall=function(S,sigma,N1,K,r,t)
{
  h=t/N1 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  valfa=NULL
  
  for(i in 0:N1){
    valfa[i+1]=u^(N1-i)*d^(i)  
  }
  valfa=S*valfa
  vend=NULL
  #pocz¹tkowy wektor koñcowy wartoœci w chwli T
  for(i in 1:N1){
    vend[i]=max(valfa[i]-K,0)
  }
  wartosc=vend
  for(j in 1:(N1-1))
  {
    N=N1-j 
    
    Z2=NULL
    for(i in 0:(N-1))
    {
      Z2[i+1]=u^(N-i)*d^(i)  
    }
    Z2=S*Z2
    
    Z3=NULL
    for(i in 1:N){
      Z3[i]=(q*wartosc[i]+(1-q)*wartosc[i+1])*exp(-r*h)
    }
    Z4=NULL
    for(i in 1:(N))
    {
      Z4[i]=max(max(Z3[i],Z2[i]-K),0)
    }
    wartosc=Z4#*exp(-r*(T-(j+1)*h/N1))
  }
  return(wartosc)
}





#utworzyæ wektor Z[] T-1 elementowy:
#i=ty element to bêdzie Z[i]*q+z[i+1]*(1-q)

# od i=1 do k

#q(i)=E()
amerikanoput=function(S,sigma,N1,K,r,t)
{
  h=t/N1 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  valfa=NULL
  
  for(i in 0:N1){
    valfa[i+1]=u^(N1-i)*d^(i)  
  }
  valfa=S*valfa
  vend=NULL
  #pocz¹tkowy wektor koñcowy wartoœci w chwli T
  for(i in 1:N1){
    vend[i]=max(K-valfa[i],0)
  }
  wartosc=vend
  for(j in 1:(N1-1))
  {
    N=N1-j 
    
    Z2=NULL
    for(i in 0:(N-1))
    {
      Z2[i+1]=u^(N-i)*d^(i)  
    }
    Z2=S*Z2
    
    Z3=NULL
    for(i in 1:N){
      Z3[i]=(q*wartosc[i]+(1-q)*wartosc[i+1])*exp(-r*h)
    }
    Z4=NULL
    for(i in 1:(N))
    {
      Z4[i]=max(max(Z3[i],K-Z2[i]),0)
    }
    wartosc=Z4#*exp(-r*(T-(j+1)*h/N1))
  }
  return(wartosc)
}



bermudacall=function(S,sigma,N1,K,r,t,wector=c(1:N1))
{
  h=t/N1 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  valfa=NULL
  #browser()
  for(i in 0:N1){
    valfa[i+1]=u^(N1-i)*d^(i)  
  }
  valfa=S*valfa
  vend=NULL
  #pocz¹tkowy wektor koñcowy wartoœci w chwli T
  for(i in 1:(N1+1)){
    vend[i]=max(valfa[i]-K,0)
  }
  wartosc=vend
  for(j in 1:(N1))
  {
    N=N1-j+1 
    
    Z2=NULL
    for(i in 0:(N-1))
    {
      Z2[i+1]=u^(N-i)*d^(i)  
    }
    Z2=S*Z2
    
    Z3=NULL
    for(i in 1:N){
      Z3[i]=(q*wartosc[i]+(1-q)*wartosc[i+1])*exp(-r*h)
    }
    Z4=NULL
    for(i in 1:N)
    {
      if(any(wector==N)==1){Z4[i]=max(max(Z3[i],Z2[i]-K),0)}
      else{Z4[i]=Z3[i]}
      
    }
    wartosc=Z4#*exp(-r*(T-(j+1)*h/N1))
  }
  return(wartosc)
}
bermudacall(10,0.2,100,10,0.02,1,c(101))
#S 50
#sigma 0.6
#N 1000
#K 52
#r 0.1
#t 2
S 100
K 90
r=0.1
T=0.5
N=60
sigma=0.2
bermudacall(100,0.2,60,90,0.1,0.5)
cnf3(100,0.2,60,90,0.1,0.5)



cnf2(50,0.6,720,52,0.1,2)
pnf2(50,0.6,720,52,0.1,2)
bermudacall(50,0.6,720,52,0.1,2,730)
amerikanocall(100,0.2,60,90,0.1,0.5)
bermudaput(50,0.6,720,52,0.1,2,730)
amerikanoput(100,0.2,60,90,0.1,0.5)

bermudaput=function(S,sigma,N1,K,r,t,wector=c(1:N1))
{
  h=t/N1 
  u=exp(sigma*sqrt(h))
  d=exp(-sigma*sqrt(h))
  q=(exp(r*h)-d)/(u-d)
  valfa=NULL
  
  for(i in 0:N1){
    valfa[i+1]=u^(N1-i)*d^(i)  
  }
  valfa=S*valfa
  vend=NULL
  #pocz¹tkowy wektor koñcowy wartoœci w chwli T
  for(i in 1:N1){
    vend[i]=max(K-valfa[i],0)
  }
  wartosc=vend
  for(j in 1:(N1-1))
  {
    N=N1-j 
    
    Z2=NULL
    for(i in 0:(N-1))
    {
      Z2[i+1]=u^(N-i)*d^(i)  
    }
    Z2=S*Z2
    
    Z3=NULL
    for(i in 1:N){
      Z3[i]=(q*wartosc[i]+(1-q)*wartosc[i+1])*exp(-r*h)
    }
    Z4=NULL
    for(i in 1:(N))
    {
      if(any(wector==i)==1){Z4[i]=max(max(Z3[i],K-Z2[i]),0)}
      else{Z4[i]=Z3[i]}
      
    }
    wartosc=Z4#*exp(-r*(T-(j+1)*h/N1))
  }
  return(wartosc)
}



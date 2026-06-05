conditionnal_test_under_null = function(a,n,b){
  quant = qbinom(1-a, n, 1/2, lower.tail = TRUE, log.p = FALSE)
  tab = rbinom(b, n, 1/2)
  x=0:n
  mass_fuction = dbinom(x, n, 1/2, log = FALSE)
  hist(tab,probability=TRUE,breaks = seq(min(tab)-0.5, max(tab)+0.5, by = 1))
  lines(x,mass_fuction,type ="h",col="red")
  for (i in 1:b){
    if (tab[i]>quant){
      tab[i]=1
    }
    else{
      tab[i]=0
    }
  }
  risk_exp=sum(tab)/b
  print(quant)
  print(tab)
  return(risk_exp)
}

conditionnal_test_under_null(0.1,100,10000)

#Variation du risque de première espèce empirique en fonction du niveau du test 
vect_a=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a = c(vect_a,conditionnal_test_under_null(a,100,10000))
}
plot(x,vect_a)
abline(0,1,col='red')

unconditionnal_test = function(a,p,b){
  Y1 = rpois(b,p[1])
  Y2 = rpois(b,p[2])
  vect_Y1=c()
  for(i in 1:b){
    vect_Y1 = c(vect_Y1,Y1[i])
    n = Y1[i]+Y2[i]
    quant = qbinom(1-a, n, 1/2, lower.tail = TRUE, log.p = FALSE)
    if(vect_Y1[i] > quant){
      vect_Y1[i]=1
    }
    else{
    vect_Y1[i]=0
    }
  }
return(sum(vect_Y1)/b)
}

unconditionnal_test(0.05,c(10,10),10000)

#Variation de la probabilité de rejeter empirique en fonction du niveau du test 
vect_a2=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a2 = c(vect_a2,unconditionnal_test(a,c(8,10),10000))
}
plot(x,vect_a2)

#fonction qui calcul distribution de la p-values, génére B p-values indépendante 
conditionnal_pvalue_under_null = function(n,b,l){
  vect_pval=c()
  vect_Bpval=c()
  vect_repF=c()
  vect_repE=c()
  tab = rbinom(b,n,1/2)
  support = 0:n
  for(k in support){
    pi = 0
    for(i in k:n){
      pi = pi + choose(n,i)*1/2**n
    }
    vect_pval = c(vect_pval,pi)
  }
  for(i in tab){
    vect_Bpval=c(vect_Bpval,vect_pval[match(i,support)])
  }
  for(t in rev(vect_pval)){
    pi <- 0
    for(k in support){
      if(vect_pval[k+1] <= t){
        pi <- pi +
          choose(n,k)*(l/(l+l))^k*(l/(l+l))^(n-k)
      }
    }
    vect_repF <- c(vect_repF, pi)
  }
  for(i in rev(vect_Bpval)){
    pi=0
    pi <- mean(vect_Bpval <= i)
    vect_repE = c(vect_repE,pi)
  }
  par(mfrow = c(1,2))
  
  plot(support,vect_pval,xlab = "k",ylab = "p(k)",main = "P-values théoriques")
  #lines(vect_Bpval,type='p',col='blue')

  t_vals = sort(unique(vect_pval))
  vect_repF_unique = sapply(t_vals,function(t) sum(dbinom(support, n, 1/2)[vect_pval <= t]))
  F_theo = stepfun(t_vals,c(0, vect_repF_unique))
  F_emp = ecdf(vect_Bpval)
  plot(F_theo,do.points = FALSE,verticals = TRUE,xlim = c(0,1),ylim = c(0,1),xlab = "t",ylab = "F(t)")
  abline(0, 1, col = "red", lwd = 2)
  lines(F_emp,verticals = TRUE,do.points = FALSE,lty = 2,type ='l')
}
conditionnal_pvalue_under_null(1000,100,3)

unconditionnal_pvalue = function(a,l1,l2,b){
  Y1 = rpois(b,l1)
  Y2 = rpois(b,l2)
  Y1_Y2 = Y1+Y2
  vect_pval = c()
  for(i in 1:b){
    pv=0
    for(j in Y1[i]:Y1_Y2[i]){
      pv = pv+choose(Y1_Y2[i],j)*1/2**Y1_Y2[i]
    }
    vect_pval = c(vect_pval,pv)
  }
  for (i in 1:b){
    if (vect_pval[i]<=a){
      vect_pval[i]=1
    }
    else{
      vect_pval[i]=0
    }
  }
  risk_exp=sum(vect_pval)/b
  return(risk_exp)
}
unconditionnal_pvalue(0.05,3,2,1000)

#visualisation du risque de rejet epirique en fonction de la valeur de l1
vect_risk = c()
x=seq(0, 40, by = 1)
for(i in x){
  vect_risk=c(vect_risk,unconditionnal_pvalue(0.05,i,20,1000))
}
plot(x,vect_risk)

#visualisation du risque de rejet epirique en fonction du niveau du test
vect_a=c()
x=seq(0, 1, by = 0.05)
for(a in x){
  vect_a = c(vect_a,unconditionnal_pvalue(a,2,3,1000))
}
plot(x,vect_a)

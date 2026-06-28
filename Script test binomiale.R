conditionnal_test_under_null = function(a,n,b){
  quant = qbinom(1-a, n, 1/2, lower.tail = TRUE, log.p = FALSE)
  tab = rbinom(b, n, 1/2)
  x=0:n
  mass_fuction = dbinom(x, n, 1/2, log = FALSE)
  hist(tab,probability=TRUE,breaks = seq(min(tab)-0.5, max(tab)+0.5, by = 1),title ="")
  lines(x,mass_fuction,type ="h",col="red")
  tab <- ifelse(tab > quant, 1, 0)
  risk_exp=sum(tab)/b
  print(quant)
  print(tab)
  return(risk_exp)
}

conditionnal_test_under_null(0.1,100,10000)

#Variation du risque de première espèce empirique en fonction du niveau du test 
par(mfrow = c(1,1))
vect_a=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a = c(vect_a,conditionnal_test_under_null(a,100,10000))
}
plot(x,vect_a,ylab = "risque empirique",xlab="niveau du test")
abline(0,1,col='red')

unconditionnal_test = function(a,p,b){
  Y1 = rpois(b,p[1])
  Y2 = rpois(b,p[2])
  vect_Y1=c()
  n <- Y1 + Y2
  quant <- qbinom(1 - a, n, 1/2)
  
  vect_Y1 <- as.integer(Y1 > quant)
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
  vect_pval <- 1 - pbinom(support - 1, n, 0.5)
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
  
  plot(vect_Bpval,type='p',col='blue')

  t_vals = sort(unique(vect_pval))
  vect_repF_unique = sapply(t_vals,function(t) sum(dbinom(support, n, 1/2)[vect_pval <= t]))
  F_theo = stepfun(t_vals,c(0, vect_repF_unique))
  F_emp = ecdf(vect_Bpval)
  plot(F_theo,do.points = FALSE,verticals = TRUE,xlim = c(0,1),ylim = c(0,1),xlab = "t",ylab = "F(t)")
  abline(0, 1, col = "red", lwd = 2)
  lines(F_emp,verticals = TRUE,do.points = FALSE,lty = 2,type ='l')
}
conditionnal_pvalue_under_null(1000,1000,3)

unconditionnal_pvalue = function(a,l1,l2,b){
  Y1 = rpois(b,l1)
  Y2 = rpois(b,l2)
  Y1_Y2 = Y1+Y2
  vect_pval = 1 - pbinom(Y1 - 1, Y1_Y2, 0.5)
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

#Visualisation probléme test multiple:
multitest_problem <- function(alpha = 0.05, m_max = 100, B = 2000){
  
  one_test <- function(){
    unconditionnal_pvalue(alpha, 10, 10, 1)
  }
  
  m_vals <- 1:m_max
  risk <- numeric(length(m_vals))
  
  for(m in m_vals){
    
    risk[m] <- mean(replicate(B, {
      any(replicate(m, one_test()) == 1)
    }))
    
  }
  
  plot(m_vals, risk, type="l", lwd=2,
       xlab="Nombre de tests m",
       ylab="P(au moins un rejet)",
       main="Problème des tests multiples")
  
  abline(h=alpha, col="red", lty=2)
  
  return(risk)
}
multitest_problem(0.05,100,100)

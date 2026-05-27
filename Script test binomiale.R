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

?rpois
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
'print(vect_n00)' 
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

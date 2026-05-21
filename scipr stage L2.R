conditionnal_test_under_null = function(a,n,n0.,n.0,b){
  quant = qhyper(1-a, n.0, n-n.0,n0., lower.tail = TRUE, log.p = FALSE)
  tab = rhyper(b,n.0,n-n.0, n0.)
  x=0:(n-n.0)
  mass_fuction = dhyper(x,n.0,n-n.0,n0.,log = FALSE)
  hist(tab,probability=TRUE,breaks = seq(min(tab)-0.5, max(tab)+0.5, by = 1))
  lines(x,mass_fuction,type ="h",col="red")
  for (i in 1:b){
    if (tab[i]>=quant){
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

conditionnal_test_under_null(0.05,100,20,50,1000)

#Variation du risque de première espèce empirique en fonction du niveau du test 
vect_a=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a = c(vect_a,conditionnal_test_under_null(a,100,20,50,1000))
}
plot(x,vect_a)

#Variation du risque de première espèce empirique en fonction de n0.     
vect_n0.=c()
x_2=seq(0, 100, by = 10)
for(n0. in seq(0, 100, by = 10)){
  vect_n0. = c(vect_n0.,conditionnal_test_under_null(0.1,100,n0.,50,1000))
}
plot(x_2,vect_n0.)

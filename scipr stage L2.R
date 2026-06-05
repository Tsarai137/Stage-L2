#Fonction qui répète b fois le test conditionnel sous l'hypothése nul
conditionnal_test_under_null = function(a,n,n0.,n.0,b){
  quant = qhyper(1-a, n.0, n-n.0,n0., lower.tail = TRUE, log.p = FALSE)
  tab = rhyper(b,n.0,n-n.0, n0.)
  x=0:(n-n.0)
  mass_fuction = dhyper(x,n.0,n-n.0,n0.,log = FALSE)
  hist(tab,probability=TRUE,breaks = seq(min(tab)-0.5, max(tab)+0.5, by = 1))
  lines(x,mass_fuction,type ="h",col="red")
  risk_exp <- mean(tab > quant)
  print(quant)
  print(tab)
  return(risk_exp)
}

conditionnal_test_under_null(0.05,100,20,50,1000)

#Variation du risque de première espèce empirique en fonction du niveau du test 
vect_a=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a = c(vect_a,conditionnal_test_under_null(a,100,20,50,10000))
}
plot(x,vect_a)
abline(0,1,col='red')

#Variation du risque de première espèce empirique en fonction de n0.     
vect_n0.=c()
x_2=seq(0, 100, by = 10)
for(n0. in seq(0, 100, by = 10)){
  vect_n0. = c(vect_n0.,conditionnal_test_under_null(0.1,100,n0.,50,10000))
}
plot(x_2,vect_n0.)

#Fonction qui répète b fois le test inconditionnel 
unconditionnal_test = function(a,n,p ,b){
  tab = rmultinom(b,n,p)
  vect_n00 = c()
  for(i in 1:b){
    vect_n00 = c(vect_n00,tab[1,i])
    n0. = tab[1,i]+tab[2,i]
    n.0 = tab[1,i]+tab[3,i]
    quant = qhyper(1-a, n.0, n-n.0,n0., lower.tail = TRUE, log.p = FALSE)
    if(vect_n00[i]>quant){
      vect_n00[i]=1
    }
    else{
    vect_n00[i]=0
    }
  }
print(sum(vect_n00)/b)
}

unconditionnal_test(0.05,100,c(0.1,0.1,0.1,0.7),10000)

#Variation de la probabilité de rejeter empirique en fonction du niveau du test 
vect_a2=c()
x=seq(0, 1, by = 0.05)
for(a in seq(0, 1, by = 0.05)){
  vect_a2 = c(vect_a2,unconditionnal_test(a,100,c(0.4,0.3,0.2,0.1),1000))
}
plot(x,vect_a2)

#Variation de la probabilité de rejeter empirique en fonction de la variation de 2 probabilités  
vect_p00=c()
z=seq(0,0.7, by=0.05)
for(p00 in z){
  vect_p00=c(vect_p00,unconditionnal_test(0.05,100,c(p00,0.2,1-p00-0.3,0.1),1000))
}
 plot(z,vect_p00)

#Fonction qui calcule la distribution de la pvalue conditionellement et génére b pvalues indépendante 
#et affiche la fonction de répartirion théorqiue, empirique et uniforme ainsi que les tirages de pvalues 
conditionnal_pvalue_under_null = function(n,n0.,n.0,b){
   vect_pval = c()
   vect_Bpval = c()
   vect_repF = c()
   vect_repE = c()
   support = max(0,n0.+n.0-n):min(n0.,n.0)
   tab = rhyper(b,n.0,n-n.0, n0.)
   for(k in support){
     pk=0
     for(i in k:min(n0.,n.0)){
       pk = pk+choose(n.0,i)*choose(n-n.0,n0.-i)/choose(n, n0.)
     }
     vect_pval=c(vect_pval,pk)
   }
   for(i in tab){
     vect_Bpval = c(vect_Bpval,vect_pval[match(i,support)])
   }
   for(i in rev(vect_pval)){
     pv = 0
     for(pk in vect_pval){
       if(pk<=i){
         j=which(vect_pval==pk)
         pv = pv + choose(n.0,support[j])*choose(n-n.0,n0.-support[j])/choose(n,n0.)
       }
     }
     vect_repF=c(vect_repF,pv)
   }
   vect_repE=ecdf(vect_Bpval)
   vect_repF = unique(vect_repF)
   par(mfrow=c(1,2))
   plot(support,vect_pval,type='l')
   lines(vect_Bpval,col='red',type='p')
   lines(support,runif(length(support), min = 0, max = 1),type="p",col="blue")
   f = stepfun(rev(vect_pval), c(0, vect_repF))
   plot(c(0,1), c(0,1),type="n",xlab="t",ylab="F(t)")
   uniform = punif(support, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
   lines(support,uniform,col="red")
   lines(vect_repE,col="pink")
   lines(f,col="green")
 }
conditionnal_pvalue_under_null(100,40,40,10000)
?stepfun 
?ecdf


rm(list = ls())
library(ggplot2)


#### Question 1 ####

#les notations:
#t_interval: les étudiants arrivent à la cantine selon un processus de Poisson de paramètre 1, i.e. l'intervalle du temps où deux étudiants arrivent suit une loi exponentielle de paramètre 1

#t_arrival: les temps où les étudiants arrivent

#t_servi: suit une loi exponentielle de moyenne 0.75, i.e. la loi exponentielle de paramètre 4/3

# caculate waiting time for each student and simulate

set.seed(1)

nSim = 10000



N = numeric(nSim)

  for(i in (1: nSim)){
    n = 200
    t = 1
    sum_t_interval = 0
    sum_t_servi = 0
    t_wait = numeric(n)
    t_wait[1] = 0
    while(t < n){
      t_interval = rexp(1)
      t_servi = rexp(1, rate = 4/3)
      sum_t_interval = sum_t_interval + t_interval
      sum_t_servi = sum_t_servi + t_servi
      if(sum_t_interval < sum_t_servi){
        t_wait[t+1] = sum_t_servi - sum_t_interval
      }
      else{
        sum_t_servi = sum_t_interval
        t_wait[t+1] = 0
      }
      t = t + 1
    }
    N[i] = sum(t_wait > 5)
  }
   

# encadrer l'erreur pour constuire l'intervalle de confiance (ici au niveau 5%) 
  E_N = mean(N)
  e = qnorm(0.975)*sqrt(var(N))/sqrt(nSim) 


# la loi de N approximativement
ggplot(data=as.data.frame(N))+
  geom_histogram(aes(x=N, y=..density..),fill='deepskyblue4')


#### Question 3 ####

# we imagine that t_interval_obs are our observation values of t_interval of students
t_interval_obs <- rexp(199)

# we assume the prior distribution of unknown parameter is a uniform distribution, and we set the interval of this distribution
a <- seq(0, 5, length.out = 1000)
f_post = numeric()

for (i in (1:1000) ){
      theta = a[i]
      #use bayesian formule to calculate the posterior probability density
      f_post[i] = theta^199*exp(-theta*(sum(t_interval_obs)))} 
 
# find out at which parameter, the posterior proba density is the highest.
plot(a, f_post)
print(a[which.max(f_post)])



#test



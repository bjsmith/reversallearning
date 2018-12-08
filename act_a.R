theta=seq(0,1,0.1)
Act_a_minus_theta<-seq(-10,10,0.1)
RT_0=0
RT_A<-RT_0+1/(1+(exp(Act_a_minus_theta)))

plot(Act_a_minus_theta,RT_A)

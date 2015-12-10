# RDT to Microscopy Probit model

# overall coefficients are alpha = −0.22 (−0.24, −0.21)	beta = 0.97 (0.95, 0.99)
# 
# so, we can predict microscopy from RDT...
# 
# micro = alpha + beta x pnorm(RDT)

rdt_to_micro = function(alpha = -0.22, beta = 0.97, rdt = .5){
  m = alpha + beta*pnorm(RDT)
  return(m)
}
# rdt_to_micro(rdt = .1)
  
RDT = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

micro = rdt_to_micro(rdt = RDT)
micro

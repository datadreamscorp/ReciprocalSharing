library(baryplot)

binom <- function(p, j, l){
  ( factorial(j) / ( factorial(l) * factorial(j - l) ) ) * (p^l) * (1-p)^(j - l)
}

trinom <- function(x, y, n, j, k){
  ( factorial(n) / ( factorial(j) * factorial(k) * factorial(n - j - k) ) ) * (x^j) * (y^k) * (1-x-y)^(n-j-k)
}

r_jk <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5){
  r = 0
  for ( l in 0:(j+1) ) {
    r = r + binom(p, j+1, l) * ( (l*B + (j+1-l)*b)/(n-k) )^gamma
  }
  r
}

s_jk <- function(p, j, k, B = 10, b = 1, gamma = 0.5){
  s = 0
  for ( l in 0:(j+1) ) {
    s = s + binom(p, j+1, l) * ( (l*B + (j+1-l)*b)/(j + 1) )^gamma
  }
  s
}

v_Sjk <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5, c = 0.01, theta = 1.0, w = 0.9){
  (1-w)*( r_jk(p, j, k, n, B=B, b=b, gamma=0.5) - ( (n-1-k)*c )^theta ) + w*( s_jk(p, j, k, B=B, b=b, gamma=gamma) - (j*c)^theta )
}

v_L <- function(p, B = 10, b = 1, gamma = 0.5){
  p*(B^gamma) + (1-p)*(b^gamma)
}

h_jk <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5){
  
  h1 = 0
  h2 = 0
  
  for ( l in 0:j ) {
    h1 = h1 + binom(p, j, l) * ( B + ( (l-1)*B + (j+1-l)*b )/( n - k ) )^gamma
  }
  h1 = p*h1
  
  for ( l in 0:j ) {
    h2 = h2 + binom(p, j, l) * ( b + (l*B + (j-l)*b)/(n-k) )^gamma
  }
  h2 = (1-p)*h2
  
  h = h1 + h2
}

v_Hjk <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5, c = 0.01, theta = 1.0, w = 0.9){
  (1-w)*( h_jk(p, j, k, n, B=B, b=b, gamma=gamma) - ( (n-1-k)*c )^theta ) + w*v_L(p, B=B, b=b, gamma=gamma)
}

w_S <- function(x, y, p, n, B = 10, b = 1, gamma = 0.5, c = 0.01, theta = 1.0, w = 0.9){
  ws = 0
  N = n - 1
  for ( j in 0:(N) ) {
    for ( k in 0:(N-j) ) {
      ws = ws + trinom(x, y, N, j, k) * v_Sjk(p, j, k, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w)
    }
  }
  ws
}

w_H <- function(x, y, p, n, B = 10, b = 1, gamma = 0.5, c = 0.01, theta = 1.0, w = 0.9){
  wh = 0
  N = n - 1
  for ( j in 0:(N) ) {
    for ( k in 0:(N-j) ) {
      wh = wh + trinom(x, y, N, j, k) * v_Hjk(p, j, k, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w)
    }
  }
  wh
}

sharing.game <- function(x, y, z, w0=0, p=0.5, n=10, B=20, b=1, gamma=0.5, c=0.05, theta=2.0, w=0.99){
  w1 <- w_S(x, y, p, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w) + w0
  w2 <- v_L(p, B=B, b=b, gamma=gamma) + w0
  w3 <- w_H(x, y, p, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w) + w0
  return( c(w1,w2,w3) )
}

sharing.game.exp <- function(x, y, z, w0=0, p=0.5, n=10, B=20, b=1, gamma=0.5, c=0.05, theta=2.0, w=0.99){
  w1 <- exp(w_S(x, y, p, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w) + w0)
  w2 <- exp(v_L(p, B=B, b=b, gamma=gamma) + w0)
  w3 <- exp(w_H(x, y, p, n, B=B, b=b, gamma=gamma, c=c, theta=theta, w=w) + w0)
  return( c(w1,w2,w3) )
}



########################################################## GRAFICOS


#################################################### w = 0.99

#c = 0.05, n= 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)

#bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
#bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 50/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)
#bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 75/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 90/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

bary.plotsim(19/20, 0.9/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35)
#bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35)
bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35)
bary.plotsim(19/20, 0.10/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35)

#c = 0.05, n= 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)

#bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
#bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 50/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)
#bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 75/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 90/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

bary.plotsim(19/20, 0.9/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35)
#bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35)
bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35)
bary.plotsim(19/20, 0.10/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35)


#c = 0.03, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)

#bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 40/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 50/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 80/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 90/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

bary.plotsim(19/20, 0.25/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35)
bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35)
bary.plotsim(19/20, 0.75/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35)

#c = 0.03, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)

#bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 50/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
#bary.plotsim(0.5/100, 75/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)
bary.plotsim(0.5/100, 90/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

bary.plotsim(19/20, 0.25/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35)
bary.plotsim(19/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35)
bary.plotsim(19/20, 0.75/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35)



#c = 0.01, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 5/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 20/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 60/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

#c = 0.01, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0, 0.7/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 5/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 20/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 60/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35)



################################ n = 10

#c = 0.05, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=10)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#c = 0.05, n= 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=10)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)



#c = 0.03, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#c = 0.03, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=10)



#c = 0.01, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=10)
#bary.plotsim(1/20, 1/1.2, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=10)

#c = 0.01, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)

bary.plotsim(0.5/100, 2.5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(0.5/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)

bary.plotsim(1/20, 1/10, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)
#bary.plotsim(1/20, 1/1.2, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10)




#################################################### w = 0.90

#c = 0.05, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)

#c = 0.05, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.90)



#c = 0.03, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(3.8/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(4/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)

#c = 0.03, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(3.8/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(4/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.90)



#c = 0.01, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(3.8/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(4/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)

#c = 0.01, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(3.8/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(4/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)

bary.plotsim(2/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)
bary.plotsim(2/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.90)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)

#bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
#bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)
#bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.90)



################################ n = 10

#c = 0.05, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10.2/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)

bary.plotsim(11/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 20/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)

#c = 0.05, n= 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10.2/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)

bary.plotsim(11/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(10/100, 20/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.90)



#c = 0.03, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9.8/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)

bary.plotsim(10/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 20/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)

#c = 0.03, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9.8/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)

bary.plotsim(10/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(10/100, 20/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.90)



#c = 0.01, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9.8/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

bary.plotsim(10/100, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 20/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

#c = 0.01, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9.8/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

bary.plotsim(9/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(8/100, 20/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(7/100, 40/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(5/100, 60/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(3/100, 80/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

bary.plotsim(10/100, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 5/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(10/100, 10/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 20/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 40/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 60/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
bary.plotsim(9/100, 80/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)

#bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
#bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)
#bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.90)




#################################################### w = 0.75

#c = 0.05, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)

bary.plotsim(2/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(4/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(6/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(8/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(12/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(14/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(16/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(17/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(18/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(18/20, 1.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(14/20, 5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(12/20, 7/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 9/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(8/20, 11/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(6/20, 13/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(4/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(2/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(1.5/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(1.25/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

#c = 0.05, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 35, w=0.75)

bary.plotsim(2/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(4/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(6/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(8/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(12/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(14/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(16/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(17/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(18/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(18/20, 1.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
#bary.plotsim(14/20, 5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)

bary.plotsim(12/20, 7/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 9/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(8/20, 11/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(6/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(4/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(2/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(1.5/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(1.25/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)



#c = 0.03, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)

bary.plotsim(2/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(4/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(6/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
#bary.plotsim(8/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
#bary.plotsim(12/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(14/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(16/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(17/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(18/20, 0.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(18/20, 1/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(18/20, 1.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(15/20, 4/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(14/20, 5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(12/20, 7/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(10/20, 9/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(8/20, 11/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(6/20, 13/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(4/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(2/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1.5/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1.25/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

#c = 0.03, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 35, w=0.75)

bary.plotsim(2/20, 0.5/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(4/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(6/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
#bary.plotsim(8/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(10/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
#bary.plotsim(12/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n=35, w=0.75)
bary.plotsim(14/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(16/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(17/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(18/20, 0.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(18/20, 1/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(18/20, 1.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(15/20, 4/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(14/20, 5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)

bary.plotsim(12/20, 7/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(10/20, 9/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(8/20, 11/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(6/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(4/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(2/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1.5/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1.25/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)
bary.plotsim(1/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n=35, w=0.75)



#c = 0.01, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)

bary.plotsim(0.3/20, 19/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1.03/20, 18/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(1.075/20, 18/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 13/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1/20, 12/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1/20, 13/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.9/20, 14/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.8/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.7/20, 15.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 16/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.4/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

#bary.plotsim(0.5/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 17.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

#c = 0.01, n = 35, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(1/100, 99/100, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(3.9/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(4.1/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 35, w=0.75)

bary.plotsim(0.3/20, 19/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1.03/20, 18/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(1.075/20, 18/20, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(2/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1/20, 12/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

bary.plotsim(1/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.9/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.8/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.7/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.4/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)

#bary.plotsim(0.5/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
bary.plotsim(0.5/20, 17.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)




################################ n = 10

#c = 0.05, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(52/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(53/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

#c = 0.05, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(52/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(53/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)
#bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.05, n = 10, w=0.75)




#c = 0.03, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(52/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(53/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

#c = 0.03, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(52/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(53/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.03, n = 10, w=0.75)



#c = 0.01, n = 10
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(47/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(48/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=FALSE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#c = 0.01, n = 10, withcol
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(47/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(48/100, 0, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(0.3/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 19/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(1.15/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(1.075/20, 18/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 16.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.35/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 15.25/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.2/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

bary.plotsim(2/20, 15.15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.1/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15.05/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.95/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.85/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
bary.plotsim(2/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(2/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(1/20, 12/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(1/20, 13/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.9/20, 14/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.8/20, 15/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.7/20, 15.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.5/20, 16/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
#bary.plotsim(0.4/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)

#bary.plotsim(0.5/20, 17/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n=35, w=0.75)
#bary.plotsim(0.5/20, 17.5/20, arrow=FALSE, withcol=TRUE, thegame=sharing.game.exp, c=0.01, n = 10, w=0.75)
################################

#h_jk_REVISAR <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5){
#  h = 0
#  for ( l in 0:(j+1) ) {
#    h = h + binom(p, j+1, l) * ( ( ( l/(j+1) )*( B + ( (l-1)*B + (j+1-l)*b )/( n - k ) )^gamma ) + ( 1 - ( l/(j+1) ) )*( b + (l*B + (j-l)*b)/(n-k) )^gamma )
#  }
#  h
#}

#w_H(0, 0.5, 0.5, 50, B=20, gamma=0.5)
#v_L(0.5, B=20, gamma=0.5)

#library( help=baryplot )
#?bary.plotsim

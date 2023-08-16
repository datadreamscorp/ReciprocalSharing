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

###############################################################################

#c = 0.01
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(0, 2.5/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game)

bary.plotsim(1/20, 1/3, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/2.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/1.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game)

bary.plotsim(1/20, 1/4, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/6, arrow=TRUE, withcol=FALSE, thegame=sharing.game)
bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game)


#c = 0.01, n= 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)

bary.plotsim(1/20, 1/3, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/2.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/1.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)

bary.plotsim(1/20, 1/4, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/6, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, n=35)


#c = 0.01, n= 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(1/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)

bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
#bary.plotsim(1/20, 1/6, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
#bary.plotsim(1/20, 1/4, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(1/20, 1/2.3, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
#bary.plotsim(1/20, 1/1.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)

#bary.plotsim(1/20, 1/4, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
#bary.plotsim(1/20, 1/6, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game.exp, n=35)


#c = 0.03, n = 35
bary.init()
bary.labels("Sharer","Loner","Hoarder")
bary.plotsim(99/100, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/100, 99/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(0.2/10, 0, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(99/100, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(0, 1/100, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)

bary.plotsim(1/20, 1/3, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/2.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/2, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/1.5, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/1.2, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)

bary.plotsim(1/20, 1/4, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/6, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(1/20, 1/10, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)

bary.plotsim(18/20, 1/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(18/20, 0.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)
bary.plotsim(18/20, 1.5/20, arrow=TRUE, withcol=FALSE, thegame=sharing.game, c = 0.03, n = 35)




################################

h_jk_REVISAR <- function(p, j, k, n, B = 10, b = 1, gamma = 0.5){
  h = 0
  for ( l in 0:(j+1) ) {
    h = h + binom(p, j+1, l) * ( ( ( l/(j+1) )*( B + ( (l-1)*B + (j+1-l)*b )/( n - k ) )^gamma ) + ( 1 - ( l/(j+1) ) )*( b + (l*B + (j-l)*b)/(n-k) )^gamma )
  }
  h
}

w_H(0, 0.5, 0.5, 50, B=20, gamma=0.5)
v_L(0.5, B=20, gamma=0.5)

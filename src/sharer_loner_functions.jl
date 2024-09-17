#loner payoff
v_L(p; B=5, b=1, r=0.5) = ( p * B^r ) + ( (1-p) * b^r )

#bernstein polynomial
poly(p, k, l) = ( 
	( factorial(big(k)) / ( factorial(big(l)) * factorial(big(k - l)) ) ) * 
	(p^l) * 
	(1-p)^(k - l)
)

#utility of gains for sharers
u_S(k, l; B=5, b=1, r=0.5) = ( (l/(k+1))*B + (1 - (l/(k+1)))*b )^r

#sequence of payoffs for sharers
r_k(p, k; B=5, b=1, r=0.5) = [poly(p, k+1, l)*u_S(k, l, B=B, b=b, r=r) for l in 0:(k+1)]

#payoff of a sharer in a cluster of k sharers
v_S(p, k; B=5, b=1, C=0.1, r=0.5, f=1.5) = sum( r_k(p, k, B=B, b=b, r=r) ) - (k*C)^f

#expected payoff of sharers in a group of n individuals
w_S(x, n, p; B=5, b=1, C=0.1, r=0.5, f=1.5) = sum(
	[
		poly(x, n-1, k)*v_S(p, k, B=B, b=b, C=C, r=r, f=f)
		for k in 0:1:(n-1)
	]
)
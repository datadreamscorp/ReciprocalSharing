### A Pluto.jl notebook ###
# v0.19.19

using Markdown
using InteractiveUtils

# ╔═╡ 8a22d116-5722-48c1-a34f-3b2ade245213
begin
	using Pkg
	Pkg.activate("..")
	using Revise
	using Plots, Colors
	import ReciprocalSharing as rs
end

# ╔═╡ f2a2b56e-2b26-4673-abb6-ea8ef909bb40
md"
# Reciprocal sharing, risk and the evolution of friendship
#### Alejandro Pérez Velilla & Jorge Peña
"

# ╔═╡ a391201a-cc38-4267-bd7e-ba0f5c04fa76
B = 10; b = 1; C = 0.05; k = 5; r = 0.5; f = 2.5; p = 0.5; n = 15

# ╔═╡ 2f179f34-cdf1-4298-be67-78e3ca9b99e4
begin

	sharer_color = RGBA(0.2, 0.7, 0.5, 1.0)
	loner_color = RGBA(0.7, 0.2, 0.5, 1.0)
	
	p_plot = plot(
		0:0.01:1, 
		rs.v_L.(0:0.01:1, B=B, b=b, r=r), 
		xlab="p", 
		ylab="payoff", 
		label="loner",
		lw=2,
		legend=false,
		color=loner_color
	)
	plot!(
		0:0.01:1, 
		rs.v_S.(0:0.01:1, k, B=B, b=b, C=C, r=r, f=f),
		label="sharer",
		lw=2,
		color=sharer_color
	)
	annotate!(0.25, 2.5, text("k = $k", 8))

	k_plot = hline(
		[rs.v_L(p, B=B, b=b, r=r)],
		label="loner", 
		lw=2, 
		legend=false,
		color=loner_color
	)
	plot!(
		1:1:15, 
		rs.v_S.(p, 1:1:15, B=B, b=b, C=C, r=r, f=f),
		lw=2,
		color=sharer_color
	)
	scatter!(
		1:1:15, 
		rs.v_S.(p, 1:1:15, B=B, b=b, C=C, r=r, f=f),
		label="sharer",
		xlab="k", 
		#ylab="payoff", 
		color=sharer_color
	)
	annotate!(4, 1.95, text("p = $p", 8))
	

	x_plot = hline(
		[rs.v_L(p, B=B, b=b, r=r)], 
		label="loner", 
		lw=2, 
		xlim=(0.0, 1.0),
		color=loner_color
	)
	plot!(
		0:0.01:1, 
		rs.w_S.(0:0.01:1, n, p, B=B, b=b, C=C, r=r, f=f),
		label="sharer",
		xlab="x", 
		ylab="payoff", 
		lw=2,
		color=sharer_color
	)
	annotate!(0.1, 2, text("n = $n \np = $p", 8))

	plot(plot(p_plot, k_plot), x_plot,
		layout=(2,1)
	)
end

# ╔═╡ Cell order:
# ╟─8a22d116-5722-48c1-a34f-3b2ade245213
# ╟─f2a2b56e-2b26-4673-abb6-ea8ef909bb40
# ╠═a391201a-cc38-4267-bd7e-ba0f5c04fa76
# ╟─2f179f34-cdf1-4298-be67-78e3ca9b99e4

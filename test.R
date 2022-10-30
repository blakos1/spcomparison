simulate_composition = function(fract_dim){
  sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
  sim1
}

x = simulate_composition(0.2)
y = simulate_composition(1.2)

plot(x)
plot(y)

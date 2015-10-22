#Script 1 
## Page 31
f = function(x) {2 + cos(x) + sin(2*x)}
f0 = function(x) {f(x) - 2.5}
uniroot(f0, c(0,4),tol = 0.001)
#Question: 
# f.root, init.it,estim.prec

## Page 38
b = loess(y ~ x, span = 0.33)
b
# y and x are not defined


## page 52
b = loess(y ~ x, span = 0.33)
fit0 = fitted(b)
fb = NULL
for (i in 100) {
  ys = fit0 +rnorm(length(x), sd=b$s)
  bb = loess(ys~x , span = 0.33)
  fb = cbind(fb, predict(bb))
}

fit = apply(fb, 1, function(x){
  quantile(x, prob = c(0.0275, 0.5,0.975))
})
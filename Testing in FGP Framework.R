#testing in optimal FGP framework
grid = read.csv("1.3 - grid.csv")$x
d = length(grid)
int.lengths = diff(grid)

J = 3

#read in functions
ell = read.csv("ell.csv", header = FALSE)$V1
phi = read.csv("phi.csv", header = FALSE)$V1

#two minute runtime
system.time({p.total = read.csv("../p_mtx.csv")
q.total = read.csv("../q_mtx.csv")})

n = nrow(p.total)
Tm = ncol(p.total)

#train and test data
prop = 0.75
T.train = floor(prop*Tm)
#fit to specific date instead:
T.train = which(colnames(p.total) == "X2002.12.31")
p.train = p.total[, 1:T.train]
q.train = q.total[, 1:T.train]
p.test = p.total[, (T.train + 1):Tm]
q.test = q.total[, (T.train + 1):Tm]

#get class labels for stocks, adj to add to evaluation of derivatives
categories = list(1:33, 34:66, 67:100)
category = sapply(1:n, function(x) which(sapply(categories, function(y) x %in% y)))
adj = d*(category - 1)


#-------obtain daily changes in log wealth-------
wealth.changes = function(p, q, ell, phi){
  #indices for evaluation
  indices = apply(X = p, MARGIN = c(1,2), FUN = function(x) which.max(x < grid) - 1) 
  eval_indices = indices + adj
  
  #fixed uniform lambdas
  lambdas = rep(1/n, n)
  
  delta.pq = q - p
  
  #phi and differences of ell, phi at evaluation points
  dell = diff(ell)
  dell.mtx = matrix(dell[eval_indices], nrow = n, byrow = FALSE)
  
  dphi = diff(phi)
  phi.mtx = matrix(phi[eval_indices], nrow = n, byrow = FALSE)
  dphi.mtx = matrix(dphi[eval_indices], nrow = n, byrow = FALSE)
  eval.grid = matrix(grid[indices], nrow = n, byrow = FALSE)
  
  #for computing derivative of ell,
  eval.int.lengths = matrix(int.lengths[indices], nrow = n, byrow = FALSE)
  
  cw.wealth = 1 + colSums(lambdas * (delta.pq * dell.mtx / eval.int.lengths))
  my.wealth = 1 + colSums(lambdas * delta.pq * (phi.mtx + (dphi.mtx/eval.int.lengths) * (p - eval.grid)))
  
  out = list(cw.wealth, my.wealth)
  names(out) = c("cw.wealth", "my.wealth")
  return(out)
}

z.train = wealth.changes(p.train, q.train, ell,phi)
z.test = wealth.changes(p.test, q.test, ell,phi)

sum(log(z.train$cw.wealth))
sum(log(z.test$cw.wealth))
sum(log(z.train$my.wealth))
sum(log(z.test$my.wealth))

#plot results
dates = colnames(p.total)[(T.train + 1): ncol(p.total)]
dates = gsub("X", "", dates)
dates = as.Date(dates, "%Y.%m.%d")
#plot limits
ylow = min(cumsum(log(z.test$cw.wealth)), cumsum(log(z.test$my.wealth))) - 0.04
yhigh = max(cumsum(log(z.test$cw.wealth)), cumsum(log(z.test$my.wealth))) + 0.04


png(file="performance.png", width=1100, height=700, pointsize = 17.5)
plot(dates, cumsum(log(z.test$cw.wealth)), type = "l", xlab = "Date", ylab = "log Relative Wealth", col = "black", lwd = 2,
     ylim = c(ylow, yhigh))
lines(dates, cumsum(log(z.test$my.wealth)), type = "l", xlab = "Date", ylab = "log Relative Wealth", col = "red", lwd = 2)
legend("topleft", inset = 0.01, box.lty = 0, legend = c("Campbell and Wong's Method", "Our Method"), col = c("black", "red"), lwd  = 2)
dev.off()

#testing if phi satisfies additional constraints:
# alpha = 100
# dphi = diff(phi)
# constraint.mtx = matrix(nrow = J*(d-2), ncol = 2)
# for (j in 1:J){
#   for (i in 1:(d-2)){
#     constraint.mtx[(j-1)*(d-2) + i, 1] = dphi[(j-1)*d + i+1]/int.lengths[i+1] - dphi[(j-1)*d + i]/int.lengths[i]
#     constraint.mtx[(j-1)*(d-2) + i, 2] = alpha*(int.lengths[i+1]+int.lengths[i])/2
#   }
# }

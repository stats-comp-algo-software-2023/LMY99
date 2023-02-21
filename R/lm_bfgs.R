lm_log_likelihood <- function(coef,design,outcome,noise_var=1){
  prediction <- design %*% coef
  residual <- outcome - prediction
  return(sum(-residual^2/noise_var/2))
}
lm_loglike_grad <- function(coef,design,outcome,noise_var=1){
  prediction <- design %*% coef
  residual <- outcome - prediction
  return(drop(crossprod(design,residual)/noise_var))
}
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3), ...) {
  n <- length(x)
  numerical_grad <- rep(0, n)
  # Fill in
  for(i in 1:n){
    dif <- rep(0, n); dif[i] <- dx
    numerical_grad[i] <- (func(x+dif, ...) - func(x-dif, ...))/2/dx
  }
  return(numerical_grad)
}
lm_bfgs <- function(design,outcome,noise_var=1){
  num_predictor <- ncol(design)
  init_coef <- rep(0, num_predictor)
  optim_result <- optim(init_coef, lm_log_likelihood, lm_loglike_grad,
                        design=design, outcome=outcome, method='BFGS',
                        control=list(fnscale=-1)
                        )
  return(optim_result$par)
}

#' @export
hiper_glm <- function(design,outcome,model='linear',option=list()){
  support_model <- c('linear','logit')
  if(!(model %in% support_model))
    stop(sprintf("Model %s not supported.",model))
  # TODO: Find maximum likelihood estimate for GLM
  hglm_out <- list()
  class(hglm_out) <- 'hglm'
  warning("hiper_glm not yet implemented.")
  return(hglm_out)
}

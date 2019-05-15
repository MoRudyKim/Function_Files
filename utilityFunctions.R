rmSome <- function(type, env = globalenv(), negate = FALSE) {
  fun <- match.fun(type)
  if(negate) fun <- negate(fun)
  objget <- mget(ls(envir = env), envir = env)
  rmnames <- names(Filter(fun, objget))
  rm(list = rmnames, envir = env)
}


rmSome(is.ggplot)
rmSome(is.data.frame,negate = TRUE)






















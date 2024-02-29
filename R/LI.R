LI <- function(x, k) {
  UseMethod('LI')
}

LI.default <- function(x, k) {
  x$LI
}

LI.profilelike <- function(x, k = c(4.5, 6.8, 8, 16, 32)) {
  lik.norm <- x$profile.lik.norm
  theta <- x$theta
  li <- vapply(1 / k, function(ki) {
    range(theta[lik.norm >= ki], na.rm = TRUE)
  }, numeric(2), USE.NAMES = FALSE)
  rownames(li) <- c('lower', 'upper')
  colnames(li) <- sprintf('%8s', sprintf('1/%s LI', k))
  t(li)
}

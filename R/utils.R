check_formula <- function(formula, data, theta.off) {
    m <- stats::model.frame(formula, data)
    X <- stats::model.matrix(formula, m)
    y <- stats::model.response(m)
    if(!is.numeric(theta.off)){
        stop("Warning message: 'profile.theta' must be a numeric variable")
    }
    len_t <- length(theta.off)
    len_y <- length(y)
    len_x <- nrow(X)
    if( len_t != len_y | len_t != len_x | len_y != len_x ){
        cat("Warning message: remove missing data \n")
    }
    list(x = X, y = y)
}

hilo_theta <- function(fit, digits) {
    fit_summ <- coef(summary(fit))
    mle <- fit_summ["theta.off",1]
    se <- fit_summ["theta.off",2]
    round(mle + c(-4,4) * se, digits)
}

profilelike_fun <- function(theta, log.lik) {
    ll_nm <- !is.na(log.lik)
    theta <- theta[ll_nm]
    log.lik <- log.lik[ll_nm]
    profile.lik <- exp(log.lik)

    mm <- max(log.lik, na.rm=TRUE)
    log.norm.lik <- log.lik - mm
    profile.lik.norm <- exp(log.norm.lik)

    l <- list(theta=theta, profile.lik=profile.lik, profile.lik.norm=profile.lik.norm)
    class(l) <- c('profilelike', 'list')
    l
}

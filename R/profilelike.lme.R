profilelike.lme <-
function(formula, data, subject, random, correlation=NULL, profile.theta, method="ML", lo.theta=NULL, hi.theta=NULL, length=300, round=2, subset=NULL, weights=NULL, ...){
    if(!is.null(subset)){
        stop("Warning message: 'subset' should not be provided")
    }
    theta.off <- data[,profile.theta]
    Xy <- check_formula(formula, data, theta.off)
    X <- Xy$x # model.matrix(formula, mf)
    y <- Xy$y # model.response(mf)

    id <- data[,subject]

    if( is.null(lo.theta) | is.null(hi.theta) ){
        cat("Warning message: provide lo.theta and hi.theta \n")
        fit <- stats::lm(y ~ -1 + X + theta.off, na.action=stats::na.fail)
        hl <- hilo_theta(fit, round)
        lo.theta <- hl[1]
        hi.theta <- hl[2]
    }

    theta <- seq(from =lo.theta, to=hi.theta, length=length)
    log.lik <- rep(NA, length)

    for(i in seq(length)){
        pi <- theta[i]
        y.off <- y - pi*theta.off
        fit <- nlme::lme(y.off ~ -1 + X, random = random, correlation = correlation, weights = weights,
            method = "ML", na.action = stats::na.fail
        )
        log.lik[i] <- stats::logLik(fit)
    }

    profilelike_fun(theta, log.lik)
}

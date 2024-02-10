profilelike.polr <-
function(formula, data, profile.theta, method="logistic", lo.theta=NULL, hi.theta=NULL, length=300, round=2, subset=NULL, weights=NULL, offset=NULL, ...){
    if(!is.null(subset)){
        stop("Warning message: 'subset' should not be provided")
    }
    if(!is.null(weights)){
        stop("Warning message: 'weights' should not be provided")
    }
    if(!is.null(offset)){
        stop("Warning message: 'offset' should not be provided")
    }
    theta.off <- data[,profile.theta]
    Xy <- check_formula(formula, data, theta.off)
    X <- Xy$x # model.matrix(formula, mf)
    y <- Xy$y # model.response(mf)

    if( is.null(lo.theta) | is.null(hi.theta) ){
        cat("Warning message: provide lo.theta and hi.theta \n")
    }

    theta <- seq(from =lo.theta, to=hi.theta, length=length)
    log.lik <- rep(NA, length)

    oneColumn <- ncol(X) == 1
    for(i in seq(length)){
        pi <- theta[i]
        if(oneColumn) {
            fit <- MASS::polr(y ~ X + offset(pi*theta.off), method=method, na.action=stats::na.fail)
        } else {
            fit <- MASS::polr(y ~ X[,-1] + offset(pi*theta.off), method=method, na.action=stats::na.fail)
        }
        log.lik[i] <- stats::logLik(fit)
    }

    profilelike_fun(theta, log.lik)
}

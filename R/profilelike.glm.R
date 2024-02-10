profilelike.glm <-
function(formula, data, profile.theta, family=stats::gaussian, offset.glm=NULL, lo.theta=NULL, hi.theta=NULL, length=300, round=2, subset=NULL, weights=NULL, offset=NULL, ...){
    if(!is.null(subset)){
        stop("Warning message: 'subset' should not be provided")
    }
    if(!is.null(weights)){
        stop("Warning message: 'weights' should not be provided")
    }
    if(!is.null(offset)){
        stop("Warning message: do not use 'offset'; use 'offset.glm' instead of 'offset' ")
    }
    theta.off <- data[,profile.theta]
    Xy <- check_formula(formula, data, theta.off)
    X <- Xy$x # model.matrix(formula, mf)
    y <- Xy$y # model.response(mf)

    if( is.null(lo.theta) | is.null(hi.theta) ){
        cat("Warning message: provide lo.theta and hi.theta \n")
        fit <- stats::glm(y ~ -1 + X + theta.off, family=family, na.action=stats::na.fail)
        hl <- hilo_theta(fit, round)
        lo.theta <- hl[1]
        hi.theta <- hl[2]
    }

    theta <- seq(from =lo.theta, to=hi.theta, length=length)
    log.lik <- rep(NA, length)

    useOffset <- !is.null(offset.glm)
    if(useOffset) {
        glm.off <- data[,offset.glm]
    }
    for(i in seq(length)){
        pi <- theta[i]
        if(useOffset){
            fit <- stats::glm(y ~ -1 + X + offset(pi*theta.off) + offset(glm.off), family=family, na.action=stats::na.fail)
        } else {
            fit <- stats::glm(y ~ -1 + X + offset(pi*theta.off), family=family, na.action=stats::na.fail)
        }
        log.lik[i] <- stats::logLik(fit)
    }

    profilelike_fun(theta, log.lik)
}

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
m <- stats::model.frame(formula, data)
X <- stats::model.matrix(formula, m)
y <- stats::model.response(m)
theta.off <- data[,names(data)==profile.theta]

	if(!is.numeric(theta.off)){
		stop("Warning message: 'profile.theta' must be a numeric variable")
	}
	if( ( length(theta.off)!= length(y) | length(theta.off)!= length(X[,1]) | length(y)!= length(X[,1]) ) ){
		cat("Warning message: remove missing data \n")
		}
		if( ( is.null(lo.theta) | is.null(hi.theta) )){
			cat("Warning message: provide lo.theta and hi.theta \n")
			}

theta <- seq(from =lo.theta, to=hi.theta, length=length)
log.lik <- rep(NA, length)

for(i in 1:length){
	pi <- theta[i]
		if(length(X[1,])==1) { fit <- MASS::polr(y ~ X + offset(pi*theta.off), method=method, na.action=stats::na.fail) }
			else { fit <- MASS::polr(y ~ X[,-1] + offset(pi*theta.off), method=method, na.action=stats::na.fail) }
	log.lik[i] <- stats::logLik(fit)
	}

theta <- theta[is.na(log.lik)!=1]
log.lik <- log.lik[is.na(log.lik)!=1]
profile.lik <- exp(log.lik)

mm <- max(log.lik, na.rm=TRUE)
log.norm.lik <- log.lik - mm
profile.lik.norm <- exp(log.norm.lik)

return(list(theta=theta, profile.lik=profile.lik, profile.lik.norm=profile.lik.norm))
}


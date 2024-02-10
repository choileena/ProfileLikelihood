profilelike.summary <- function(k, theta, profile.lik.norm, round=2){
    mle <- round(max(theta[profile.lik.norm==max(profile.lik.norm)]),round)
    theta1.x.p <- theta[profile.lik.norm >=(1/k)]
    li.k.p <- rep(k, length(theta1.x.p))
    theta.x.p.norm <- theta[profile.lik.norm >=0.146] #6.849
    li.x.p.norm <- rep(0.146, length(theta.x.p.norm))

    LI.norm <- round(range(theta.x.p.norm), round)
    LI.k <- round(range(theta1.x.p), round)

    list(k = k, mle = mle, LI.k = LI.k,  LI.norm = LI.norm)
}

summary.profilelike <- function(obj, k, round = 2) {
    profilelike.summary(k, obj$theta, obj$profile.lik.norm, round)
}

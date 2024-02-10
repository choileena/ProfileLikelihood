profilelike.plot <- function(theta, profile.lik.norm, round=2){
    mle <- round(max(theta[profile.lik.norm==max(profile.lik.norm)]),round)
    theta1.x.p <- theta[profile.lik.norm >=(1/8)]
    theta2.x.p <- theta[profile.lik.norm >=(1/20)]
    theta3.x.p <- theta[profile.lik.norm >=(1/32)]
    theta.x.p.norm <- theta[profile.lik.norm >=0.146] #6.849
    li.x.p.norm <- rep(0.146, length(theta.x.p.norm))
    li8.x.p <- rep(1/8, length(theta1.x.p))
    li20.x.p <- rep(1/20, length(theta2.x.p))
    li32.x.p <- rep(1/32, length(theta3.x.p))

    plot(theta, profile.lik.norm, type="l", lty=1, lwd=1, ylim=c(0,1), xlim=c(min(theta), max(theta)), ylab="", xlab=expression(theta))
    graphics::lines(theta.x.p.norm, li.x.p.norm, lty=1, lwd=1, col="violet")
    graphics::lines(theta1.x.p, li8.x.p, lty=1, lwd=1, col=4)
    graphics::lines(theta2.x.p, li20.x.p, lty=1, lwd=1, col=2)
    graphics::lines(theta3.x.p, li32.x.p, lty=1, lwd=1, col=3)
    graphics::abline(v=0, lty=2, lwd=1.2, col="gray")
    if(mle > 0){
        pos=0.15
    } else{
        pos=0.8
    }
    spot <- theta[pos*length(theta)]
    range1 <- round(range(theta1.x.p), round)
    range2 <- round(range(theta2.x.p), round)
    range3 <- round(range(theta3.x.p), round)
    range4 <- round(range(theta.x.p.norm), round)

    graphics::text(spot, 1, paste("Max at  ", mle), cex=0.9, col=1)
    graphics::text(spot, 0.95, paste("1/6.8 LI (", range4[1], ",", range4[2], ")" ), cex=0.9, col="violet")
    graphics::text(spot, 0.90, paste("1/8 LI (", range1[1], ",", range1[2], ")" ), cex=0.9, col=4)
    graphics::text(spot, 0.85, paste("1/20 LI (", range2[1], ",", range2[2], ")" ), cex=0.9, col=2)
    graphics::text(spot, 0.80, paste("1/32 LI (", range3[1], ",", range3[2], ")" ), cex=0.9, col=3)
}

plot.profilelike <- function(obj, round = 2) {
    profilelike.plot(obj$theta, obj$profile.lik.norm, round)
}

profilelike.plot <- function(theta, profile.lik.norm, ...) {
    # make "profilelike" object
    l <- list(theta = theta, profile.lik.norm = profile.lik.norm)
    class(l) <- c('profilelike', 'list')
    plot.profilelike(l, ...)
}

plot.profilelike <- function(x, ...) {
    xargs <- list(...)
    k <- xargs$k
    rnd <- xargs$round
    showText <- xargs$showText
    pos <- xargs$textx
    texty <- xargs$texty
    if(is.null(k)) {
        k <- c(8, 32)
    } else if(isFALSE(k)) {
        k <- numeric(0)
        showText <- FALSE
    }
    if(is.null(rnd)) rnd <- 2
    if(is.null(showText)) showText <- TRUE
    if(is.null(texty)) texty <- 0.9
    # set and preserve `par`
    op <- par(las = 1)
    on.exit(par(op))

    theta <- x$theta
    profile.lik.norm <- x$profile.lik.norm
    mle <- round(max(theta[(max(profile.lik.norm) - profile.lik.norm) < 1e-10]), rnd)
    if(is.null(pos)) {
        if(mle > 0){
            pos <- 0.15
        } else{
            pos <- 0.8
        }
    }
    spot <- theta[pos*length(theta)]

    plotArgs <- list(x=theta, y=profile.lik.norm, type="l", lty=1, lwd=1, ylim=c(0,1), xlim=range(theta), ylab="", xlab=expression(theta))
    moreArgs <- xargs[setdiff(names(xargs), c('k','round','showText','textx','texty'))]
    plotArgs <- modifyList(plotArgs, moreArgs)
    do.call(plot, plotArgs)
    if(showText) graphics::text(spot, texty, paste("Max at  ", mle), cex = 0.9, col = 1)

    col_vec <- c('violet', 4, 2, 3)
    y_spot <- seq(texty - 0.06, by = -0.06, length.out = length(k))
    for(i in seq_along(k)) {
        k_i <- 1 / k[i]
        t.xp <- theta[profile.lik.norm >= k_i]
        li.xp <- rep(k_i, length(t.xp))
        rng <- round(range(t.xp, na.rm = TRUE), rnd)
        graphics::lines(t.xp, li.xp, lty = plotArgs$lty, lwd = plotArgs$lwd, col = col_vec[i])
        lab_i <- sprintf("1/%s LI (%s, %s)", k[i], rng[1], rng[2])
        if(showText) graphics::text(spot, y_spot[i], lab_i, cex = 0.9, col = col_vec[i])
    }
}

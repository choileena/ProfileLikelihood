LR.pvalue <- function(y1, y2, n1, n2, interval=0.01){
    s <- y1 + y2
    n <-  n1 + n2
    s.failure <- n - s
    x1 <- n1-y1
    x2 <- n2-y2
    Y <- c(rep(1, s), rep(0, s.failure))
    treat <- c(rep(1, y1), rep(0, y2), rep(1, x1), rep(0, x2))
    fit.glm <- stats::glm( Y ~ treat, family = stats::binomial)
    fit_coef <- stats::coef(summary(fit.glm))
    mle <- fit_coef[2,1]
    se <- fit_coef[2,2]

    if(y1==0 | y2==0){
        cat("Warning message: Likelihood intervals, LRs and the corresonding p-values are not reliable with empty cells in 2-by-2 tables. \n")
        lo.psi <- mle - 2*abs(mle) - 2
        hi.psi <- mle + 2*abs(mle) + 2
    } else{
        lo.psi <- mle - 2*se - 2
        hi.psi <- mle + 2*se + 2
    }

    lo.psi1 <- round(lo.psi, 1)
    hi.psi1 <- round(hi.psi, 1)
    if(lo.psi1 < 0 & hi.psi1 > 0){
        psi <- c(seq(from=lo.psi1, to=-interval, by=interval), 0, seq(from=interval, to=hi.psi1, by=interval))
    } else if(lo.psi1 >= 0 & hi.psi1 >= 0){
        psi <- c(0, seq(from=interval, to=hi.psi1, by=interval))
    } else{
        psi <- c(seq(from=lo.psi1, to=-interval, by=interval), 0)
    }

    L <- function(lambda){
        y1*psi[i] + (y1 + y2)*lambda - n1*log(1 + exp(psi[i] + lambda)) - n2*log(1 + exp(lambda))
    }

    Epi2 <- (y2+0.5)/n2
    Odds2 <- Epi2/(1-Epi2)
    int <- log(Odds2)

    if(int == -Inf){
        low.int <- - 10000
        up.int <- 1000
    } else if(int == Inf){
        low.int <- - 1000
        up.int <- 10000
    } else{
        low.int <- - 1000
        up.int <- 1000
    }

    l_psi <- length(psi)
    log.lik <- rep(NA, l_psi)
    # "par" is unused
#     par <- matrix(NA, l_psi, 1)

    for(i in seq_along(psi)){
        ff <- stats::optimize(L, interval=c(low.int, up.int), maximum=TRUE)
#         par[i,] <- ff$maximum
        log.lik[i] <- ff$objective
    }
    lik <-  exp(log.lik)
    mm <- max(lik, na.rm=TRUE)
    profile.lik.norm <- lik/mm

    norm <- 0.146
    if(y1==0 | y2==0){
        profile.LI.norm <- NULL
    } else{
        psi1.x.p.norm <- psi[profile.lik.norm >=norm]
        psi.p.ci.norm <- range(psi1.x.p.norm)
        profile.LI.norm <- psi.p.ci.norm
    }

    which(abs(psi) < 1e-8)
    which(psi == 0)
    H0.profile <- unique(profile.lik.norm[abs(psi) < 1e-8])
    LR.profile <- H0.profile/1
    Pvalue.profile.LR <- 1- stats::pchisq(-2*log(LR.profile), df=1)

    cond.func <- function(y1, y2, n1, n2, OR){
        den.func <- function(y1, y2, n1, n2){
            s <- y1 + y2
            L <- max(0, s - n2)
            H <- min(n1, s)
            dd <- 0
            for(u in L:H){
                dd <- dd + choose(n1, u)*choose(n2, (s-u))*OR^u
            }
            dd
        }
        dd <- den.func(y1=y1, y2=y2, n1=n1, n2=n2)
        choose(n1, y1)*choose(n2, y2)*OR^y1 / dd
    }

    cond.lik <- rep(NA, l_psi)

    for(i in seq_along(cond.lik)){
        OR <- exp(psi[i])
        cond.lik[i] <- cond.func(y1=y1, y2=y2, n1=n1, n2=n2, OR=OR)
    }

    H0 <- cond.func(y1=y1, y2=y2, n1=n1, n2=n2, OR=1)
    mm.cond <- max(cond.lik, na.rm=TRUE)
    LR.cond <- H0/mm.cond
    Pvalue.cond.LR <- 1- stats::pchisq(-2*log(LR.cond), df=1)

    if(y1==0 | y2==0){
        mle.cond.lor <- NULL
        mle <- NULL
        cond.LI.norm <- NULL
    } else{
        cond.lik.norm <- cond.lik/mm.cond
        mle.cond.lor <- max(psi[cond.lik.norm==max(cond.lik.norm, na.rm=TRUE)], na.rm=TRUE)
        psi1.x.cond.norm <- psi[cond.lik.norm >= norm]
        cond.LI.norm <- range(psi1.x.cond.norm)
    }

    tt <- matrix(c(y1, y2, (n1-y1), (n2-y2)), nrow = 2, dimnames = list(c("treat", "control"), c("Success", "No success")))
    Pvalue.fisher.test <- stats::fisher.test(tt)$p.value
    Pvalue.chisq.cont.correction <- stats::chisq.test(tt)$p.value
    Pvalue.chisq.test <- stats::chisq.test(tt, correct=FALSE)$p.value

    list(
        mle.lor.uncond=mle,
        mle.lor.cond=mle.cond.lor,#FAIL
        LI.norm.profile=profile.LI.norm,
        LI.norm.cond=cond.LI.norm,#FAIL
        LR.profile=1/LR.profile,
        LR.cond=1/LR.cond,#FAIL
        Pvalue.LR.profile=Pvalue.profile.LR,
        Pvalue.LR.cond=Pvalue.cond.LR,#FAIL
        Pvalue.chisq.test=Pvalue.chisq.test,
        Pvalue.fisher.test=Pvalue.fisher.test,
        Pvalue.chisq.cont.correction=Pvalue.chisq.cont.correction
    )
}

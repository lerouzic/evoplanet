
logistic.regression <- function(time, OD) {
    time <- time - min(time)
     # Starting values
     logit <- function(x) log(x/(1-x))
     start <- list(
        P0 = min(OD),
        K  = max(OD),
        r  = coef(lm(logit(OD/(1.1*max(OD))) ~ time))[2]
    )
    reg <- nls(OD ~ K/(1+(K-P0)*exp(-r*time)/P0), start=start)
    ans <- list(
        model  =reg, 
        data   =data.frame(time=time, OD=OD),
        coef   =coef(reg),
        std.err=sqrt(diag(vcov(reg))),
        vcov   =vcov(reg))
    class(ans) <- "mylogis"
    ans
}

predict.mylogis <- function(object, newdata=list(time=seq(0, max(object$data$time), length.out=21)), ...) {
    data.frame(time=newdata$time, OD=predict(object$model, newdata=newdata, ...))
}

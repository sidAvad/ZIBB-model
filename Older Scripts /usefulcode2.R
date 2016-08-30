tt1 <- tryCatch(lgamma(phi*(1-mu[i])),error=function(e) e, warning=function(w) w)
tt2 <- tryCatch(beta(y1[i]+phi*mu[i],n -y1[i] + phi*(1-mu[i])),error=function(e) e, warning=function(w) w)
if(is(tt1, "warning")){muvec1[i] <<-  mu[i]}
if(is(tt2, "warning")){
        muvec2[i] <<-  mu[i]
        yvec[i] <<- y1[i]
}

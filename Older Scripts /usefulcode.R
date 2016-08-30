res <- nloptr(init,ZeroLL2,eval_g_ineq=inequality,y=r, 
               opts=list("algorithm"="NLOPT_LN_COBYLA"))
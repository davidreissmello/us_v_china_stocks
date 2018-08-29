daily_log_return <- function(data, npar = TRUE, print = TRUE){
  n = length(data)
  lgreturn = rep(0, n - 1)
  for(i in (1:(n-1))){
    lgreturn[i] = log(data[i+1]) - log(data[i])
  }
  lgreturn = data.frame(lgreturn)
  return(lgreturn)
}

ci_mean <- function(data, alpha, npar = TRUE, print = TRUE){
  u.sample <- mean(data, na.rm = TRUE)
  sd<- sd(data, na.rm = TRUE)
  n <- length(data)
  t <- qt(p=(alpha/2), lower.tail = FALSE, df = n - 1)
  
  u.lowerbound <- u.sample - t * (sd/sqrt(n))
  u.upperbound <- u.sample + t * (sd/sqrt(n))
  CI_u <- c(u.lowerbound,u.upperbound)
  
  return(CI_u)
}

ci_sd <- function(data, alpha, npar = TRUE, print = TRUE){
  sd<- sd(data, na.rm = TRUE)
  n <- length(data)
  t <- qt(p=(alpha/2), lower.tail = FALSE, df = n - 1)
  
  lower_chi <- qchisq(alpha/2, df = n - 1)
  upper_chi <- qchisq(1 - alpha/2, df = n -1)
  lower_sd <- sqrt((n - 1)*sd^2/upper_chi)
  upper_sd <- sqrt((n - 1)*sd^2/lower_chi)
  CI_sd <- c(lower_sd, upper_sd)
  
  return(CI_sd)
}

GoodnessOfFit<-function(data, alpha){
  qqnorm(data)
  pvalue <- ks.test(unique(data), pnorm, mean(data, na.rm = TRUE), sd(data, na.rm = TRUE))$p.value
  
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

Independence<-function(data1, data2, alpha){
  sequence <- seq(-1, 1, .01)
  
  hist1 <- hist(data1, breaks = sequence)
  hist1$breaks
  hist1$counts
  
  hist2 <- hist(data2, breaks = sequence)
  hist2$breaks
  hist2$counts
  
  matrix1<-matrix(data = data1, nrow= 250, ncol=1, byrow = FALSE, dimnames = NULL)
  matrix2<-matrix(data = data2, nrow= 250, ncol=1, byrow = FALSE, dimnames = NULL)
  
  #Test of Independece
  pvalue <- chisq.test(matrix1, matrix2)$p.value
  
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

two_sample_t_test <- function(data1, data2, alpha){
  pvalue <- t.test(data1, data2)$p.value
  
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

two_sample_t_test_less <- function(data1, data2, alpha){
  pvalue <- t.test(data1, data2, alternative = c("less"))$p.value
  
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

two_sample_t_test_greater <- function(data1, data2, alpha){
  pvalue <- t.test(data1, data2, alternative = c("greater"))$p.value
  
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

f_test_twosided <- function(data1, data2, alpha){
  pvalue <- var.test(data1, data2, alternative = c("two.sided"))$p.value
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

f_test_less <- function(data1, data2, alpha){
  pvalue <- var.test(data1, data2, alternative = c("less"))$p.value
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}

f_test_greater <- function(data1, data2, alpha){
  pvalue <- var.test(data1, data2, alternative = c("greater"))$p.value
  if(pvalue < alpha){
    return(FALSE)
  }
  return(TRUE)
}


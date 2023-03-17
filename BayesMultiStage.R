# library(MGLM)
library(extraDistr)
library(tidyr)
library(Matrix)

#make the code modular
#calculate pred prob cutoffs
P_Yi = function(i, x, n, p0=NULL, a0, b0, N_max) {
	m = N_max - n
	# choose(m, i) * beta(i + a0 + x, m - i + b0 + n - x) / beta(a0 + x, b0 + n - x)
	exp(lgamma(m+1) - lgamma(i+1) - lgamma(m-i+1) + lbeta(i + a0 + x, m - i + b0 + n - x) - lbeta(a0 + x, b0 + n - x))
	# exp(lgamma(m+1) - lgamma(i+1) - lgamma(m-i+1) + lgamma(i+a0+x)+lgamma(N_max-i+b0-x)-lgamma(a0+b0+N_max)-lgamma(a0+x)-lgamma(b0+n-x)+lgamma(a0+b0+n))
	# dbbinom(i, m , alpha = a0 + x, beta = b0 + n - x)
}

Bi = function(i, x, n=NULL, p0, a0, b0, N_max) {
	pbeta(p0, a0 + x + i, b0 + N_max - x - i, lower.tail = F)
}

PP = function(theta_T, ...) {
	arguments = list(...)
	# print(arguments)
	m = arguments$N_max - arguments$n
	# print(m)
	sum(P_Yi(i=0:m, ...) * (Bi(i=0:m, ...) >= theta_T))
	# sum(P_Yi(i=0:m, ...) * (Bi(i=0:m, ...) > theta_T))
}

PP2 = function(theta_T, ...) {
	arguments = list(...)
	# print(arguments)
	n = arguments$n
	x = arguments$x
	a0 = arguments$a0
	b0 = arguments$b0
	m = arguments$N_max - n
	# print(m)
	# sum(P_Yi(i=0:m, ...) * (Bi(i=0:m, ...) >= theta_T))
	# sum(P_Yi(i=0:m, ...) * (Bi(i=0:m, ...) > theta_T))
	sumBi = sum(Bi(i=0:m, ...) >= theta_T)
	pbb = pbbinom(m - sumBi, size = m, alpha = a0 + x, beta = b0 + n - x, lower.tail = F)
}

# PP_v = Vectorize(FUN = PP, vectorize.args = "x")

calc_cutoffs = function(theta_L, theta_U = 1, theta_T, p0, p1, a0 = p0, b0 = 1-p0, n) {
	N_max = max(n)
	# min_x = 0
	# max_x = N_max
	x_pass = numeric(0)
	x_fail = numeric(0)

	for(nl in n){
		# for(x in min_x:min(nl, max_x)){
		for(x in 0:nl){
			# PP_val = PP(theta_T = theta_T, x=x, n = nl, p0=p0, a0=a0, b0=b0, N_max=N_max)
			PP_val = PP2(theta_T = theta_T, x=x, n = nl, p0=p0, a0=a0, b0=b0, N_max=N_max)
			if (PP_val <= theta_L) {
			# if (PP_val < theta_L) {
				# min_x = x+1
				x_fail[nl] = x
			}
			else if (PP_val >= theta_U && (theta_U != 1 || nl == N_max)) {
				# max_x = x-1
				x_pass[nl] = x
				break
			}
		}
	}
	return(list(x_fail = x_fail[n], x_pass = x_pass[n]))
}

calc_cutoff_l_grid = function(l, n, theta_T, theta_L, theta_T_exp, theta_L_exp, x_exp, l_exp){
	x_pass = x_exp[which(l_exp == l & theta_T_exp >= theta_T & theta_L_exp > theta_L)]
	nl = n[l]
	return(as.integer(c(x_pass-1,nl)[1]))
}

calc_cutoff_grid = function(n, theta_T, theta_L, theta_T_exp, theta_L_exp, x_exp, l_exp){
	#check if things are the same length
	# length(theta_T) == length(theta_L)
	# length(theta_T_exp) == length(theta_L_exp) == length(x_exp) == length(l_exp)
	k = length(n)
	x_fail = vapply(X = 1:k, FUN = calc_cutoff_l_grid, FUN.VALUE = integer(1), 
		n = n, theta_T = theta_T, theta_L = theta_L, 
		theta_T_exp = theta_T_exp, theta_L_exp = theta_L_exp, x_exp = x_exp, l_exp = l_exp)
	x_pass = c(rep(NA, k-1), x_fail[k]+1)
	x_fail[x_fail == -1] = NA
	return(list(x_fail = x_fail, x_pass = x_pass))
}

calc_cutoff_grid_NA = function(n, theta_T, theta_L, theta_T_exp, theta_L_exp, x_exp, l_exp){
	#check if things are the same length
	# length(theta_T) == length(theta_L)
	# length(theta_T_exp) == length(theta_L_exp) == length(x_exp) == length(l_exp)
	k = length(n)
	x_fail = rep(NA, k)
	x_pass = rep(NA, k)
	return(list(x_fail = x_fail, x_pass = x_pass))
}

calc_theta_grid = function(n, p0, a0 = p0, b0 = 1-p0){
	#n=c(2,4); p0=0.25; a0 = p0; b0 = 1-p0
	k = length(n)
	nk = n[k]
	m_x_list = lapply(X = 1:k, FUN = function(l, n, N_max){
		nl = n[l]
		m = N_max - nl
		x = 0:nl
		data.frame(m, x, nl, l)
	}, n = n, N_max = nk)
	
	m_x = do.call(rbind, m_x_list)

	theta_T = unlist(mapply(FUN = function(m,...){Bi(i=0:m, ...)}, m_x$m, m_x$x, MoreArgs = list(p0=p0, a0=a0, b0=b0, N_max = nk)))
	theta_L = unlist(mapply(FUN = function(m, x, nl, a0, b0){pbbinom(0:m-1, size = m, alpha = a0 + x, beta = b0 + nl - x, lower.tail = F)}, m_x$m, m_x$x, m_x$nl, MoreArgs = list(a0=a0, b0=b0)))
	
	x_exp = rep(m_x$x, times = m_x$m+1)
	l_exp = rep(m_x$l, times = m_x$m+1)
	
	theta_x_l = data.frame(theta_T = theta_T, theta_L = theta_L, x_exp, l_exp)

	return(theta_x_l)
}

# calc_cutoffs2 = function(theta_L, theta_U = 1, theta_T, p0, p1, a0 = p0, b0 = 1-p0, n) {
	# N_max = max(n)
	# x_pass = numeric(0)
	# x_fail = numeric(0)

	# for(nl in n){
		# PP_val = PP(theta_T = theta_T, x=0:nl, n = nl, p0=p0, a0=a0, b0=b0, N_max=N_max)
		# x_fail[nl] = which(rev(PP_val) <= theta_L)[1]-1
		# x_pass[nl] = which(PP_val >= theta_U && (theta_U != 1 || nl == N_max))[1]-1
	# }
	# return(list(x_fail = x_fail[n], x_pass = x_pass[n]))
# }

##same for response, toxicity, independent, dependent

#utilities
incr = function(x){
	if(is.unsorted(x)) {return (integer(0))}
	return (x)
}

#distribution of response counts at each stage
##response only
# pxn = function(x, n, p, cx){
pxn = function(x, n, p, cx, xmat_nm = NULL, env = NULL){
	k = length(n)
	# stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == length(n)))
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	x_n=NA
	if(!is.null(xmat_nm) && !is.null(env)){
		x_n=get(xmat_nm, envir = env)
	}
	else {
		x_n=matrix(NA, nrow=nk+1, ncol = k)
	}
	# pxn_dyn(x, n, p, cx, x_n)
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	# pxn_dyn(x, n, d, p, cx, x_n, environment())
	prob = pxn_dyn(x, n, d, p, x_fail, x_pass, x_n, environment())
	if(!is.null(xmat_nm) && !is.null(env)){
		assign(xmat_nm, x_n, envir = env)
	}
	return(prob)
	# x_n
}
# pxn_dyn = function(x, n, p, cx, x_n){
# pxn_dyn = function(x, n, d, p, cx, x_n, env){
pxn_dyn = function(x, n, d, p, x_fail, x_pass, x_n, env){
	# invisible(readline(prompt=paste0("n = ", n, " x = ", x)))
	l = length(n)
	# d = diff(c(0, n))
	# d = n - c(0,n[-l])
	# d = c(n[1], n[-1] - n[-l])
	nl = n[l]
	dl = d[l]
	# x_fail = cx$x_fail
	# x_pass = cx$x_pass
	prob = 0
	# stopifnot(length(x_fail) == l && length(x_pass) == l)
	# cat(x_n, "\n")
	if (x > nl || x < 0) {
# cat("TEST5\n")
		return(0)
	}
	else if (!is.na(x_n[x+1,l])) {
# cat("TEST1\n")
		prob = x_n[x+1,l]
		return(prob)
	}
	else if (l==1 || (is.na(x_fail[l-1]) && is.na(x_pass[l-1]))){
# cat("TEST2\n")
		prob = dbinom(x=x, size=nl, prob=p)
	}
	# else if (x == 0 && is.na(x_fail[l-1])) {
# cat("TEST3\n")
		# prob = ifelse(is.na(x_fail[l-1]), (1-p)^nl, 0)
		# prob = (1-p)^nl
	# }
	# else if (x == nl && is.na(x_pass[l-1])) {
# cat("TEST4\n")
		# prob = ifelse(is.na(x_pass[l-1]), p^nl, 0)
		# prob = p^nl
	# }
	else if (!is.na(x_fail[l-1]) && x <= x_fail[l-1]) {
# cat("TEST6\n")
		# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
		# prob = pxn_dyn(x, n[-l], d[-l], p, cx = cx, x_n, environment())
		prob = pxn_dyn(x, n[-l], d[-l], p, x_fail = x_fail[-l], x_pass = x_pass[-l], x_n, environment())
	}
	else if (!is.na(x_pass[l-1]) && x-dl >= x_pass[l-1]) {
# cat("TEST7\n")
		# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
		# prob = pxn_dyn(x-dl, n[-l], d[-l], p, cx = cx, x_n, environment())
		prob = pxn_dyn(x-dl, n[-l], d[-l], p, x_fail = x_fail[-l], x_pass = x_pass[-l], x_n, environment())
	}
	else {
# cat("TEST8\n")
		min_i = 0
		max_i = x

		#double check some of these
		if(!is.na(x_fail[l-1])) {
			max_i = max(0, x-1-x_fail[l-1])
		}
		if(!is.na(x_pass[l-1])) {
			min_i = min(x, x+1-x_pass[l-1])
		}
		
		if(min_i <= max_i){
# cat(l, x, min_i, max_i, "\n")
			# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
			x_fail = x_fail[-l]
			x_pass = x_pass[-l]
			for (i in min_i:max_i){
				# prob = prob + dbinom(x=i, size=dl, prob=p) * pxn_dyn(x-i, n[-l], d[-l], p, cx = cx, x_n, environment())
				prob = prob + dbinom(x=i, size=dl, prob=p) * pxn_dyn(x-i, n[-l], d[-l], p, x_fail = x_fail, x_pass = x_pass, x_n, environment())
			}
		}
	}
	x_n[x+1,l] = prob
	assign("x_n", x_n, envir = env)
	return(prob)
}

##response + toxicity independent
###TODO: this is flawed because it can give probabilities for xR and xT from different stages###
pxn_RT = function(xR, xT, n, pR, pT, cxR, cxT){
	pxn(xR, n, pR, cxR) * pxn(xT, n, pT, cxT)
}

##response + toxicity dependent
my_dmultinom = function (x, size = NULL, prob, log = FALSE) 
{
    r <- lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
    if (log) 
        r
    else exp(r)
}

calc_p_OR = function(pT = 0.5, pR = 0.5, phi = 1) {
	p00=NA
	p01=NA
	p10=NA
	p11=NA
	if (phi == 1){
		p00 = (1-pT) * (1-pR)
		p01 = pT     * (1-pR)
		p10 = (1-pT) * pR
		p11 = pT     * pR
	}
	else {
		S  = sqrt((1 + (pT + pR) * (phi-1))^2 + 4 * phi * (1-phi) * pT * pR)
		p11 = (1 + (pT+pR) * (phi-1) - S)/2/(phi-1)
		p01 = pT - p11
		p10 = pR - p11
		p00 = 1 - p01 - p10 - p11
	}
	return(c(p00=p00, p01=p01, p10=p10, p11=p11))
}

pxn_RT_OR = function(xR, xT, n, pR, pT, cxR, cxT, phi = 1){
	k = length(n)
	# stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == length(n)))
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=array(NA,dim=c(max(n)+1, max(n)+1, length(n)))
	x_n=array(NA,dim=c(nk+1, nk+1, k))
	p = calc_p_OR(pT, pR, phi)
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	# pxn_RT_OR_dyn(xR, xT, n, d, p, cxR, cxT, x_n, environment())
	pxn_RT_OR_dyn(xR, xT, n, d, p, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n, environment())
	# pxn_RT_OR_dyn(xR, xT, n, p, cxR, cxT, x_n, environment())
	# x_n ###
}

# pxn_RT_OR_dyn = function(xR, xT, n, d, p, cxR, cxT, x_n, env){
pxn_RT_OR_dyn = function(xR, xT, n, d, p, xR_fail, xR_pass, xT_fail, xT_pass, x_n, env){
# pxn_RT_OR_dyn = function(xR, xT, n, p, cxR, cxT, x_n, env){
	# invisible(readline(prompt=paste0("n = ", n, " x = ", x)))
	# d = diff(c(0, n))
	l = length(n)
	nl = n[l]
	dl = d[l]
	# xR_fail = cxR$x_fail
	# xR_pass = cxR$x_pass
	# xT_fail = cxT$x_fail
	# xT_pass = cxT$x_pass
	prob = 0
	# stopifnot(length(xR_fail) == l && length(xR_pass) == l)
	# stopifnot(length(xT_fail) == l && length(xT_pass) == l)
# cat(x_n, "\n")
	if (xR > nl || xR < 0 || xT > nl || xT < 0) {
# cat("TEST5\n")
		return(0)
	}
	else if (!is.na(x_n[xR+1, xT+1, l])) {
# cat("TEST1\n")
		prob = x_n[xR+1, xT+1, l]
		return(prob)
	}
	else if(l==1 || (
			is.na(xR_fail[l-1]) && 
			is.na(xR_pass[l-1]) && 
			is.na(xT_fail[l-1]) && 
			is.na(xT_pass[l-1]))){
# cat("TEST2\n")
		for(g in max(0,xR+xT-nl):min(xR, xT)){
# cat("nl", nl, "xR", xR, "xT", xT, "g", g, "\n")
			prob = prob + my_dmultinom(x=c(nl-xR-xT+g, xT-g, xR-g, g), size = nl, prob=p)
		}
	}
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
# cat("TEST3\n")
		# prob = ifelse(is.na(xR_fail[l-1]) && is.na(xT_fail[l-1]), p["p00"]^nl, 0)
		# prob = p["p00"]^nl
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = p["p01"]^nl
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = p["p10"]^nl
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
# cat("TEST4\n")
		# prob = ifelse(is.na(xR_pass[l-1]) && is.na(xT_pass[l-1]), p["p11"]^nl, 0)
		# prob = p["p11"]^nl
	# }
	else if ((!is.na(xR_fail[l-1]) && xR <= xR_fail[l-1]) || 
			(!is.na(xT_fail[l-1]) && xT <= xT_fail[l-1])) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_OR_dyn(xR, xT, n[-l], d[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
		prob = pxn_RT_OR_dyn(xR, xT, n[-l], d[-l], p, xR_fail=xR_fail[-l], xR_pass=xR_pass[-l], xT_fail=xT_fail[-l], xT_pass=xT_pass[-l], x_n, environment())
		# prob = pxn_RT_OR_dyn(xR, xT, n[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
	}
	else if (!is.na(xR_pass[l-1]) && xR-dl >= xR_pass[l-1] && 
			!is.na(xT_pass[l-1]) && xT-dl >= xT_pass[l-1]) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_OR_dyn(xR-dl, xT-dl, n[-l], d[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
		prob = pxn_RT_OR_dyn(xR-dl, xT-dl, n[-l], d[-l], p, xR_fail=xR_fail[-l], xR_pass=xR_pass[-l], xT_fail=xT_fail[-l], xT_pass=xT_pass[-l], x_n, environment())
		# prob = pxn_RT_OR_dyn(xR-dl, xT-dl, d[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
	}
	else {
		# min_i = 0
		# max_i = xR
		# min_j = 0
		# max_j = xT
		min_i = max(0, xR-n[l-1])
		max_i = min(xR, dl)
		min_j = max(0, xT-n[l-1])
		max_j = min(xT, dl)

		#double check some of these
		if(!is.na(xR_fail[l-1])) {
			# max_i = max(0, xR-1-xR_fail[l-1])
			max_i = min(max_i, max(0, xR-1-xR_fail[l-1]))
		}
		if(!is.na(xR_pass[l-1])) {
			# min_i = min(xR, xR+1-xR_pass[l-1])
			min_i = max(min_i, xR+1-xR_pass[l-1])
		}
		if(!is.na(xT_fail[l-1])) {
			# max_j = max(0, xT-1-xT_fail[l-1])
			max_j = min(max_j, max(0, xT-1-xT_fail[l-1]))
		}
		if(!is.na(xT_pass[l-1])) {
			# min_j = min(xT, xT+1-xT_pass[l-1])
			min_j = max(min_j, xT+1-xT_pass[l-1])
		}

# cat("nl", nl, "min_i", min_i, "max_i", max_i, "min_j", min_j, "max_j", max_j, "\n")
		if(min_i <= max_i && min_j <= max_j){
			# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
			# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
			xR_fail_l=xR_fail[-l]
			xR_pass_l=xR_pass[-l]
			xT_fail_l=xT_fail[-l]
			xT_pass_l=xT_pass[-l]
			for (i in min_i:max_i){
				for(j in min_j:max_j){
					ppij = 0
					min_g = max(0,i+j-dl)
					max_g = min(i, j)
					if (min_g <= max_g){
						for(g in min_g:max_g){
# cat("dl", dl, "i", i, "j", j, "g", g, "\n")
							ppij = ppij + my_dmultinom(x=c(dl-i-j+g, j-g, i-g, g), size = dl, prob=p)
						}
						# prob = prob + ppij * pxn_RT_OR_dyn(xR-i, xT-j, n[-l], d[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
						prob = prob + ppij * pxn_RT_OR_dyn(xR-i, xT-j, n[-l], d[-l], p, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n, environment())
						# prob = prob + ppij * pxn_RT_OR_dyn(xR-i, xT-j, n[-l], p, cxR = cxR, cxT = cxT, x_n, environment())
					}
				}
			}
		}
	}
	x_n[xR+1, xT+1, l] = prob
	assign("x_n", x_n, envir = env)
	# assign('x_n[xR+1, xT+1, l]', prob, envir = env)
	return(prob)
}

##response + toxicity prior
multi_MD = function(x, a) {
# multi_MD = function(x00, x01, x10, x11, a00, a01, a10, a11) {
	# stopifnot(length(x) == length(a))
	n = sum(x)
	# n = x00+x01+x10+x11
	A = sum(a)
	# A = a00+a01+a10+a11
	return(
		# gamma(sum(a)) * gamma(n+1) / gamma(n+sum(a)) * prod(gamma(x+a) / gamma(a) / gamma(x+1))
		exp(lgamma(A) + lgamma(n+1) - lgamma(n+A) + sum(lgamma(x+a)-lgamma(a)-lgamma(x+1)))
		# exp(lgamma(A) + lgamma(n+1) - lgamma(n+A) 
			# + lgamma(x00+a00) + lgamma(x01+a01) + lgamma(x10+a10) + lgamma(x11+a11) 
			# - lgamma(a00) - lgamma(a01) - lgamma(a10) - lgamma(a11) 
			# - lgamma(x00+1) - lgamma(x01+1) - lgamma(x10+1) - lgamma(x11+1))
	)
}

# pxn_RT_dm = function(xR, xT, n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	# x_n=array(NA,dim=c(max(n)+1, max(n)+1, length(n)))
	# p = calc_p_OR(pT, pR, phi)
	# a = lambda * p
	# names(a) = c("a00", "a01", "a10", "a11")
	# pxn_RT_dm_dyn(xR, xT, n, a, cxR, cxT, x_n, environment())
	# # x_n ###
# }

# #something is wrong
# #in order for all stages to be dirichlet multinomial we must add the previous counts to the prior for the next stage
# pxn_RT_dm_dyn = function(xR, xT, n, a, cxR, cxT, x_n, env){
	# # invisible(readline(prompt=paste0("n = ", n, " x = ", x)))
	# call_ct <<- call_ct + 1
	# d = diff(c(0, n))
	# l = length(n)
	# nl = n[l]
	# dl = d[l]
	# xR_fail = cxR$x_fail
	# xR_pass = cxR$x_pass
	# xT_fail = cxT$x_fail
	# xT_pass = cxT$x_pass
	# prob = 0
	# stopifnot(length(xR_fail) == l && length(xR_pass) == l)
	# stopifnot(length(xT_fail) == l && length(xT_pass) == l)
# # cat(x_n, "\n")
	# if (!is.na(x_n[xR+1, xT+1, l])) {
# # cat("TEST1\n")
		# dyn_ct <<- dyn_ct + 1
		# prob = x_n[xR+1, xT+1, l]
		# return(prob)
	# }
	# else if (xR > nl || xR < 0 || xT > nl || xT < 0) {
# # cat("TEST5\n")
		# prob = 0
	# }
	# else if(l==1 || (
			# is.na(xR_fail[l-1]) && 
			# is.na(xR_pass[l-1]) && 
			# is.na(xT_fail[l-1]) && 
			# is.na(xT_pass[l-1]))){
# # cat("TEST2\n")
		# for(g in max(0,xR+xT-nl):min(xR, xT)){
# # cat("nl", nl, "xR", xR, "xT", xT, "g", g, "\n")
			# prob = prob + multi_MD(x=c(nl-xR-xT+g, xT-g, xR-g, g), a=a)
			# # prob = prob + exp(ddirmn(Y=c(nl-xR-xT+g, xT-g, xR-g, g), alpha=a))
		# }
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
# # cat("TEST3\n")
		# prob = multi_MD(x=c(nl, 0, 0, 0), a=a)
		# # prob = exp(ddirmn(Y=c(nl, 0, 0, 0), alpha=a))
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = multi_MD(x=c(0, nl, 0, 0), a=a)
		# # prob = exp(ddirmn(Y=c(0, nl, 0, 0), alpha=a))
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = multi_MD(x=c(0, 0, nl, 0), a=a)
		# # prob = exp(ddirmn(Y=c(0, 0, nl, 0), alpha=a))
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
# # cat("TEST4\n")
		# prob = multi_MD(x=c(0, 0, 0, nl), a=a)
		# # prob = exp(ddirmn(Y=c(0, 0, 0, nl), alpha=a))
	# }
	# else if ((!is.na(xR_fail[l-1]) && xR <= xR_fail[l-1]) || 
			# (!is.na(xT_fail[l-1]) && xT <= xT_fail[l-1])) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn(xR, xT, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
	# }
	# else if (!is.na(xR_pass[l-1]) && xR-dl >= xR_pass[l-1] && 
			# !is.na(xT_pass[l-1]) && xT-dl >= xT_pass[l-1]) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn(xR-dl, xT-dl, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
	# }
	# else {
		# # min_i = 0
		# # max_i = xR
		# # min_j = 0
		# # max_j = xT
		# min_i = max(0, xR-n[l-1])
		# max_i = min(xR, dl)
		# min_j = max(0, xT-n[l-1])
		# max_j = min(xT, dl)

		# #double check some of these
		# if(!is.na(xR_fail[l-1])) {
			# # max_i = min(max(0, xR-1-xR_fail[l-1]), dl)
			# max_i = min(max_i, max(0, xR-1-xR_fail[l-1]))
		# }
		# if(!is.na(xR_pass[l-1])) {
			# # min_i = min(xR, xR+1-xR_pass[l-1], dl)
			# min_i = max(min_i, xR+1-xR_pass[l-1])
		# }
		# if(!is.na(xT_fail[l-1])) {
			# # max_j = min(max(0, xT-1-xT_fail[l-1]), dl)
			# max_j = min(max_j, max(0, xT-1-xT_fail[l-1]))
		# }
		# if(!is.na(xT_pass[l-1])) {
			# # min_j = min(xT, xT+1-xT_pass[l-1], dl)
			# min_j = max(min_j, xT+1-xT_pass[l-1])
		# }
		
# # cat("nl", nl, "min_i", min_i, "max_i", max_i, "min_j", min_j, "max_j", max_j, "\n")
		# if(min_i <= max_i && min_j <= max_j){
			# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
			# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
			# for (i in min_i:max_i){
				# for(j in min_j:max_j){
					# ppij = 0
					# min_g = max(0,i+j-dl)
					# max_g = min(i, j)
					# x = xR-i
					# y = xT-j
					# # z = n[-l]
					# z = n[l-1]
					# # a_add = c(
						# # (n[-l]-xR+i)*(n[-l]-xT+j)/n[-l], 
						# # (n[-l]-xR+i)*(xT-j)/n[-l], 
						# # (xR-i)*(n[-l]-xT+j)/n[-l], 
						# # (xR-i)*(xT-j)/n[-l])
					# # a_add = z * calc_p_OR(pR = x/z, pT = y/z, phi = a["a00"] * a["a11"] / a["a01"] / a["a10"])
					# phi = a["a00"] * a["a11"] / a["a01"] / a["a10"]
					# # cat(phi, "\n")
					# p = calc_p_OR(pR = x/z, pT = y/z, phi = phi)
					# a_add = z * p
					# # cat("x =", x, "y =", y, "z =", z, "\n")
					# # cat("pR =", x/z, "pT =", y/z, "p =", p, "a =", a_add, "\n")
					# if (min_g <= max_g){
						# for(g in min_g:max_g){
# # cat("dl", dl, "i", i, "j", j, "g", g, "\n")
							# # a_add = n[l-1] - c(dl-i-j+g, j-g, i-g, g)
							# ppij = ppij + multi_MD(x=c(dl-i-j+g, j-g, i-g, g), a=a+a_add) ##edit here
							# # ppij = ppij + multi_MD(x=c(dl-i-j+g, j-g, i-g, g), a=a) ##edit here
							# # ppij = ppij + multi_MD(x=c(dl-i-j+g, j-g, i-g, g), a=a) * pxn_RT_dm_dyn(xR-i, xT-j, n[-l], a = a + c(dl-i-j+g, j-g, i-g, g), cxR = cxR, cxT = cxT, x_n, environment())
						# }
						# prob = prob + ppij * pxn_RT_dm_dyn(xR-i, xT-j, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
						# # prob = prob + ppij * pxn_RT_dm_dyn(xR-i, xT-j, n[-l], a=a+a_add, cxR = cxR, cxT = cxT, x_n, environment())
						# # prob = prob + ppij
					# }
				# }
			# }
		# }
	# }
	# x_n[xR+1, xT+1, l] = prob
	# assign("x_n", x_n, envir = env)
	# return(prob)
# }

pxn_RT_dm2 = function(xR, xT, n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, nk+1, nk+1, k))
	p = calc_p_OR(pT, pR, phi)
	a = lambda * p
	# names(a) = c("a00", "a01", "a10", "a11")
	# print(x_n)
	# x = pxn_RT_dm_dyn2(xR, xT, n, a, cxR, cxT, x_n, environment())
	# print(x_n)
	# return(x)
# }

# pxn_RT_dm_dyn2 = function(xR, xT, n, a, cxR, cxT, x_n, env){
	prob = 0
	i = xR
	j = xT
	min_g = max(0,i+j-nk)
	max_g = min(i, j)
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	for(g in min_g:max_g){
		# prob = prob + pxn_RT_dm_dyn_joint(x = c(nk-i-j+g, j-g, i-g, g), n=n, d=d, a=a, cxR=cxR, cxT=cxT, x_n=x_n, env=environment())
		prob = prob + pxn_RT_dm_dyn_joint(x = c(nk-i-j+g, j-g, i-g, g), n=n, d=d, a=a, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, env=environment())
		# cat("nl =", nl, "xR =", xR, "xT =", xT, "x00 =", nl-i-j+g, "x01 =", j-g, "x10 =", i-g, "x11 =", g, "\n")
	}
	# print(x_n)
	return(prob)
}

#alias
pxn_RT_dm = pxn_RT_dm2

#need to recourse over x00, x01, x10, x11
#in order for all stages to be dirichlet multinomial we must add the previous counts to the prior for the next stage
# pxn_RT_dm_dyn_joint = function(x, n, d, a, cxR, cxT, x_n, env){
pxn_RT_dm_dyn_joint = function(x, n, d, a, xR_fail, xR_pass, xT_fail, xT_pass, x_n, env){
# pxn_RT_dm_dyn_joint = function(x00, x01, x10, x11, n, d, a00, a01, a10, a11, cxR, cxT, x_n, env){
	# call_ct <<- call_ct + 1
	# names(x) = c("x00", "x01", "x10", "x11")
	# xR = x["x11"] + x["x10"]
	# x00 = x[1]
	# x01 = x[2]
	# x10 = x[3]
	# x11 = x[4]
	xR = x[4] + x[3]
	# xR = x11 + x10
	# xT = x["x11"] + x["x01"]
	xT = x[4] + x[2]
	# xT = x11 + x01
	# d = diff(c(0, n))
	l = length(n)
	nl = n[l]
	dl = d[l]
	# xR_fail = cxR$x_fail
	# xR_pass = cxR$x_pass
	# xT_fail = cxT$x_fail
	# xT_pass = cxT$x_pass
	prob = 0
	# stopifnot(length(xR_fail) == l && length(xR_pass) == l)
	# stopifnot(length(xT_fail) == l && length(xT_pass) == l)
	# cat("x00 =", x["x00"], "x01 =", x["x01"], "x10 =", x["x10"], "x11 =", x["x11"], "\n")
	# if (!is.na(x_n[x["x00"]+1, x["x01"]+1, x["x10"]+1, x["x11"]+1, l])) {
	if (xR > nl || xR < 0 || xT > nl || xT < 0) {
		return(0)
	}
	else if (!is.na(x_n[x[1]+1, x[2]+1, x[3]+1, x[4]+1, l])) {
	# if (!is.na(x_n[x00+1, x01+1, x10+1, x11+1, l])) {
		# dyn_ct <<- dyn_ct + 1
		# prob = x_n[x["x00"]+1, x["x01"]+1, x["x10"]+1, x["x11"]+1, l]
		prob = x_n[x[1]+1, x[2]+1, x[3]+1, x[4]+1, l]
		# prob = x_n[x00+1, x01+1, x10+1, x11+1, l]
		return(prob)
	}
	else if(l==1 || (
			is.na(xR_fail[l-1]) && 
			is.na(xR_pass[l-1]) && 
			is.na(xT_fail[l-1]) && 
			is.na(xT_pass[l-1]))){
		prob = multi_MD(x=x, a=a)
		# prob = multi_MD(x00=x00, x01=x01, x10=x10, x11=x11, a00=a00, a01=a01, a10=a10, a11=a11)
	}
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = multi_MD(x=c(nl, 0, 0, 0), a=a)
		# # prob = multi_MD(x00=nl, x01=0, x10=0, x11=0, a00=a00, a01=a01, a10=a10, a11=a11)
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = multi_MD(x=c(0, nl, 0, 0), a=a)
		# # prob = multi_MD(x00=0, x01=nl, x10=0, x11=0, a00=a00, a01=a01, a10=a10, a11=a11)
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = multi_MD(x=c(0, 0, nl, 0), a=a)
		# # prob = multi_MD(x00=0, x01=0, x10=nl, x11=0, a00=a00, a01=a01, a10=a10, a11=a11)
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = multi_MD(x=c(0, 0, 0, nl), a=a)
		# # prob = multi_MD(x00=0, x01=0, x10=0, x11=nl, a00=a00, a01=a01, a10=a10, a11=a11)
	# }
	else if ((!is.na(xR_fail[l-1]) && xR <= xR_fail[l-1]) || 
			(!is.na(xT_fail[l-1]) && xT <= xT_fail[l-1])) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn_joint(x, n[-l], d[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
		prob = pxn_RT_dm_dyn_joint(x, n[-l], d[-l], a, xR_fail=xR_fail[-l], xR_pass=xR_pass[-l], xT_fail=xT_fail[-l], xT_pass=xT_pass[-l], x_n, environment())
		# prob = pxn_RT_dm_dyn_joint(x00=x00, x01=x01, x10=x10, x11=x11, n[-l], d[-l], a00=a00, a01=a01, a10=a10, a11=a11, cxR = cxR, cxT = cxT, x_n, environment())
	}
	else if (!is.na(xR_pass[l-1]) && xR-dl >= xR_pass[l-1] && 
			!is.na(xT_pass[l-1]) && xT-dl >= xT_pass[l-1]) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn_joint(x-dl, n[-l], d[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
		prob = pxn_RT_dm_dyn_joint(x-dl, n[-l], d[-l], a, xR_fail=xR_fail[-l], xR_pass=xR_pass[-l], xT_fail=xT_fail[-l], xT_pass=xT_pass[-l], x_n, environment())
		# prob = pxn_RT_dm_dyn_joint(x00=x00-dl, x01=x01-dl, x10=x10-dl, x11=x11-dl, n[-l], d[-l], a00=a00, a01=a01, a10=a10, a11=a11, cxR = cxR, cxT = cxT, x_n, environment())
	}
	else {
		min_i = max(0, xR-n[l-1])
		max_i = min(xR, dl)
		min_j = max(0, xT-n[l-1])
		max_j = min(xT, dl)

		#double check some of these
		if(!is.na(xR_fail[l-1])) {
			max_i = min(max_i, max(0, xR-1-xR_fail[l-1]))
		}
		if(!is.na(xR_pass[l-1])) {
			min_i = max(min_i, xR+1-xR_pass[l-1])
		}
		if(!is.na(xT_fail[l-1])) {
			max_j = min(max_j, max(0, xT-1-xT_fail[l-1]))
		}
		if(!is.na(xT_pass[l-1])) {
			min_j = max(min_j, xT+1-xT_pass[l-1])
		}
		
		if(min_i <= max_i && min_j <= max_j){
			# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
			# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
			xR_fail_l=xR_fail[-l]
			xR_pass_l=xR_pass[-l]
			xT_fail_l=xT_fail[-l]
			xT_pass_l=xT_pass[-l]
			for (i in min_i:max_i){
				for(j in min_j:max_j){
					# min_g = max(i+j-dl, j-x["x01"], i-x["x10"], 0)
					min_g = max(i+j-dl, j-x[2], i-x[3], 0)
					# min_g = max(i+j-dl, j-x01, i-x10, 0)
					# max_g = min(x["x00"]+i+j-dl, j, i, x["x11"])
					max_g = min(x[1]+i+j-dl, j, i, x[4])
					# max_g = min(x00+i+j-dl, j, i, x11)
					if (min_g <= max_g){
						for(g in min_g:max_g){
							# cat("dl", dl, "i", i, "j", j, "g", g, "\n")
							prob = prob + multi_MD(x=c(dl-i-j+g, j-g, i-g, g), a=a+x-c(dl-i-j+g, j-g, i-g, g)) * 
								# pxn_RT_dm_dyn_joint(x = x-c(dl-i-j+g, j-g, i-g, g), n[-l], d[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
								pxn_RT_dm_dyn_joint(x = x-c(dl-i-j+g, j-g, i-g, g), n[-l], d[-l], a, 
								xR_fail=xR_fail_l, xR_pass=xR_pass_l, xT_fail=xT_fail_l, xT_pass=xT_pass_l, x_n, environment())
							# prob = prob + multi_MD(x00=dl-i-j+g, x01=j-g, x10=i-g, x11=g, 
									# a00=a00+x00-dl+i+j-g, a01=a01+x01-j+g, a10=a10+x10-i+g, a11=a11+x11-g) * 
								# pxn_RT_dm_dyn_joint(x00=x00-dl+i+j-g, x01=x01-j+g, x10=x10-i+g, x11=x11-g, n[-l], d[-l], 
									# a00=a00, a01=a01, a10=a10, a11=a11, cxR = cxR, cxT = cxT, x_n, environment())
						}
						# prob = prob + ppij * pxn_RT_dm_dyn2(xR-i, xT-j, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
					}
				}
			}
		}
	}
	# x_n[x["x00"]+1, x["x01"]+1, x["x10"]+1, x["x11"]+1, l] = prob
	# assign("x_n", x_n, envir = env)
	# assign('x_n[x["x00"]+1, x["x01"]+1, x["x10"]+1, x["x11"]+1, l]', prob, envir = env)
	assign('x_n[x[1]+1, x[2]+1, x[3]+1, x[4]+1, l]', prob, envir = env)
	# assign('x_n[x00+1, x01+1, x10+1, x11+1, l]', prob, envir = env)
	return(prob)
}

# pxn_RT_dm3 = function(xR, xT, n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	# x_n=array(NA,dim=c(max(n)+1, max(n)+1, length(n)))
	# p = calc_p_OR(pT, pR, phi)
	# a = lambda * p
	# names(a) = c("a00", "a01", "a10", "a11")
	# pxn_RT_dm_dyn3(xR, xT, n, a, cxR, cxT, x_n, environment())
# }

# pxn_RT_dm_dyn3 = function(xR, xT, n, a, cxR, cxT, x_n, env){
	# call_ct <<- call_ct + 1
	# d = diff(c(0, n))
	# l = length(n)
	# nl = n[l]
	# dl = d[l]
	# xR_fail = cxR$x_fail
	# xR_pass = cxR$x_pass
	# xT_fail = cxT$x_fail
	# xT_pass = cxT$x_pass
	# prob = 0
	# stopifnot(length(xR_fail) == l && length(xR_pass) == l)
	# stopifnot(length(xT_fail) == l && length(xT_pass) == l)
	# if (!is.na(x_n[xR+1, xT+1, l])) {
		# dyn_ct <<- dyn_ct + 1
		# prob = x_n[xR+1, xT+1, l]
		# return(prob)
	# }
	# else if (xR > nl || xR < 0 || xT > nl || xT < 0) {
		# prob = 0
	# }
	# else if(l==1 || (
			# is.na(xR_fail[l-1]) && 
			# is.na(xR_pass[l-1]) && 
			# is.na(xT_fail[l-1]) && 
			# is.na(xT_pass[l-1]))){
		# for(g in max(0,xR+xT-nl):min(xR, xT)){
			# prob = prob + multi_MD(x=c(nl-xR-xT+g, xT-g, xR-g, g), a=a)
		# }
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = multi_MD(x=c(nl, 0, 0, 0), a=a)
	# }
	# else if (xR == 0 && is.na(xR_fail[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = multi_MD(x=c(0, nl, 0, 0), a=a)
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == 0 && is.na(xT_fail[l-1])) {
		# prob = multi_MD(x=c(0, 0, nl, 0), a=a)
	# }
	# else if (xR == nl && is.na(xR_pass[l-1]) && xT == nl && is.na(xT_pass[l-1])) {
		# prob = multi_MD(x=c(0, 0, 0, nl), a=a)
	# }
	# else if ((!is.na(xR_fail[l-1]) && xR <= xR_fail[l-1]) || 
			# (!is.na(xT_fail[l-1]) && xT <= xT_fail[l-1])) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn3(xR, xT, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
	# }
	# else if (!is.na(xR_pass[l-1]) && xR-dl >= xR_pass[l-1] && 
			# !is.na(xT_pass[l-1]) && xT-dl >= xT_pass[l-1]) {
		# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
		# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
		# prob = pxn_RT_dm_dyn3(xR-dl, xT-dl, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
	# }
	# else {
		# min_i = max(0, xR-n[l-1])
		# max_i = min(xR, dl)
		# min_j = max(0, xT-n[l-1])
		# max_j = min(xT, dl)

		# if(!is.na(xR_fail[l-1])) {
			# max_i = min(max_i, max(0, xR-1-xR_fail[l-1]))
		# }
		# if(!is.na(xR_pass[l-1])) {
			# min_i = max(min_i, xR+1-xR_pass[l-1])
		# }
		# if(!is.na(xT_fail[l-1])) {
			# max_j = min(max_j, max(0, xT-1-xT_fail[l-1]))
		# }
		# if(!is.na(xT_pass[l-1])) {
			# min_j = max(min_j, xT+1-xT_pass[l-1])
		# }
		
		# if(min_i <= max_i && min_j <= max_j){
			# cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
			# cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
			# for (i in min_i:max_i){
				# for(j in min_j:max_j){
					# prob = prob + pxn_RT_dm_dyn3(xR-i, xT-j, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment()) * 
						# sum_MD(xR, xT, i, j, nl, dl, a)
				# }
			# }
		# }
		# # if(min_i <= max_i && min_j <= max_j){
			# # cxR = list(x_fail = xR_fail[-l], x_pass = xR_pass[-l])
			# # cxT = list(x_fail = xT_fail[-l], x_pass = xT_pass[-l])
			# # for (i in min_i:max_i){
				# # for(j in min_j:max_j){
					# # ppij = 0
					# # min_g = max(0,i+j-dl)
					# # max_g = min(i, j)
					# # if (min_g <= max_g){
						# # for(g in min_g:max_g){
							# # ppij = ppij + multi_MD(x = c(dl-i-j+g, j-g, i-g, g), a = a+x-c(dl-i-j+g, j-g, i-g, g))
						# # }
						# # prob = prob + ppij * pxn_RT_dm_dyn3(xR-i, xT-j, n[-l], a, cxR = cxR, cxT = cxT, x_n, environment())
					# # }
				# # }
			# # }
		# # }
	# }
	# x_n[xR+1, xT+1, l] = prob
	# assign("x_n", x_n, envir = env)
	# return(prob)
# }

# sum_MD = function(xR, xT, i, j, nl, dl, a){
	# prob = 0
	# min_h = max(0, xR+xT-nl)
	# max_h = min(xR, xT)
	# if (min_h <= max_h){
		# for(h in min_h:max_h){
			# x = c(nl-xR-xT+h, xT-h, xR-h, h)
			# names(x) = c("x00", "x01", "x10", "x11")
			# min_g = max(i+j-dl, j-x["x01"], i-x["x10"], 0)
			# max_g = min(x["x00"]+i+j-dl, j, i, x["x11"])
			# if (min_g <= max_g){
				# for(g in min_g:max_g){
					# prob = prob + multi_MD(x=c(dl-i-j+g, j-g, i-g, g), a=a+x-c(dl-i-j+g, j-g, i-g, g))
				# }
			# }
		# }
	# }
	# return(prob)
# }

#DM for response only
pxn_dm = function(x, n, p, cx, lambda=2){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=matrix(NA, nrow=nk+1, ncol = k)
	a = lambda * c(1-p, p)
	# names(a) = c("a0", "a1")
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	pxn_dm_dyn(x, n, d, a, x_fail, x_pass, x_n, environment())
	# x_n
}

pxn_dm_dyn = function(x, n, d, a, x_fail, x_pass, x_n, env){
	# invisible(readline(prompt=paste0("n = ", n, " x = ", x)))
	l = length(n)
	nl = n[l]
	dl = d[l]
	# x_fail = cx$x_fail
	# x_pass = cx$x_pass
	prob = 0
	# stopifnot(length(x_fail) == l && length(x_pass) == l)
	# cat(x_n, "\n")
	if (x > nl || x < 0) {
# cat("TEST5\n")
		return(0)
	}
	else if (!is.na(x_n[x+1,l])) {
# cat("TEST1\n")
		prob = x_n[x+1,l]
		return(prob)
	}
	else if (l==1 || (is.na(x_fail[l-1]) && is.na(x_pass[l-1]))){
# cat("TEST2\n")
		# prob = dbinom(x=x, size=nl, prob=p)
		prob = multi_MD(x=c(nl-x,x), a=a)
	}
	# else if (x == 0 && is.na(x_fail[l-1])) {
# # cat("TEST3\n")
		# # prob = ifelse(is.na(x_fail[l-1]), (1-p)^nl, 0)
		# # prob = (1-p)^nl
		# prob = multi_MD(x=c(nl,0), a=a)
	# }
	# else if (x == nl && is.na(x_pass[l-1])) {
# # cat("TEST4\n")
		# # prob = ifelse(is.na(x_pass[l-1]), p^nl, 0)
		# # prob = p^nl
		# prob = multi_MD(x=c(0,nl), a=a)
	# }
	else if (!is.na(x_fail[l-1]) && x <= x_fail[l-1]) {
		# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
		# x_fail = x_fail[-l]
		# x_pass = x_pass[-l]
		prob = pxn_dm_dyn(x, n[-l], d[-l], a, x_fail[-l], x_pass[-l], x_n, environment())
	}
	else if (!is.na(x_pass[l-1]) && x-dl >= x_pass[l-1]) {
		# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
		# x_fail = x_fail[-l]
		# x_pass = x_pass[-l]
		prob = pxn_dm_dyn(x-dl, n[-l], d[-l], a, x_fail[-l], x_pass[-l], x_n, environment())
	}
	else {
		min_i = 0
		max_i = x

		#double check some of these
		if(!is.na(x_fail[l-1])) {
			max_i = min(max(0, x-1-x_fail[l-1]), dl)
		}
		if(!is.na(x_pass[l-1])) {
			min_i = min(x, x+1-x_pass[l-1], dl)
		}
		
		if(min_i <= max_i){
			# cx = list(x_fail = x_fail[-l], x_pass = x_pass[-l])
			x_fail = x_fail[-l]
			x_pass = x_pass[-l]
			for (i in min_i:max_i){
				# prob = prob + dbinom(x=i, size=dl, prob=p) * pxn_dyn(x-i, n[-l], p, cx = cx, x_n, environment())
				prob = prob + multi_MD(x=c(dl-i,i), a=a+c(n[l-1]-x+i,x-i)) * pxn_dm_dyn(x-i, n[-l], d[-l], a, x_fail, x_pass, x_n, environment())
				# prob = prob + multi_MD(x=c(dl-i,i), a=a) * pxn_dm_dyn(x-i, n[-l], a, cx = cx, x_n, environment())
				# prob = prob + dbinom(x=i, size=dl, prob=a[2]/sum(a)) * pxn_dm_dyn(x-i, n[-l], a, cx = cx, x_n, environment())
				# cat("prob =", prob, "l =", l, "x =", x, "i =", i, "dl =", dl, "a =", a, "n =", n, "\n")
			}
		}
	}
	x_n[x+1,l] = prob
	assign("x_n", x_n, envir = env)
	# assign("x_n[x+1,l]", x_n[x+1,l], envir = env)
	# assign('x_n[x+1,l]', prob, envir = env)
	# assign('x_n[x[1]+1,l]', prob, envir = env)
	# "x_n[x+1,l]" <<- prob
	return(prob)
}

#T1E/power
##response only
# p_success = function(n, p, cx){
	# k = length(n)
	# nk = n[k]
	# xk_pass = cx$x_fail[k]+1
	# return(sum(vapply(X = xk_pass:nk, FUN = pxn, FUN.VALUE = 1, n=n, p=p, cx=cx)))
# }

# p_success = function(n, p, cx){
p_success = function(n, p, cx, xmat_nm = NULL, env = NULL){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	x_n=NA
	if(!is.null(xmat_nm) && !is.null(env)){
		x_n=get(xmat_nm, envir = env)
	}
	else {
		x_n=matrix(NA, nrow=nk+1, ncol = k)
	}
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	# xk_pass = cx$x_fail[k]+1
	# xk_pass = x_fail[k]+1
	xk_pass = max(x_fail, -1, na.rm=T)+1
	prob = 0
	# if (xk_pass <= 0) {
		# prob = 1
	# }
	# else 
	if(xk_pass <= nk){
		for(x in xk_pass:nk) {
		# for(x in 0:x_fail[k]) {
			prob = prob + pxn_dyn(x=x, n=n, d=d, p=p, x_fail=x_fail, x_pass=x_pass, x_n = x_n, env = environment())
		}
	}
	if(!is.null(xmat_nm) && !is.null(env)){
		assign(xmat_nm, x_n, envir = env)
	}
	return(prob)
	# return(1-prob)
	# return(sum(vapply(X = xk_pass:nk, FUN = pxn_dyn, FUN.VALUE = 1, n=n, d=d, p=p, cx=cx, x_n = x_n, env = environment())))
}

##response + toxicity independent
p_success_RT = function(n, pR, pT, cxR, cxT){
	return(p_success(n=n, p=pR, cx=cxR) * p_success(n=n, p=pT, cx=cxT))
}

##response + toxicity dependent
# p_success_RT_OR = function(n, pR, pT, cxR, cxT, phi = 1){
	# k = length(n)
	# nk = n[k]
	# xRk_pass = cxR$x_fail[k]+1
	# xTk_pass = cxT$x_fail[k]+1
	# # probs = 0
	# # for(i in xRk_pass:nk){
		# # for(j in xTk_pass:nk){
			# # probs = probs + pxn_RT_OR(xR = i, xT = j, n = n, pR = pR, pT = pT, cxR = cxR, cxT = cxT, phi = phi)
		# # }
	# # }
	# # return(sum(probs))
	# return(sum(vapply(X = xRk_pass:nk, 
		# FUN = function(xR, n, pR, pT, cxR, cxT, phi){
		# sum(vapply(X = xTk_pass:nk, FUN = pxn_RT_OR, FUN.VALUE = 1, 
		# xR = xR, n = n, pR = pR, pT = pT, cxR = cxR, cxT = cxT, phi = phi))}, 
		# FUN.VALUE = 1, n = n, pR = pR, pT = pT, cxR = cxR, cxT = cxT, phi = phi)))
# }

p_success_RT_OR = function(n, pR, pT, cxR, cxT, phi = 1){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == length(n)))
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	d = diff(c(0, n))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, length(n)))
	p = calc_p_OR(pT, pR, phi)
	# xRk_pass = cxR$x_fail[k]+1
	# xTk_pass = cxT$x_fail[k]+1
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	# xRk_pass = xR_fail[k]+1
	xRk_pass = max(xR_fail, -1, na.rm=T)+1
	# xTk_pass = xT_fail[k]+1
	xTk_pass = max(xT_fail, -1, na.rm=T)+1
	prob = 0
	if(xRk_pass <= nk && xTk_pass <= nk){
		for(i in xRk_pass:nk){
			for(j in xTk_pass:nk){
				# prob = prob + pxn_RT_OR_dyn(xR=i, xT=j, n=n, d=d, p=p, cxR=cxR, cxT=cxT, x_n=x_n, environment())
				prob = prob + pxn_RT_OR_dyn(xR=i, xT=j, n=n, d=d, p=p, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, environment())
			}
		}
	}
	return(prob)
}

##response + toxicity prior
p_success_RT_dm = function(n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, nk+1, nk+1, k))
	p = calc_p_OR(pT, pR, phi)
	a = lambda * p
	# names(a) = c("a00", "a01", "a10", "a11")
	# a00 = a[1]
	# a01 = a[2]
	# a10 = a[3]
	# a11 = a[4]
	prob = 0
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	# xRk_pass = cxR$x_fail[k]+1
	# xTk_pass = cxT$x_fail[k]+1
	# xRk_pass = xR_fail[k]+1
	xRk_pass = max(xR_fail, -1, na.rm=T)+1
	# xTk_pass = xT_fail[k]+1
	xTk_pass = max(xT_fail, -1, na.rm=T)+1
	d = diff(c(0, n))
	if(xRk_pass <= nk && xTk_pass <= nk){
		for(i in xRk_pass:nk){
			for(j in xTk_pass:nk){
				min_g = max(0,i+j-nk)
				max_g = min(i, j)
				for(g in min_g:max_g){
					prob = prob + pxn_RT_dm_dyn_joint(x = c(nk-i-j+g, j-g, i-g, g), n=n, d=d, a=a, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, env=environment())
					# prob = prob + pxn_RT_dm_dyn_joint(x00=nk-i-j+g, x01=j-g, x10=i-g, x11=g, n=n, d=d, 
						# a00=a00, a01=a01, a10=a10, a11=a11, cxR=cxR, cxT=cxT, x_n=x_n, env=environment())
				}
			}
		}
	}
	return(prob)

	# return(sum(vapply(X = xRk_pass:nk, 
		# FUN = function(xR, n, pR, pT, cxR, cxT, phi, lambda){
		# sum(vapply(X = xTk_pass:nk, FUN = pxn_RT_dm2, FUN.VALUE = 1, 
		# xR = xR, n = n, pR = pR, pT = pT, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda))}, 
		# FUN.VALUE = 1, n = n, pR = pR, pT = pT, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda)))
}

	# k = length(n)
	# stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	# nk = n[k]
	# x_n=array(NA,dim=c(nk+1, nk+1, nk+1, nk+1, k))
	# p = calc_p_OR(pT, pR, phi)
	# a = lambda * p
	# names(a) = c("a00", "a01", "a10", "a11")
	# prob = 0
	# i = xR
	# j = xT
	# min_g = max(0,i+j-nk)
	# max_g = min(i, j)
	# d = diff(c(0, n))
	# for(g in min_g:max_g){
		# prob = prob + pxn_RT_dm_dyn_joint(x = c(nk-i-j+g, j-g, i-g, g), n=n, d=d, a=a, cxR=cxR, cxT=cxT, x_n=x_n, env=environment())
	# }
	# return(prob)

##response only prior
p_success_dm = function(n, p, cx, lambda=2){
	# k = length(n)
	# nk = n[k]
	# xk_pass = cx$x_pass[k]
	# xk_pass = cx$x_fail[k]+1
	# return(sum(vapply(X = xk_pass:nk, FUN = pxn_dm, FUN.VALUE = 1, n=n, p=p, cx=cx, lambda=lambda)))
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=matrix(NA, nrow=nk+1, ncol = k)
	a = lambda * c(1-p, p)
	# names(a) = c("a0", "a1")
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	# xk_pass = x_fail[k]+1
	xk_pass = max(x_fail, -1, na.rm=T)+1
	# pxn_dm_dyn(x, n, d, a, x_fail, x_pass, x_n, environment())
	prob = 0
	if(xk_pass <= nk){
		for(x in xk_pass:nk) {
			prob = prob + pxn_dm_dyn(x=x, n=n, d=d, a=a, x_fail=x_fail, x_pass=x_pass, x_n=x_n, env = environment())
			# prob = prob + pxn_dyn(x=x, n=n, d=d, p=p, cx=cx, x_n = x_n, env = environment())
		}
	}
	return(prob)
}

# pxn_dm = function(x, n, p, cx, lambda=2){
	# k = length(n)
	# stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	# nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	# a = lambda * c(1-p, p)
	# names(a) = c("a0", "a1")
	# d = diff(c(0, n))
	# x_fail = cx$x_fail
	# x_pass = cx$x_pass
	# pxn_dm_dyn(x, n, d, a, x_fail, x_pass, x_n, environment())
	# x_n
# }


#PET
##response only
# pet = function(n, p, cx){
pet = function(n, p, cx, xmat_nm = NULL, env = NULL){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	x_n=NA
	if(!is.null(xmat_nm) && !is.null(env)){
		x_n=get(xmat_nm, envir = env)
	}
	else {
		x_n=matrix(NA, nrow=nk+1, ncol = k)
	}
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	# xk_et = x_fail[k-1]
	xk_et = max(x_fail[-k], -1, na.rm=T)
	# xk_pass = cx$x_fail[k]+1
	# xk_pass = x_fail[k]+1
	prob = 0
	# if(0 <= xk_et){
	for(x in incr(0:xk_et)) {
		prob = prob + pxn_dyn(x=x, n=n[-k], d=d[-k], p=p, x_fail=x_fail[-k], x_pass=x_pass[-k], x_n = x_n, env = environment())
	}
	# }
	if(!is.null(xmat_nm) && !is.null(env)){
		assign(xmat_nm, x_n, envir = env)
	}
	return(prob)
}

##response + toxicity independent
pet_RT = function(n, pR, pT, cxR, cxT){
	return(1-(1-pet(n=n, p=pR, cx=cxR)) * (1-pet(n=n, p=pT, cx=cxT)))
}

pet_RT_min = function(n, pR0, pR1, pT0, pT1, cxR, cxT){
	return(min(
		pet_RT(n=n, pR=pR0, pT=pT1, cxR=cxR, cxT=cxT), 
		pet_RT(n=n, pR=pR1, pT=pT0, cxR=cxR, cxT=cxT)
	))
}

##response + toxicity dependent
pet_RT_OR = function(n, pR, pT, cxR, cxT, phi = 1){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	d = diff(c(0, n))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, length(n)))
	p = calc_p_OR(pT, pR, phi)
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	# xRk_et = xR_fail[k-1]
	xRk_et = max(xR_fail[-k], -1, na.rm=T)
	# xTk_et = xT_fail[k-1]
	xTk_et = max(xT_fail[-k], -1, na.rm=T)
	prob = 0
	if(k>1){
		for(xR in 0:n[k-1]){
			for(xT in incr(0:ifelse(xR<=xRk_et, n[k-1], xTk_et))){
				prob = prob + pxn_RT_OR_dyn(xR=xR, xT=xT, n=n[-k], d=d[-k], p=p, xR_fail=xR_fail[-k], xR_pass=xR_pass[-k], xT_fail=xT_fail[-k], xT_pass=xT_pass[-k], x_n=x_n, environment())
			}
		}
	}
	return(prob)
}

##response + toxicity prior
pet_RT_dm = function(n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, nk+1, nk+1, k))
	p = calc_p_OR(pT, pR, phi)
	a = lambda * p
	prob = 0
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	# xRk_et = xR_fail[k-1]
	xRk_et = max(xR_fail[-k], -1, na.rm=T)
	# xTk_et = xT_fail[k-1]
	xTk_et = max(xT_fail[-k], -1, na.rm=T)
	d = diff(c(0, n))
	if(k>1){
		for(i in 0:n[k-1]){
			for(j in incr(0:ifelse(i<=xRk_et, n[k-1], xTk_et))){
				min_g = max(0,i+j-n[k-1])
				max_g = min(i, j)
				for(g in min_g:max_g){
					prob = prob + pxn_RT_dm_dyn_joint(x = c(n[k-1]-i-j+g, j-g, i-g, g), n=n[-k], d=d[-k], a=a, xR_fail=xR_fail[-k], xR_pass=xR_pass[-k], xT_fail=xT_fail[-k], xT_pass=xT_pass[-k], x_n=x_n, env=environment())
				}
# cat(i, j, prob, "\n")
			}
		}
	}
	return(prob)
}

##response only prior
###TODO: not tested###
pet_dm = function(n, p, cx, lambda=2){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=matrix(NA, nrow=nk+1, ncol = k)
	a = lambda * c(1-p, p)
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	# xk_et = x_fail[k-1]
	xk_et = max(x_fail[-k], -1, na.rm=T)
	prob = 0
	if(k>1){
		for(x in incr(0:xk_et)) {
			prob = prob + pxn_dm_dyn(x=x, n=n[-k], d=d[-k], a=a, x_fail=x_fail[-k], x_pass=x_pass[-k], x_n=x_n, env = environment())
		}
	}
	return(prob)
}

#Accrual
##response only
# EN = function(n, p, cx){
EN = function(n, p, cx, xmat_nm = NULL, env = NULL){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	x_n=NA
	if(!is.null(xmat_nm) && !is.null(env)){
		x_n=get(xmat_nm, envir = env)
	}
	else {
		x_n=matrix(NA, nrow=nk+1, ncol = k)
	}
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	x = 0
	EN = 0
	cprob = 0
	for(l in 1:k){
		prob = 0
		while(!is.na(x_fail[l]) && x <= x_fail[l]) {
			prob = prob + pxn_dyn(x=x, n=n, d=d, p=p, x_fail=x_fail, x_pass=x_pass, x_n = x_n, env = environment())
			x = x+1
		}
		cprob = cprob + prob
		EN = EN + n[l] * prob
	}
	EN = EN + nk * (1-cprob)
	if(!is.null(xmat_nm) && !is.null(env)){
		assign(xmat_nm, x_n, envir = env)
	}
	return(EN)
}

EN_mat = function(n, p, cx){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# x_n=matrix(NA, nrow=nk+1, ncol = k)
	# x_n=NA
	# if(!is.null(xmat_nm) && !is.null(env)){
		# x_n=get(xmat_nm, envir = env)
	# }
	# else {
		# x_n=matrix(NA, nrow=nk+1, ncol = k)
	# }
	# d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	x = 0
	EN = 0
	cprob = 0
	xprobs = pxn_mat(n=n, p=p, cx=cx)
	for(l in 1:k){
		prob = 0
		while(!is.na(x_fail[l]) && x <= x_fail[l]) {
			# prob = prob + pxn_dyn(x=x, n=n, d=d, p=p, x_fail=x_fail, x_pass=x_pass, x_n = x_n, env = environment())
			prob = prob + xprobs[x+1]
			x = x+1
		}
		cprob = cprob + prob
		EN = EN + n[l] * prob
	}
	EN = EN + nk * (1-cprob)
	# if(!is.null(xmat_nm) && !is.null(env)){
		# assign(xmat_nm, x_n, envir = env)
	# }
	return(EN)
}

##response + toxicity independent
EN_RT = function(n, pR, pT, cxR, cxT){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	xR_n=matrix(NA, nrow=nk+1, ncol = k)
	xT_n=matrix(NA, nrow=nk+1, ncol = k)
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	xR = 0
	xT = 0
	EN = 0
	cprob = 0
	for(l in 1:k){
		probR = 0
		while(!is.na(xR_fail[l]) && xR <= xR_fail[l]) {
			probR = probR + pxn_dyn(x=xR, n=n, d=d, p=pR, x_fail=xR_fail, x_pass=xR_pass, x_n = xR_n, env = environment())
			xR = xR+1
		}
		probT = 0
		while(!is.na(xT_fail[l]) && xT <= xT_fail[l]) {
			probT = probT + pxn_dyn(x=xT, n=n, d=d, p=pT, x_fail=xT_fail, x_pass=xT_pass, x_n = xT_n, env = environment())
			xT = xT+1
		}
		cprob = cprob + 1-(1-probR)*(1-probT)
		EN = EN + n[l] * (1-(1-probR)*(1-probT))
	}
	EN = EN + nk * (1-cprob)
	return(EN)
}

EN_mat_RT = function(n, pR, pT, cxR, cxT){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	# xR_n=matrix(NA, nrow=nk+1, ncol = k)
	# xT_n=matrix(NA, nrow=nk+1, ncol = k)
	# d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	xR = 0
	xT = 0
	EN = 0
	cprob = 0
	xprobsR = pxn_mat(n=n, p=pR, cx=cxR)
	xprobsT = pxn_mat(n=n, p=pT, cx=cxT)
	for(l in 1:k){
		probR = 0
		while(!is.na(xR_fail[l]) && xR <= xR_fail[l]) {
			# probR = probR + pxn_dyn(x=xR, n=n, d=d, p=pR, x_fail=xR_fail, x_pass=xR_pass, x_n = xR_n, env = environment())
			probR = probR + xprobsR[xR+1]
			xR = xR+1
		}
		probT = 0
		while(!is.na(xT_fail[l]) && xT <= xT_fail[l]) {
			# probT = probT + pxn_dyn(x=xT, n=n, d=d, p=pT, x_fail=xT_fail, x_pass=xT_pass, x_n = xT_n, env = environment())
			probT = probT + xprobsT[xT+1]
			xT = xT+1
		}
		cprob = cprob + 1-(1-probR)*(1-probT)
		EN = EN + n[l] * (1-(1-probR)*(1-probT))
	}
	EN = EN + nk * (1-cprob)
	return(EN)
}

EN_RT_max = function(n, pR0, pR1, pT0, pT1, cxR, cxT){
	return(max(
		EN_RT(n=n, pR=pR0, pT=pT1, cxR=cxR, cxT=cxT), 
		EN_RT(n=n, pR=pR1, pT=pT0, cxR=cxR, cxT=cxT)
	))
}

EN_mat_RT_max = function(n, pR0, pR1, pT0, pT1, cxR, cxT){
	return(max(
		EN_mat_RT(n=n, pR=pR0, pT=pT1, cxR=cxR, cxT=cxT), 
		EN_mat_RT(n=n, pR=pR1, pT=pT0, cxR=cxR, cxT=cxT)
	))
}

##response + toxicity dependent
EN_RT_OR = function(n, pR, pT, cxR, cxT, phi = 1){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, length(n)))
	p = calc_p_OR(pT, pR, phi)
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass

	EN = 0
	cprob = 0

	# xR = 0
	xRlmin = 0
	xTlmin = 0

	for(l in 1:k){
		prob = 0
		xR = xRlmin
		while(xR <= n[l]) {
			xT = xTlmin
			while((!is.na(xT_fail[l]) && xT <= xT_fail[l]) || (!is.na(xR_fail[l]) && xR <= xR_fail[l] && xT <= n[l])) {
				prob = prob + pxn_RT_OR_dyn(xR=xR, xT=xT, n=n, d=d, p=p, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, environment())
				xT = xT+1
			}
			xR = xR+1
		}
		cprob = cprob + prob
		EN = EN + n[l] * prob

		# xR = xR_fail[l]+1
		if(!is.na(xR_fail[l])){
			xRlmin = xR_fail[l]+1
		}
		if(!is.na(xT_fail[l])){
			xTlmin = xT_fail[l]+1
		}
	}

	EN = EN + nk * (1-cprob)
	return(EN)
}

##response + toxicity prior
EN_RT_dm = function(n, pR, pT, cxR, cxT, phi = 1, lambda=4){
	k = length(n)
	stopifnot(all(vapply(X = c(cxR, cxT), FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=array(NA,dim=c(nk+1, nk+1, nk+1, nk+1, k))
	# x_n=array(NA,dim=c(nk+1, nk+1, length(n)))
	p = calc_p_OR(pT, pR, phi)
	a = lambda * p
	xR_fail = cxR$x_fail
	xR_pass = cxR$x_pass
	xT_fail = cxT$x_fail
	xT_pass = cxT$x_pass
	d = diff(c(0, n))

	EN = 0
	cprob = 0

	# xR = 0
	xRlmin = 0
	xTlmin = 0

	for(l in 1:k){
		prob = 0
		xR = xRlmin
		while(xR <= n[l]) {
			xT = xTlmin
			# while(xT <= xT_fail[l] || (xR <= xR_fail[l] && xT <= n[l])) {
			while((!is.na(xT_fail[l]) && xT <= xT_fail[l]) || (!is.na(xR_fail[l]) && xR <= xR_fail[l] && xT <= n[l])) {
				min_g = max(0,xR+xT-n[l])
				max_g = min(xR, xT)
				prob2 = 0
				for(g in min_g:max_g){
					prob = prob + pxn_RT_dm_dyn_joint(x = c(n[l]-xR-xT+g, xT-g, xR-g, g), n=n, d=d, a=a, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, env=environment())
					# prob2 = prob2 + pxn_RT_dm_dyn_joint(x = c(nk-xR-xT+g, xT-g, xR-g, g), n=n, d=d, a=a, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, env=environment())
				}
				# prob = prob + prob2
				# prob = prob + pxn_RT_dm2(xR=xR, xT=xT, n=n, pR=pR, pT=pT, cxR=cxR, cxT=cxR, phi = phi, lambda=lambda)
				# prob = prob + pxn_RT_OR_dyn(xR=xR, xT=xT, n=n, d=d, p=p, xR_fail=xR_fail, xR_pass=xR_pass, xT_fail=xT_fail, xT_pass=xT_pass, x_n=x_n, environment())
				# cat(xR, xT, prob, "\n")
				xT = xT+1
			}
			xR = xR+1
		}
		# cat(prob, "\n")
		cprob = cprob + prob
		EN = EN + n[l] * prob

		# xR = xR_fail[l]+1
		if(!is.na(xR_fail[l])) {
			xRlmin = xR_fail[l]+1
		}
		if(!is.na(xT_fail[l])) {
			xTlmin = xT_fail[l]+1
		}
	}
	EN = EN + nk * (1-cprob)
	return(EN)
}

##response only prior
EN_dm = function(n, p, cx, lambda=2){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_n=matrix(NA, nrow=nk+1, ncol = k)
	a = lambda * c(1-p, p)
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_pass = cx$x_pass
	
	x = 0
	EN = 0
	cprob = 0
	for(l in 1:k){
		prob = 0
		while(!is.na(x_fail[l]) && x <= x_fail[l]) {
			prob = prob + pxn_dm_dyn(x=x, n=n, d=d, a=a, x_fail=x_fail, x_pass=x_pass, x_n = x_n, env = environment())
			x = x+1
		}
		cprob = cprob + prob
		EN = EN + n[l] * prob
	}
	EN = EN + nk * (1-cprob)
	return(EN)
}

#All trial stats
##response only
trial_perf = function(n, p0, p1, cx){
	k = length(n)
	nk = n[k]
	x_n0=matrix(NA, nrow=nk+1, ncol = k)
	
	T1E = p_success(n=n, p=p0, cx=cx, xmat_nm = "x_n0", env = environment())
	power = p_success(n=n, p=p1, cx=cx)
	pet = pet(n=n, p=p0, cx=cx, xmat_nm = "x_n0", env = environment())
	EN = EN(n=n, p=p0, cx=cx, xmat_nm = "x_n0", env = environment())

	return(c(T1E = T1E, power = power, pet = pet, EN = EN))
}

##response + toxicity independent
trial_perf_RT = function(n, pR0, pR1, pT0, pT1, cxR, cxT){
	# T1E = max(
		# p_success_RT(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT),
		# p_success_RT(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT)
	# )
	T1E_R = p_success_RT(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT)
	T1E_T = p_success_RT(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT)
	power = unname(p_success_RT(n = n, pR = pR1, pT = pT1, cxR = cxR, cxT = cxT))

	pet = min(
		pet_RT(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT),
		pet_RT(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT)
	)

	EN = max(
		EN_RT(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT),
		EN_RT(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT)
	)
	
	return(c(T1E_R = T1E_R, T1E_T = T1E_T, power = power, pet = pet, EN = EN))
}

##response + toxicity dependent
trial_perf_RT_OR = function(n, pR0, pR1, pT0, pT1, cxR, cxT, phi = 1){
	T1E = max(
		p_success_RT_OR(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi),
		p_success_RT_OR(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi)
	)
	
	power = unname(p_success_RT_OR(n = n, pR = pR1, pT = pT1, cxR = cxR, cxT = cxT, phi = phi))

	pet = min(
		pet_RT_OR(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi),
		pet_RT_OR(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi)
	)

	EN = max(
		EN_RT_OR(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi),
		EN_RT_OR(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi)
	)
	
	return(c(T1E = T1E, power = power, pet = pet, EN = EN))
}

##response + toxicity prior
trial_perf_RT_dm = function(n, pR0, pR1, pT0, pT1, cxR, cxT, phi = 1, lambda=4){
	T1E = max(
		p_success_RT_dm(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda),
		p_success_RT_dm(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda)
	)
	
	power = unname(p_success_RT_dm(n = n, pR = pR1, pT = pT1, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda))

	pet = min(
		pet_RT_dm(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda),
		pet_RT_dm(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda)
	)

	EN = max(
		EN_RT_dm(n = n, pR = pR1, pT = pT0, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda),
		EN_RT_dm(n = n, pR = pR0, pT = pT1, cxR = cxR, cxT = cxT, phi = phi, lambda = lambda)
	)
	
	return(c(T1E = T1E, power = power, pet = pet, EN = EN))
}

##response only prior
trial_perf_dm = function(n, p0, p1, cx, lambda = 2){
	# k = length(n)
	# nk = n[k]
	# x_n0=matrix(NA, nrow=nk+1, ncol = k)
	
	# T1E = p_success_dm(n=n, p=p0, cx=cx, lambda = lambda, xmat_nm = "x_n0", env = environment())
	T1E = p_success_dm(n=n, p=p0, cx=cx, lambda = lambda)
	power = p_success_dm(n=n, p=p1, cx=cx, lambda = lambda)
	# pet = pet_dm(n=n, p=p0, cx=cx, lambda = lambda, xmat_nm = "x_n0", env = environment())
	pet = pet_dm(n=n, p=p0, cx=cx, lambda = lambda)
	# EN = EN_dm(n=n, p=p0, cx=cx, lambda = lambda, xmat_nm = "x_n0", env = environment())
	EN = EN_dm(n=n, p=p0, cx=cx, lambda = lambda)

	return(c(T1E = T1E, power = power, pet = pet, EN = EN))
}

#does n pass beta and alpha?
##response only
# minimax = function(alpha_R, alpha_T, beta, p0, p1, a0 = p0, b0 = 1-p0){
feasible_n = function(n, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0){
	# null_df = data.frame(n = character(0), theta_T = numeric(0), theta_L = numeric(0), x_fail=character(0), T1E = numeric(0), power = numeric(0), EN = numeric(0), pet = numeric(0))
	null_df = data.frame()
	# alpha = 0.1
	# beta = 0.1
	# outer(X = 0:1000/1000, Y = 1:999/1000, FUN = calc_cutoffs, theta_U = 1, n = n_bd, p0 = 0.6, p1 = 0.8)
	# t_grid = expand.grid(theta_L = 0:100/100, theta_T = 1:99/100)
	theta_x_l = calc_theta_grid(n=n, p0=p0)
	
	#theta grid to cx grid should really go in another function
	t_grid = rbind(data.frame(theta_T=unique(theta_x_l$theta_T),theta_L=0), unique(theta_x_l[,c("theta_T", "theta_L")]))
	t_grid = t_grid[t_grid$theta_L < 1,]
	# cx_grid = mapply(FUN = calc_cutoffs, t_grid$theta_L, t_grid$theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))
	cx_grid = mapply(FUN = calc_cutoff_grid, t_grid$theta_T, t_grid$theta_L, 
		MoreArgs = list(n = n, theta_T_exp = theta_x_l$theta_T, theta_L_exp = theta_x_l$theta_L, 
		x_exp = theta_x_l$x_exp, l_exp = theta_x_l$l_exp))
	cx_dup = duplicated(t(cx_grid))
	nodup_idx = which(!cx_dup)
	# t_grid_nodup = t_grid[!cx_dup,]
	# cx_grid_nodup = cx_grid[,!cx_dup]
	# mapply(FUN = function(x,y){list(x,y)}, cx_grid_nodup[1,], cx_grid_nodup[2,])
	# for(idx in 1:ncol(cx_grid_nodup)){
		# cat(p_success(n = lee_liu_n, p = 0.2, cx = cx_grid_nodup[,idx]), "\n")
	# }
	# T1E = vapply(X = 1:ncol(cx_grid_nodup), FUN = function(idx){p_success(n = n, p = p0, cx = cx_grid_nodup[,idx])}, FUN.VALUE = 1.)
	T1E = vapply(X = nodup_idx, FUN = function(idx){p_success(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	if(!any(T1E < alpha)){return(null_df)}
	nodup_alpha_pass_idx = nodup_idx[which(T1E < alpha)]
	# t_grid_nodup_alpha = t_grid_nodup[T1E < alpha,]
	# cx_grid_nodup_alpha = cx_grid_nodup[,T1E < alpha]
	T1E_pass = T1E[T1E < alpha]
	
	# power = vapply(X = 1:ncol(cx_grid_nodup_alpha), FUN = function(idx){p_success(n = n, p = p1, cx = cx_grid_nodup_alpha[,idx])}, FUN.VALUE = 1.)
	power = vapply(X = nodup_alpha_pass_idx, FUN = function(idx){p_success(n = n, p = p1, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	if(!any(1-power < beta)){return(null_df)}
	nodup_alpha_power_pass_idx = nodup_alpha_pass_idx[which(1-power < beta)]
	# t_grid_nodup_alpha_power = t_grid_nodup_alpha[1-power < beta,]
	# cx_grid_nodup_alpha_power = cx_grid_nodup_alpha[,1-power < beta]
	T1E_pass_power = T1E_pass[1-power < beta]
	power_pass = power[1-power < beta]
	
	# exp_accrual = vapply(X = 1:ncol(cx_grid_nodup_alpha_power), FUN = function(idx){EN(n = n, p = p0, cx = cx_grid_nodup_alpha_power[,idx])}, FUN.VALUE = 1.)
	exp_accrual = vapply(X = nodup_alpha_power_pass_idx, FUN = function(idx){EN(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	
	# data.frame(t_grid_nodup_alpha_power, x_fail=vapply(X = cx_grid_nodup_alpha_power[1,], FUN = paste0, FUN.VALUE = " ", collapse = ","), T1E_pass_power, power_pass, exp_accrual)
	
	optim_idx = which.min(exp_accrual)
	nodup_alpha_power_pass_optim_idx = nodup_alpha_power_pass_idx[optim_idx]
	exp_accrual_optim = exp_accrual[optim_idx]
	# t_optim = t_grid_nodup_alpha_power[optim_idx,]
	t_optim = t_grid[nodup_alpha_power_pass_optim_idx,]
	# cx_optim = cx_grid_nodup_alpha_power[,optim_idx]
	cx_optim = cx_grid[,nodup_alpha_power_pass_optim_idx]
	T1E_optim = T1E_pass_power[optim_idx]
	power_optim = power_pass[optim_idx]
	pet_optim = pet(n = n, p = p0, cx = cx_optim)

	optim_stats = data.frame(n = paste0(n, collapse=","), t_optim, x_fail=paste0(cx_optim$x_fail, collapse=","), T1E = T1E_optim, power = power_optim, EN = exp_accrual_optim, pet = pet_optim)
	return(optim_stats)
	
	# apply(X = t(cx_grid_nodup), MARGIN = 1, FUN = p_success, n = lee_liu_n, p = 0.2)
	
	# for(theta_L in 0:1000/1000){
		# for(theta_T in 1:999/1000){
			# cx = calc_cutoffs(theta_L = 0, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
			# alpha0 = p_success(n=n, p=p1, cx=cx)
			# power = p_success(n=n, p=p1, cx=cx)
			# if(){
				#add to the set of points
			# }
			# else{
				# break
			# }
		# }
	# }
	# if(){
		# return(c(n=n, alpha0 = alpha0, power = power))
	# }
	# else{
		# return()
	# }
}

feasible_n2 = function(n, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0){
	null_df = data.frame()
	theta_x_l = calc_theta_grid(n=n, p0=p0)
	
	t_grid = rbind(data.frame(theta_T=unique(theta_x_l$theta_T),theta_L=0), unique(theta_x_l[,c("theta_T", "theta_L")]))
	t_grid = t_grid[t_grid$theta_L < 1,]
	cx_grid = mapply(FUN = calc_cutoff_grid, t_grid$theta_T, t_grid$theta_L, 
		MoreArgs = list(n = n, theta_T_exp = theta_x_l$theta_T, theta_L_exp = theta_x_l$theta_L, 
		x_exp = theta_x_l$x_exp, l_exp = theta_x_l$l_exp))
	cx_dup = duplicated(t(cx_grid))
	nodup_idx = which(!cx_dup)
	n_nodup = length(nodup_idx)
	
	T1E = rep(NA, n_nodup)
	for(i in 1:n_nodup){
		idx = nodup_idx[i]
		if(length(which(T1E >= alpha & t_grid$theta_T[nodup_idx] >= t_grid$theta_T[idx] & t_grid$theta_L[nodup_idx] >= t_grid$theta_L[idx]))>0){
			T1E[i] = 1
		} 
		else if(length(which(T1E < alpha & t_grid$theta_T[nodup_idx] <= t_grid$theta_T[idx] & t_grid$theta_L[nodup_idx] <= t_grid$theta_L[idx]))>0){
			T1E[i] = 0
		} 
		else {
			T1E[i] = p_success(n = n, p = p0, cx = cx_grid[,idx])
		}
	}

	# T1E2 = vapply(X = nodup_idx, FUN = function(idx){p_success(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	# cbind(t_grid, T1E, T1E2)
	# plot(T1E,T1E2); abline(h=0.2)
	if(!any(T1E < alpha)){return(null_df)}
	nodup_alpha_pass_idx = nodup_idx[which(T1E < alpha)]
	T1E_pass = T1E[T1E < alpha]
	n_nodup_alpha_pass = length(nodup_alpha_pass_idx)
	
	power = rep(NA, n_nodup_alpha_pass)
	for(i in 1:n_nodup_alpha_pass){
		idx = nodup_alpha_pass_idx[i]
		if(length(which(1-power < beta & t_grid$theta_T[nodup_alpha_pass_idx] >= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] >= t_grid$theta_L[idx]))>0){
			power[i] = 1
		} 
		else if(length(which(1-power >= beta & t_grid$theta_T[nodup_alpha_pass_idx] <= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] <= t_grid$theta_L[idx]))>0){
			power[i] = 0
		} 
		else {
			power[i] = p_success(n = n, p = p1, cx = cx_grid[,idx])
		}
	}
	
	# power2 = vapply(X = nodup_alpha_pass_idx, FUN = function(idx){p_success(n = n, p = p1, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	# cbind(power, power2)
	# plot(power,power2); abline(h=0.8)
	if(!any(1-power < beta)){return(null_df)}
	nodup_alpha_power_pass_idx = nodup_alpha_pass_idx[which(1-power < beta)]
	T1E_pass_power = T1E_pass[1-power < beta]
	power_pass = power[1-power < beta]
	
	exp_accrual = vapply(X = nodup_alpha_power_pass_idx, FUN = function(idx){EN(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	
	
	optim_idx = which.min(exp_accrual)
	nodup_alpha_power_pass_optim_idx = nodup_alpha_power_pass_idx[optim_idx]
	exp_accrual_optim = exp_accrual[optim_idx]
	t_optim = t_grid[nodup_alpha_power_pass_optim_idx,]
	cx_optim = cx_grid[,nodup_alpha_power_pass_optim_idx]
	# T1E_optim = T1E_pass_power[optim_idx]
	T1E_optim = p_success(n = n, p = p0, cx = cx_grid[,nodup_alpha_power_pass_optim_idx])
	# power_optim = power_pass[optim_idx]
	power_optim = p_success(n = n, p = p1, cx = cx_grid[,nodup_alpha_power_pass_optim_idx])
	pet_optim = pet(n = n, p = p0, cx = cx_optim)

	optim_stats = data.frame(n = paste0(n, collapse=","), t_optim, x_fail=paste0(cx_optim$x_fail, collapse=","), T1E = T1E_optim, power = power_optim, EN = exp_accrual_optim, pet = pet_optim)
	return(optim_stats)
}

# feasible_n3 = function(n, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0, ___){
	# null_df = data.frame()
	# theta_x_l = calc_theta_grid(n=n, p0=p0)
	
	# t_grid = rbind(data.frame(theta_T=unique(theta_x_l$theta_T),theta_L=0), unique(theta_x_l[,c("theta_T", "theta_L")]))
	# t_grid = t_grid[t_grid$theta_L < 1,]
	# cx_grid = mapply(FUN = calc_cutoff_grid, t_grid$theta_T, t_grid$theta_L, 
		# MoreArgs = list(n = n, theta_T_exp = theta_x_l$theta_T, theta_L_exp = theta_x_l$theta_L, 
		# x_exp = theta_x_l$x_exp, l_exp = theta_x_l$l_exp))
	# cx_dup = duplicated(t(cx_grid))
	# nodup_idx = which(!cx_dup)
	# n_nodup = length(nodup_idx)
	
	# T1E = rep(NA, n_nodup)
	# for(i in 1:n_nodup){
		# idx = nodup_idx[i]
		# if(length(which(T1E >= alpha & t_grid$theta_T[nodup_idx] >= t_grid$theta_T[idx] & t_grid$theta_L[nodup_idx] >= t_grid$theta_L[idx]))>0){
			# T1E[i] = 1
		# } 
		# else if(length(which(T1E < alpha & t_grid$theta_T[nodup_idx] <= t_grid$theta_T[idx] & t_grid$theta_L[nodup_idx] <= t_grid$theta_L[idx]))>0){
			# T1E[i] = 0
		# } 
		# else if () {
			# T1E[i] = 1
		# }
		# else if () {
			# T1E[i] = 0
		# }
		# else {
			# T1E[i] = p_success(n = n, p = p0, cx = cx_grid[,idx])
		# }
	# }

	# if(!any(T1E < alpha)){return(null_df)}
	# nodup_alpha_pass_idx = nodup_idx[which(T1E < alpha)]
	# T1E_pass = T1E[T1E < alpha]
	# n_nodup_alpha_pass = length(nodup_alpha_pass_idx)
	
	# power = rep(NA, n_nodup_alpha_pass)
	# for(i in 1:n_nodup_alpha_pass){
		# idx = nodup_alpha_pass_idx[i]
		# if(length(which(1-power[] < beta & t_grid$theta_T[nodup_alpha_pass_idx] >= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] >= t_grid$theta_L[idx]))>0){
			# power[i] = 1
		# } 
		# else if(length(which(1-power[] >= beta & t_grid$theta_T[nodup_alpha_pass_idx] <= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] <= t_grid$theta_L[idx]))>0){
			# power[i] = 0
		# } 
		# else if (length(which(1 - power < beta & compare_n_cx_v(n1 = n[], cx1 = cx_grid[,idx], n2 = n, cx2 = cx_grid, compfunc = "<=")))) {
			# T1E[i] = 1
		# }
		# else if () {
			# T1E[i] = 0
		# }
		# else {
			# power[i] = p_success(n = n, p = p1, cx = cx_grid[,idx])
		# }
	# }
	
	# if(!any(1-power < beta)){return(null_df)} #TODO: need to return everything
	# nodup_alpha_power_pass_idx = nodup_alpha_pass_idx[which(1-power < beta)]
	# T1E_pass_power = T1E_pass[1-power < beta]
	# power_pass = power[1-power < beta]
	
	# exp_accrual = vapply(X = nodup_alpha_power_pass_idx, FUN = function(idx){EN(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	
	# optim_idx = which.min(exp_accrual)
	# nodup_alpha_power_pass_optim_idx = nodup_alpha_power_pass_idx[optim_idx]
	# exp_accrual_optim = exp_accrual[optim_idx]
	# t_optim = t_grid[nodup_alpha_power_pass_optim_idx,]
	# cx_optim = cx_grid[,nodup_alpha_power_pass_optim_idx]
	# T1E_optim = p_success(n = n, p = p0, cx = cx_grid[,nodup_alpha_power_pass_optim_idx])
	# power_optim = p_success(n = n, p = p1, cx = cx_grid[,nodup_alpha_power_pass_optim_idx])
	# pet_optim = pet(n = n, p = p0, cx = cx_optim)

	# optim_stats = data.frame(n = paste0(n, collapse=","), t_optim, x_fail=paste0(cx_optim$x_fail, collapse=","), T1E = T1E_optim, power = power_optim, EN = exp_accrual_optim, pet = pet_optim)
	# return(optim_stats) #TODO: need to return everything
# }

##response + toxicity independent
##response + toxicity dependent
##response + toxicity prior
##response only prior

#search
#can set number of stages k or fixed size of stages. also an option to set the minimum n1
##response only
calc_n = function(nk, k, min_n1, d){
	stopifnot(min_n1 > 0 & min_n1 < nk)
	stopifnot((is.null(k) || is.na(k)) || (is.null(d) || is.na(d)))
	if(!(is.null(k) || is.na(k)) && k <= nk - min_n1+1) { #the timing of these stages need to be optimized
		if(min_n1 == nk-1){
			return(list(min_n1:nk))
		}
		else{
			return(combn(x = min_n1:(nk-1), m = k-1, FUN = c, simplify = F, nk))
		}
	}
	else if (!(is.null(d) || is.na(d))) {
		n1 = min(max(min_n1, d, 1, na.rm=T), nk, na.rm=T)
		return(list(unique(c(seq(from=n1, to=nk, by=d), nk))))
	}
	else { #both are null
		return(list(min_n1:nk))
	}
}

search_n = function(k = NULL, min_n1 = 1, d = NULL, max_n = 100, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0){
	stopifnot(is.null(k) || is.null(d))
	# for (nk in max(min_n1+1, 2, na.rm=T):max_n){
	for (nk in max(min_n1+1, 2, min_n1+k-1, na.rm=T):max_n){
		# cat(nk, "\n")
		n = calc_n(nk = nk, k = k, min_n1 = min_n1, d = d)
		# if(!is.null(k)) {
			# n = 
		# }
		# else if (!is.null(d)) {
			
		# }
		# else { #both are null
			# n = min_n1:nk
		# }
		# n_stats_l = lapply(X = n, FUN = feasible_n, alpha = alpha, beta = beta, p0 = p0, p1 = p1, a0 = a0, b0 = b0)
		n_stats_l = lapply(X = n, FUN = feasible_n2, alpha = alpha, beta = beta, p0 = p0, p1 = p1, a0 = a0, b0 = b0)
		n_stats = do.call(rbind, n_stats_l)
		# __ = feasible_n(n = n, alpha = alpha, beta = beta, p0 = p0, p1 = p1, a0 = a0, b0 = b0)
		if(nrow(n_stats) > 0){
			#minimax design
			# break
			return(n_stats[which.min(n_stats$EN),])
		}
	}
	return(data.frame())
	
	# n_stats
	#optim_n
	# which.min()
	# return(___)
}

compare_n_cx = function(n1, cx1, n2, cx2, compfunc){
	compfunc = match.fun(compfunc)
	n1c = as.character(n1)
	n2c = as.character(n2)
	x1_fail = cx1$x_fail
	# x1_pass = cx1$x_pass
	x2_fail = cx2$x_fail
	# x2_pass = cx2$x_pass
	# n1_cx1 = data.frame(n=n1, x_fail = x1_fail, x_pass = x1_pass)
	n1_cx1 = cbind(n=n1, x_fail = x1_fail)
	# n2_cx2 = data.frame(n=n2, x_fail = x2_fail, x_pass = x2_pass)
	n2_cx2 = cbind(n=n2, x_fail = x2_fail)
	# n_cx = merge(n1_cx1, n2_cx2, by = "n", suffixes = c(".1", ".2"), all=T)
	n = as.character(sort(unique(c(n1, n2))))
	n_cx1 = integer(0)
	n_cx1[n] = NA
	n_cx1[n1c] = x1_fail
	prevx = -1
	for(idx in n){
		if(is.na(n_cx1[idx])){
			n_cx1[idx] = prevx
		}
		else {
			prevx = n_cx1[idx]
		}
	}
	
	n_cx2 = integer(0)
	n_cx2[n] = NA
	n_cx2[n2c] = x2_fail
	prevx = -1
	for(idx in n){
		if(is.na(n_cx2[idx])){
			n_cx2[idx] = prevx
		}
		else {
			prevx = n_cx2[idx]
		}
	}
	
	# n_cxf = fill(n_cx, x_fail.1, x_fail.2)
	# n_cxf$x_fail.1[is.na(n_cxf$x_fail.1)] = -1
	# n_cxf$x_fail.2[is.na(n_cxf$x_fail.2)] = -1
	# all(compfunc(n_cxf$x_fail.1, n_cxf$x_fail.2))
	all(compfunc(n_cx1, n_cx2))
	# min_n = min(n1, n2)
	# max_n = max(n1, n2)
	
}

compare_n_cx_v = Vectorize(FUN = compare_n_cx, vectorize.args = c("n2", "cx2"))

n_cx_lte = function(n1, cx1, n2, cx2){
	# n1 = n_bd; cx1 = cxR_bd; n2 = n_bd+1; cx2 = cxT_bd #for testing
	# n1 = n_bd; cx1 = cxR_bd; n2 = n_bd+14; cx2 = cxT_bd #for testing
	# n1 = lee_liu_n; cx1 = cx_ex1; n2 = n_bd-9; cx2 = cxR_bd
	x1_fail = cx1$x_fail
	x1_fail_exp = c(x1_fail, -1, x1_fail[-length(x1_fail)])
	# x1_pass = cx1$x_pass
	x2_fail = cx2$x_fail
	x2_fail_exp = c(x2_fail, -1, x2_fail[-length(x2_fail)])
	# x2_pass = cx2$x_pass
	
	# outer(n1, n2, FUN = "<=")
	# apply(X = outer(n1, n2, FUN = "<="), FUN = which, MARGIN = 2)
	# unlist(apply(X = outer(n1, n2, FUN = "<="), FUN = which, MARGIN = 2))
	n_idx = which(outer(c(n1, n1-1), c(n2, n2-1), FUN = "<="), arr.ind=T)
	# n_idx = ((which(outer(n1, n2, FUN = "<="))-1) %% length(n1))+1
	# arrayInd(which(outer(n1, n2, FUN = "<=")),length(n1))
	
	# n_idx = which(n1 <= n2[1])
	# all(x1_fail[n_idx[,1]] <= x2_fail[n_idx[,2]])
	all(x1_fail_exp[n_idx[,1]] <= x2_fail_exp[n_idx[,2]])
}

# n_cx_lte_v = function(n1, cx1, n2, cx2){
	# # n1 = n_bd; cx1 = cxR_bd; n2 = list(n_bd, n_bd-1, n_bd+1, n_bd-14, n_bd+14); cx2 = list(cxT_bd, cxT_bd, cxT_bd, cxT_bd, cxT_bd)
	# x1_fail = cx1$x_fail
	# x1_fail_exp = c(x1_fail, -1, x1_fail[-length(x1_fail)])
	# x2_fail = lapply(X = cx2, FUN = getElement, name = "x_fail")
	# x2_fail_exp = lapply(X = x2_fail, FUN = function(x){c(x, -1, x[-length(x)])})
	# n2_exp = lapply(X = n2, FUN = function(n){c(n, n-1)})
	
	# # n_idx = which(outer(c(n1, n1-1), c(n2, n2-1), FUN = "<="), arr.ind=T)
	# n_idx = which(outer(c(n1, n1-1), unlist(n2_exp), FUN = "<="), arr.ind=T)
	
	# all(x1_fail_exp[n_idx[,1]] <= unlist(x2_fail_exp)[n_idx[,2]])
# }

n_cx_lte_v1 = Vectorize(FUN = n_cx_lte, vectorize.args = c("n1", "cx1"))
n_cx_lte_v2 = Vectorize(FUN = n_cx_lte, vectorize.args = c("n2", "cx2"))

search_n2 = function(k = NULL, min_n1 = 1, d = NULL, max_n = 100, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0){
	#k = 2; min_n1 = 8; d = NULL; max_n = 11; alpha = 0.2; beta = 0.2; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0
	#k = NULL; min_n1 = 10; d = 5; max_n = 20; alpha = 0.2; beta = 0.2; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0
	#k = NULL; min_n1 = 10; d = NULL; max_n = 50; alpha = 0.1; beta = 0.1; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0 #example 1 from lee & liu
	#k = 2; min_n1 = 20; d = NULL; max_n = 60; alpha = 0.05; beta = 0.2; p0 = 0.05; p1 = 0.15; a0 = p0; b0 = 1-p0 # Anastasia Ivanova UNC example
	#k = 2; min_n1 = 19; d = NULL; max_n = 33; alpha = 0.15; beta = 0.15; p0 = 0.3; p1 = 0.5; a0 = p0; b0 = 1-p0 #bryant & day example
	#k = NULL; min_n1 = 19; d = 19; max_n = 33; alpha = 0.15; beta = 0.15; p0 = 0.3; p1 = 0.5; a0 = p0; b0 = 1-p0 #bryant & day example (alternate)
	#check input
	stopifnot(is.null(k) || is.null(d))
	null_df = data.frame()
	#
	nk = max(min_n1+1, 2, min_n1+k-1, na.rm=T):max_n
	n = do.call(c,lapply(X = nk, FUN = calc_n, k = k, min_n1 = min_n1, d = d))
	
	# for (nk in max(min_n1+1, 2, min_n1+k, na.rm=T):max_n){
		# n = calc_n(nk = nk, k = k, min_n1 = min_n1, d = d)
		
		#calc theta vals
	theta_x_l = lapply(X = n, FUN = calc_theta_grid, p0 = p0, a0 = a0, b0 = b0)
	t_grid = lapply(X = theta_x_l, FUN = function(x){rbind(data.frame(theta_T=unique(x$theta_T),theta_L=0), unique(x[,c("theta_T", "theta_L")]))})
	t_grid = lapply(X = t_grid, FUN = function(x){x[x$theta_L < 1,]})
	cx_grid = lapply(X = 1:length(n), FUN = function(i, t_grid, n, theta_x_l){
		mapply(FUN = calc_cutoff_grid, t_grid[[i]]$theta_T, t_grid[[i]]$theta_L, 
			MoreArgs = list(n = n[[i]], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, 
			x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp))}, 
		t_grid=t_grid, n=n, theta_x_l=theta_x_l)

	cx_dup = lapply(X = cx_grid, FUN = function(cx){duplicated(t(cx))})
	nodup_idx = lapply(X = cx_dup, FUN = function(x){which(!x)})
	n_nodup = lapply(X = nodup_idx, FUN = length)

	#expand the n with rep function
	# n_grid = do.call(c,lapply(X = 1:length(n), 
		# FUN = function(i, n_nodup, n){rep(x = list(n[[i]]), times = n_nodup[[i]])}, 
		# n_nodup=n_nodup, n=n))
	
		#calc cx vals
		
		#check previous theta
		#check previous n/cx
		#calc T1E
# profvis({
	T1E = lapply(X = n_nodup, FUN = rep, x = NA)
	# pn = list()
	# pcx = list()
	# prev = data.frame(n = character(0), theta_T = numeric(0), theta_L = numeric(0), x_fail=character(0), T1E = numeric(0), power = numeric(0), EN = numeric(0), pet = numeric(0))
	for(i in 1:length(n)){
		# cat(i, "\n")
		if(i > 1){
			pn = do.call(c,lapply(X = 1:(i-1), 
				FUN = function(i, n_nodup, n){rep(x = list(n[[i]]), times = n_nodup[[i]])}, 
				n_nodup=n_nodup, n=n))
			# pcx = do.call(cbind,lapply(X = 1:(i-1), FUN = getElement, object = cx_grid))
			# split(cx_grid[[1]], rep(1:ncol(cx_grid[[1]]), each = nrow(cx_grid[[1]])))
			pcx = do.call(c,lapply(X = 1:(i-1), FUN=function(i, cx){lapply(X=seq_len(ncol(cx[[i]])), FUN=function(j) cx[[i]][,j])}, cx = cx_grid))
			pT1E = unlist(lapply(X = 1:(i-1), FUN = getElement, object = T1E))
		}
		for(j in 1:n_nodup[[i]]){
			idx = nodup_idx[[i]][j]
			if(any(T1E[[i]] >= alpha & t_grid[[i]]$theta_T[nodup_idx[[i]]] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 1
				# cat("TEST1\n")
			} 
			else if(any(T1E[[i]] < alpha & t_grid[[i]]$theta_T[nodup_idx[[i]]] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 0
				# cat("TEST2\n")
			} 
			# else if (i > 1 && any(pT1E >= alpha & compare_n_cx_v(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx, compfunc = "<="), na.rm=T)) {
			# else if (i > 1 && any(pT1E >= alpha & n_cx_lte_v2(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx), na.rm=T)) {
				# T1E[[i]][j] = 1
				# cat("TEST3\n")
			# }
			# else if (i > 1 && any(pT1E < alpha & compare_n_cx_v(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx, compfunc = ">="), na.rm=T)) {
			# else if (i > 1 && any(pT1E < alpha & n_cx_lte_v1(n2 = n[[i]], cx2 = cx_grid[[i]][,idx], n1 = pn, cx1 = pcx), na.rm=T)) {
				# T1E[[i]][j] = 0
				# cat("TEST4\n")
			# }
			else {
				T1E[[i]][j] = p_success(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
				# cat("TEST5\n")
			}
		}
	}
# })

	if(!any(unlist(T1E) < alpha)){return(null_df)}
	# nodup_alpha_pass_idx = nodup_idx[which(T1E < alpha)]
	nodup_alpha_pass_idx = lapply(X = 1:length(n), 
		FUN = function(i, T1E, alpha, nodup_idx){nodup_idx[[i]][which(T1E[[i]] < alpha)]}, 
		T1E = T1E, alpha = alpha, nodup_idx = nodup_idx)
	# T1E_pass = T1E[T1E < alpha]
	T1E_pass = lapply(X = T1E, FUN = function(x,a){x[which(x<a)]}, a = alpha)
	# n_nodup_alpha_pass = length(nodup_alpha_pass_idx)
	n_nodup_alpha_pass = lapply(X = nodup_alpha_pass_idx, FUN = length)
	
		#check previous theta
		#check previous n/cx
		#calc power
	# power = rep(NA, n_nodup_alpha_pass)
	pow = lapply(X = n_nodup_alpha_pass, FUN = rep, x = NA)
	# for(i in 1:n_nodup_alpha_pass){
		# idx = nodup_alpha_pass_idx[i]
		# if(length(which(1-power < beta & t_grid$theta_T[nodup_alpha_pass_idx] >= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] >= t_grid$theta_L[idx]))>0){
			# power[i] = 1
		# } 
		# else if(length(which(1-power >= beta & t_grid$theta_T[nodup_alpha_pass_idx] <= t_grid$theta_T[idx] & t_grid$theta_L[nodup_alpha_pass_idx] <= t_grid$theta_L[idx]))>0){
			# power[i] = 0
		# } 
		# else {
			# power[i] = p_success(n = n, p = p1, cx = cx_grid[,idx])
		# }
	# }
	for(i in 1:length(n)){
		# cat(i, "\n")
		if(i > 1){
			pn = do.call(c,lapply(X = 1:(i-1), 
				FUN = function(i, n_nodup_alpha_pass, n){rep(x = list(n[[i]]), times = n_nodup_alpha_pass[[i]])}, 
				n_nodup_alpha_pass=n_nodup_alpha_pass, n=n))
			pcx = do.call(c,lapply(X = 1:(i-1), FUN=function(i, cx){lapply(X=nodup_alpha_pass_idx[[i]], FUN=function(j) cx[[i]][,j])}, cx = cx_grid))
			ppower = unlist(lapply(X = 1:(i-1), FUN = getElement, object = pow))
		}
		for(j in 1:n_nodup_alpha_pass[[i]]){
			idx = nodup_alpha_pass_idx[[i]][j]
			if(any(1-pow[[i]] < beta & t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				pow[[i]][j] = 1
				# cat("TEST6\n")
			} 
			else if(any(1-pow[[i]] >= beta & t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				pow[[i]][j] = 0
				# cat("TEST7\n")
			} 
			# else if (i > 1 && any(1-ppower < beta & compare_n_cx_v(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx, compfunc = "<="), na.rm=T)) {
			# else if (i > 1 && any(1-ppower < beta & n_cx_lte_v2(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx), na.rm=T)) {
				# pow[[i]][j] = 1
				# cat("TEST8\n")
			# }
			# else if (i > 1 && any(1-ppower >= beta & compare_n_cx_v(n1 = n[[i]], cx1 = cx_grid[[i]][,idx], n2 = pn, cx2 = pcx, compfunc = ">="), na.rm=T)) {
			# else if (i > 1 && any(1-ppower >= beta & n_cx_lte_v1(n2 = n[[i]], cx2 = cx_grid[[i]][,idx], n1 = pn, cx1 = pcx), na.rm=T)) {
				# pow[[i]][j] = 0
				# cat("TEST9\n")
			# }
			else {
				pow[[i]][j] = p_success(n = n[[i]], p = p1, cx = cx_grid[[i]][,idx])
				# cat("TEST10\n")
			}
		}
	}
	
	#select ones that pass beta
	# if(!any(1-power < beta)){return(null_df)}
	if(!any(1-unlist(pow) < alpha)){return(null_df)}
	
	# nodup_alpha_power_pass_idx = nodup_alpha_pass_idx[which(1-power < beta)]
	nodup_alpha_power_pass_idx = lapply(X = 1:length(n), 
		FUN = function(i, pow, beta, nodup_alpha_pass_idx){nodup_alpha_pass_idx[[i]][which(1-pow[[i]] < beta)]}, 
		pow = pow, beta = beta, nodup_alpha_pass_idx = nodup_alpha_pass_idx)
	
	# T1E_pass_power = T1E_pass[1-power < beta]
	T1E_pass_power = lapply(X = 1:length(n), 
		FUN = function(i, pow, beta, T1E_pass){T1E_pass[[i]][which(1-pow[[i]] < beta)]}, 
		pow = pow, beta = beta, T1E_pass = T1E_pass)

	# power_pass = power[1-power < beta]
	power_pass = lapply(X = pow, FUN = function(x,a){x[which(1-x<a)]}, a = beta)
	
	# exp_accrual = vapply(X = nodup_alpha_power_pass_idx, FUN = function(idx){EN(n = n, p = p0, cx = cx_grid[,idx])}, FUN.VALUE = 1.)
	exp_accrual = lapply(X = 1:length(n), function(i, nodup_alpha_power_pass_idx, n, p0, cx_grid){vapply(X = nodup_alpha_power_pass_idx[[i]], FUN = function(idx, n_i, p0, cx_grid_i){EN(n = n_i, p = p0, cx = cx_grid_i[,idx])}, FUN.VALUE = 1., n_i=n[[i]], p0=p0, cx_grid_i=cx_grid[[i]])}, nodup_alpha_power_pass_idx=nodup_alpha_power_pass_idx, n=n, p0=p0, cx_grid=cx_grid)
	
	# optimal
	exp_accrual_min_idx = lapply(X = exp_accrual, FUN = which.min)
	# exp_accrual_min = lapply(X = exp_accrual_min_idx, FUN = getElement, object = exp_accrual_min_idx)
	exp_accrual_min = lapply(X = 1:length(n), FUN = function(x, idx, val){val[[x]][idx[[x]]]}, idx = exp_accrual_min_idx, val = exp_accrual)
	
	# min(unlist(exp_accrual_min))
	optim_i = which.min(exp_accrual_min)
	optim_idx = exp_accrual_min_idx[[optim_i]]
	optim_n = n[[optim_i]]
	optim_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[optim_i]][optim_idx]
	optim_cx = cx_grid[[optim_i]][,optim_nodup_alpha_power_pass_idx]
	optim_t = t_grid[[optim_i]][optim_nodup_alpha_power_pass_idx,]
	optim_EN = exp_accrual[[optim_i]][optim_idx]
	optim_EN2 = EN(n = optim_n, p = p0, cx = optim_cx) #for unit testing:
	optim_EN == optim_EN2
	optim_T1E = p_success(n = optim_n, p = p0, cx = optim_cx)
	optim_power = p_success(n = optim_n, p = p1, cx = optim_cx)
	optim_pet = pet(n = optim_n, p = p0, cx = optim_cx)
	
	optim_stats = data.frame(design = "optimal", n = paste0(optim_n, collapse=","), optim_t, x_fail=paste0(optim_cx$x_fail, collapse=","), T1E = optim_T1E, power = optim_power, EN = optim_EN, pet = optim_pet)
	# return(optim_stats)
	
	#TODO
	# minimax
	i_pass = which(unlist(lapply(X = exp_accrual_min_idx, FUN = length)) > 0)
	n_nmax = unlist(lapply(X=n, FUN = max, na.rm=T))
	
	minimax_i = i_pass[which.min(n_nmax[i_pass])]
	minimax_idx = exp_accrual_min_idx[[minimax_i]]
	minimax_n = n[[minimax_i]]
	minimax_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[minimax_i]][minimax_idx]
	minimax_cx = cx_grid[[minimax_i]][,minimax_nodup_alpha_power_pass_idx]
	minimax_t = t_grid[[minimax_i]][minimax_nodup_alpha_power_pass_idx,]
	minimax_EN = exp_accrual[[minimax_i]][minimax_idx]
	minimax_EN2 = EN(n = minimax_n, p = p0, cx = minimax_cx) #for unit testing:
	minimax_EN == minimax_EN2
	minimax_T1E = p_success(n = minimax_n, p = p0, cx = minimax_cx)
	minimax_power = p_success(n = minimax_n, p = p1, cx = minimax_cx)
	minimax_pet = pet(n = minimax_n, p = p0, cx = minimax_cx)
	
	minimax_stats = data.frame(design = "minimax", n = paste0(minimax_n, collapse=","), minimax_t, x_fail=paste0(minimax_cx$x_fail, collapse=","), T1E = minimax_T1E, power = minimax_power, EN = minimax_EN, pet = minimax_pet)
	
	optim_minimax_stats = rbind(optim_stats, minimax_stats)
	# pow = lapply(X = n_nodup_alpha_pass, FUN = rep, x = NA)
	
	# n_nodup_alpha_pass = lapply(X = nodup_alpha_pass_idx, FUN = length)
	
		#check previous theta
		#check previous n/cx
		#calc power
	# power = rep(NA, n_nodup_alpha_pass)

	# calc EN
	# minimax
	# optimal
		
		# n_stats_l = lapply(X = n, FUN = feasible_n2, alpha = alpha, beta = beta, p0 = p0, p1 = p1, a0 = a0, b0 = b0)
		# n_stats = do.call(rbind, n_stats_l)
		# if(nrow(n_stats) > 0){
			# return(n_stats[which.min(n_stats$EN),])
		# }
	# }
	return(optim_minimax_stats)
}

search_n3 = function(k = NULL, min_n1 = 1, d = NULL, max_n = 100, alpha, beta, p0, p1, a0 = p0, b0 = 1-p0){
	#k = 2; min_n1 = 8; d = NULL; max_n = 11; alpha = 0.2; beta = 0.2; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0
	#k = NULL; min_n1 = 10; d = 5; max_n = 20; alpha = 0.2; beta = 0.2; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0
	#k = NULL; min_n1 = 10; d = NULL; max_n = 50; alpha = 0.1; beta = 0.1; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0 #example 1 from lee & liu
	#k = NULL; min_n1 = 10; d = NULL; max_n = 42; alpha = 0.1; beta = 0.1; p0 = 0.2; p1 = 0.4; a0 = p0; b0 = 1-p0 #example 1 from lee & liu
	#k = 2; min_n1 = 20; d = NULL; max_n = 60; alpha = 0.05; beta = 0.2; p0 = 0.05; p1 = 0.15; a0 = p0; b0 = 1-p0 # Anastasia Ivanova UNC example
	#k = 2; min_n1 = 19; d = NULL; max_n = 33; alpha = 0.15; beta = 0.15; p0 = 0.3; p1 = 0.5; a0 = p0; b0 = 1-p0 #bryant & day example
	#k = NULL; min_n1 = 19; d = 19; max_n = 33; alpha = 0.15; beta = 0.15; p0 = 0.3; p1 = 0.5; a0 = p0; b0 = 1-p0 #bryant & day example (alternate)
	#k = NULL; min_n1 = 19; d = 19; max_n = 33; alpha = 0.15/0.85; beta = 0.15; p0 = 0.3; p1 = 0.5; a0 = p0; b0 = 1-p0 #bryant & day example (alternate2)
	stopifnot((is.null(k) || is.na(k)) || (is.null(d) || is.na(d)))
	null_df = data.frame()
	nk = max(min_n1+1, 2, min_n1+k-1, na.rm=T):max_n
	n = do.call(c,lapply(X = nk, FUN = calc_n, k = k, min_n1 = min_n1, d = d))
	
	theta_x_l = lapply(X = n, FUN = calc_theta_grid, p0 = p0, a0 = a0, b0 = b0)
	t_grid = lapply(X = theta_x_l, FUN = function(x){rbind(data.frame(theta_T=unique(x$theta_T),theta_L=0), unique(x[,c("theta_T", "theta_L")]))})
	t_grid = lapply(X = t_grid, FUN = function(x){x[x$theta_L < 1,]})
	# cx_grid = lapply(X = 1:length(n), FUN = function(i, t_grid, n, theta_x_l){
		# mapply(FUN = calc_cutoff_grid, t_grid[[i]]$theta_T, t_grid[[i]]$theta_L, 
			# MoreArgs = list(n = n[[i]], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, 
			# x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp))}, 
		# t_grid=t_grid, n=n, theta_x_l=theta_x_l)
	cx_grid = lapply(X = 1:length(n), FUN = function(i, t_grid, n, theta_x_l){
		mapply(FUN = calc_cutoff_grid_NA, t_grid[[i]]$theta_T, t_grid[[i]]$theta_L, 
			MoreArgs = list(n = n[[i]], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, 
			x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp))}, 
		t_grid=t_grid, n=n, theta_x_l=theta_x_l)

	# cx_dup = lapply(X = cx_grid, FUN = function(cx){duplicated(t(cx))})
	cx_dup = lapply(X = cx_grid, FUN = function(cx){rep(F, ncol(cx))})
	nodup_idx = lapply(X = cx_dup, FUN = function(x){which(!x)})
	n_nodup = lapply(X = nodup_idx, FUN = length)

	T1E = lapply(X = n_nodup, FUN = rep, x = NA)
	T1E_lt_alpha = T1E
	for(i in 1:length(n)){
		# if(i > 1){
			# pn = do.call(c,lapply(X = 1:(i-1), 
				# FUN = function(i, n_nodup, n){rep(x = list(n[[i]]), times = n_nodup[[i]])}, 
				# n_nodup=n_nodup, n=n))
			# pcx = do.call(c,lapply(X = 1:(i-1), FUN=function(i, cx){lapply(X=seq_len(ncol(cx[[i]])), FUN=function(j) cx[[i]][,j])}, cx = cx_grid))
			# pT1E = unlist(lapply(X = 1:(i-1), FUN = getElement, object = T1E))
		# }
		for(j in 1:n_nodup[[i]]){
			idx = nodup_idx[[i]][j]
			# T1E_lt_alpha = T1E[[i]] < alpha
			# if(any(T1E[[i]] < alpha & t_grid[[i]]$theta_T[nodup_idx[[i]]] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
			if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
			# if(any(t_grid[[i]]$theta_L[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])][t_grid[[i]]$theta_T[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_T[idx]] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 0
				# T1E_lt_alpha[[i]][j] = T
				# cat("TEST2\n")
			} 
			# else if(any(T1E[[i]] >= alpha & t_grid[[i]]$theta_T[nodup_idx[[i]]] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
			else if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
			# else if(any(t_grid[[i]]$theta_L[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])][t_grid[[i]]$theta_T[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_T[idx]] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 1
				# T1E_lt_alpha[[i]][j] = F
				# cat("TEST1\n")
			} 
			else {
				cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
				# cx_grid[[i]][,idx] = calc_cutoffs(theta_L = t_grid[[i]]$theta_L[idx], theta_T = t_grid[[i]]$theta_T[idx], p0=0.2, p1=0.4, n = n[[i]])
				# T1E[[i]][j] = p_success(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
				T1E[[i]][j] = p_success_mat(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
				T1E_lt_alpha[[i]][j] = T1E[[i]][j] < alpha
				# cat("TEST5\n")
			}
		}
	}

	if(!any(unlist(T1E) < alpha)){return(null_df)}
	nodup_alpha_pass_idx = lapply(X = 1:length(n), 
		FUN = function(i, T1E, alpha, nodup_idx){nodup_idx[[i]][which(T1E[[i]] < alpha)]}, 
		T1E = T1E, alpha = alpha, nodup_idx = nodup_idx)
	T1E_pass = lapply(X = T1E, FUN = function(x,a){x[which(x<a)]}, a = alpha)
	n_nodup_alpha_pass = lapply(X = nodup_alpha_pass_idx, FUN = length)
	
	pow = lapply(X = n_nodup_alpha_pass, FUN = rep, x = NA)
	T2E_gte_beta = pow
	for(i in 1:length(n)){
		# cat(i, "\n")
		# if(i > 1){
			# pn = do.call(c,lapply(X = 1:(i-1), 
				# FUN = function(i, n_nodup_alpha_pass, n){rep(x = list(n[[i]]), times = n_nodup_alpha_pass[[i]])}, 
				# n_nodup_alpha_pass=n_nodup_alpha_pass, n=n))
			# pcx = do.call(c,lapply(X = 1:(i-1), FUN=function(i, cx){lapply(X=nodup_alpha_pass_idx[[i]], FUN=function(j) cx[[i]][,j])}, cx = cx_grid))
			# ppower = unlist(lapply(X = 1:(i-1), FUN = getElement, object = pow))
		# }
		for(j in 1:n_nodup_alpha_pass[[i]]){
			idx = nodup_alpha_pass_idx[[i]][j]
			# if(any(1-pow[[i]] >= beta & t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
			if(any(t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]][which(T2E_gte_beta[[i]])] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]][which(T2E_gte_beta[[i]])] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				pow[[i]][j] = 0
				# cat("TEST7\n")
			} 
			# else if(any(1-pow[[i]] < beta & t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
			else if(any(t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]][which(!T2E_gte_beta[[i]])] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]][which(!T2E_gte_beta[[i]])] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				pow[[i]][j] = 1
				# cat("TEST6\n")
			} 
			else {
				cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
				# pow[[i]][j] = p_success(n = n[[i]], p = p1, cx = cx_grid[[i]][,idx])
				pow[[i]][j] = p_success_mat(n = n[[i]], p = p1, cx = cx_grid[[i]][,idx])
				T2E_gte_beta[[i]][j] = 1-pow[[i]][j] >= beta
				# cat("TEST10\n")
			}
		}
	}
	
	#select ones that pass beta
	# if(!any(1-unlist(pow) < alpha)){return(null_df)} ####!!!!this looks like an error. investigate.!!!!####
	if(!any(1-unlist(pow) >= beta)){return(null_df)}
	
	nodup_alpha_power_pass_idx = lapply(X = 1:length(n), 
		FUN = function(i, pow, beta, nodup_alpha_pass_idx){nodup_alpha_pass_idx[[i]][which(1-pow[[i]] < beta)]}, 
		pow = pow, beta = beta, nodup_alpha_pass_idx = nodup_alpha_pass_idx)
	
	T1E_pass_power = lapply(X = 1:length(n), 
		FUN = function(i, pow, beta, T1E_pass){T1E_pass[[i]][which(1-pow[[i]] < beta)]}, 
		pow = pow, beta = beta, T1E_pass = T1E_pass)

	power_pass = lapply(X = pow, FUN = function(x,a){x[which(1-x<a)]}, a = beta)
	
	exp_accrual = lapply(X = 1:length(n), function(i, nodup_alpha_power_pass_idx, n, p0, cx_grid){vapply(X = nodup_alpha_power_pass_idx[[i]], FUN = function(idx, n_i, p0, cx_grid_i){EN(n = n_i, p = p0, cx = cx_grid_i[,idx])}, FUN.VALUE = 1., n_i=n[[i]], p0=p0, cx_grid_i=cx_grid[[i]])}, nodup_alpha_power_pass_idx=nodup_alpha_power_pass_idx, n=n, p0=p0, cx_grid=cx_grid)
	
	exp_accrual_min_idx = lapply(X = exp_accrual, FUN = which.min)
	exp_accrual_min = lapply(X = 1:length(n), FUN = function(x, idx, val){val[[x]][idx[[x]]]}, idx = exp_accrual_min_idx, val = exp_accrual)
	
	optim_i = which.min(exp_accrual_min)
	optim_idx = exp_accrual_min_idx[[optim_i]]
	optim_n = n[[optim_i]]
	optim_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[optim_i]][optim_idx]
	optim_cx = cx_grid[[optim_i]][,optim_nodup_alpha_power_pass_idx]
	optim_t = t_grid[[optim_i]][optim_nodup_alpha_power_pass_idx,]
	optim_EN = exp_accrual[[optim_i]][optim_idx]
	optim_EN2 = EN(n = optim_n, p = p0, cx = optim_cx) #for unit testing:
	optim_EN == optim_EN2
	# optim_T1E = p_success(n = optim_n, p = p0, cx = optim_cx)
	optim_T1E = p_success_mat(n = optim_n, p = p0, cx = optim_cx)
	# optim_power = p_success(n = optim_n, p = p1, cx = optim_cx)
	optim_power = p_success_mat(n = optim_n, p = p1, cx = optim_cx)
	optim_pet = pet(n = optim_n, p = p0, cx = optim_cx)
	
	optim_n_cx = rm_stages(optim_n, optim_cx)
	
	# optim_stats = data.frame(design = "optimal", n = paste0(optim_n, collapse=","), optim_t, x_fail=paste0(optim_cx$x_fail, collapse=","), T1E = optim_T1E, power = optim_power, EN = optim_EN, pet = optim_pet, stringsAsFactors=F)
	optim_stats = data.frame(design = "optimal", n = paste0(optim_n_cx$n, collapse=","), optim_t, x_fail=paste0(optim_n_cx$cx$x_fail, collapse=","), T1E = optim_T1E, power = optim_power, EN = optim_EN, pet = optim_pet, stringsAsFactors=F)

	# minimax
	i_pass = which(unlist(lapply(X = exp_accrual_min_idx, FUN = length)) > 0)
	n_nmax = unlist(lapply(X=n, FUN = max, na.rm=T))
	
	minimax_i = i_pass[which.min(n_nmax[i_pass])]
	minimax_idx = exp_accrual_min_idx[[minimax_i]]
	minimax_n = n[[minimax_i]]
	minimax_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[minimax_i]][minimax_idx]
	minimax_cx = cx_grid[[minimax_i]][,minimax_nodup_alpha_power_pass_idx]
	minimax_t = t_grid[[minimax_i]][minimax_nodup_alpha_power_pass_idx,]
	minimax_EN = exp_accrual[[minimax_i]][minimax_idx]
	minimax_EN2 = EN(n = minimax_n, p = p0, cx = minimax_cx) #for unit testing:
	minimax_EN == minimax_EN2
	# minimax_T1E = p_success(n = minimax_n, p = p0, cx = minimax_cx)
	minimax_T1E = p_success_mat(n = minimax_n, p = p0, cx = minimax_cx)
	# minimax_power = p_success(n = minimax_n, p = p1, cx = minimax_cx)
	minimax_power = p_success_mat(n = minimax_n, p = p1, cx = minimax_cx)
	minimax_pet = pet(n = minimax_n, p = p0, cx = minimax_cx)
	
	minimax_n_cx = rm_stages(minimax_n, minimax_cx)
	
	# minimax_stats = data.frame(design = "minimax", n = paste0(minimax_n, collapse=","), minimax_t, x_fail=paste0(minimax_cx$x_fail, collapse=","), T1E = minimax_T1E, power = minimax_power, EN = minimax_EN, pet = minimax_pet, stringsAsFactors=F)
	minimax_stats = data.frame(design = "minimax", n = paste0(minimax_n_cx$n, collapse=","), minimax_t, x_fail=paste0(minimax_n_cx$cx$x_fail, collapse=","), T1E = minimax_T1E, power = minimax_power, EN = minimax_EN, pet = minimax_pet, stringsAsFactors=F)
	
	optim_minimax_stats = rbind(optim_stats, minimax_stats)
	
	exp_accrual_min_no_mm_op = exp_accrual_min
	exp_accrual_min_no_mm_op[[optim_i]] = numeric(0)
	exp_accrual_min_no_mm_op[[minimax_i]] = numeric(0)
	alt1_i = which.min(exp_accrual_min_no_mm_op)
	# alt1_i = integer(0)
	
	if(length(alt1_i)>0){
		alt1_idx = exp_accrual_min_idx[[alt1_i]]
		alt1_n = n[[alt1_i]]
		alt1_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[alt1_i]][alt1_idx]
		alt1_cx = cx_grid[[alt1_i]][,alt1_nodup_alpha_power_pass_idx]
		alt1_t = t_grid[[alt1_i]][alt1_nodup_alpha_power_pass_idx,]
		alt1_EN = exp_accrual[[alt1_i]][alt1_idx]
		alt1_EN2 = EN(n = alt1_n, p = p0, cx = alt1_cx) #for unit testing:
		alt1_EN == alt1_EN2
		alt1_T1E = p_success_mat(n = alt1_n, p = p0, cx = alt1_cx)
		alt1_power = p_success_mat(n = alt1_n, p = p1, cx = alt1_cx)
		alt1_pet = pet(n = alt1_n, p = p0, cx = alt1_cx)
		alt1_n_cx = rm_stages(alt1_n, alt1_cx)
		# alt1_stats = data.frame(design = "alt1", n = paste0(alt1_n, collapse=","), alt1_t, x_fail=paste0(alt1_cx$x_fail, collapse=","), T1E = alt1_T1E, power = alt1_power, EN = alt1_EN, pet = alt1_pet, stringsAsFactors=F)
		alt1_stats = data.frame(design = "alt1", n = paste0(alt1_n_cx$n, collapse=","), alt1_t, x_fail=paste0(alt1_n_cx$cx$x_fail, collapse=","), T1E = alt1_T1E, power = alt1_power, EN = alt1_EN, pet = alt1_pet, stringsAsFactors=F)
		optim_minimax_stats = rbind(optim_minimax_stats, alt1_stats)

		exp_accrual_min_no_mm_op[[alt1_i]] = numeric(0)
		alt2_i = which.min(exp_accrual_min_no_mm_op)
		# alt2_i = integer(0)
	# }
	
		if(length(alt2_i)>0){
			alt2_idx = exp_accrual_min_idx[[alt2_i]]
			alt2_n = n[[alt2_i]]
			alt2_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[alt2_i]][alt2_idx]
			alt2_cx = cx_grid[[alt2_i]][,alt2_nodup_alpha_power_pass_idx]
			alt2_t = t_grid[[alt2_i]][alt2_nodup_alpha_power_pass_idx,]
			alt2_EN = exp_accrual[[alt2_i]][alt2_idx]
			alt2_EN2 = EN(n = alt2_n, p = p0, cx = alt2_cx) #for unit testing:
			alt2_EN == alt2_EN2
			alt2_T1E = p_success_mat(n = alt2_n, p = p0, cx = alt2_cx)
			alt2_power = p_success_mat(n = alt2_n, p = p1, cx = alt2_cx)
			alt2_pet = pet(n = alt2_n, p = p0, cx = alt2_cx)
			alt2_n_cx = rm_stages(alt2_n, alt2_cx)
			# alt2_stats = data.frame(design = "alt2", n = paste0(alt2_n, collapse=","), alt2_t, x_fail=paste0(alt2_cx$x_fail, collapse=","), T1E = alt2_T1E, power = alt2_power, EN = alt2_EN, pet = alt2_pet, stringsAsFactors=F)
			alt2_stats = data.frame(design = "alt2", n = paste0(alt2_n_cx$n, collapse=","), alt2_t, x_fail=paste0(alt2_n_cx$cx$x_fail, collapse=","), T1E = alt2_T1E, power = alt2_power, EN = alt2_EN, pet = alt2_pet, stringsAsFactors=F)
			optim_minimax_stats = rbind(optim_minimax_stats, alt2_stats)
		}
	}
	# optim_minimax_stats
	return(optim_minimax_stats)
}

search_n_RT = function(k = NULL, min_n1 = 1, d = NULL, max_n = 100, alphaR, alphaT, beta, pR0, pR1, pT0, pT1, aR0 = pR0, bR0 = 1-pR0, aT0 = pT0, bT0 = 1-pT0){
	#k = NULL; min_n1 = 19; d = 19; max_n = 33; alphaR = 0.15; alphaT = 0.15; beta = 0.15; pR0 = 0.3; pR1 = 0.5; pT0 = 0.6; pT1 = 0.8; aR0 = pR0; bR0 = 1-pR0; aT0 = pT0; bT0 = 1-pT0 #bryant & day example (alternate)
	# k = NULL; min_n1 = 1; d = NULL; max_n = 33; alphaR = 0.15; alphaT = 0.15; beta = 0.15; pR0 = 0.3; pR1 = 0.5; pT0 = 0.6; pT1 = 0.8; aR0 = pR0; bR0 = 1-pR0; aT0 = pT0; bT0 = 1-pT0 #bryant & day example (alternate)
	#k = NULL; min_n1 = 10; d = 5; max_n = 20; alphaR = 0.2; beta = 0.2; pR0 = 0.2; pR1 = 0.4; aR0 = pR0; bR0 = 1-pR0 #short example
	stopifnot((is.null(k) || is.na(k)) || (is.null(d) || is.na(d)))
	null_df = data.frame()
	nk = max(min_n1+1, 2, min_n1+k-1, na.rm=T):max_n
	n = do.call(c,lapply(X = nk, FUN = calc_n, k = k, min_n1 = min_n1, d = d))
	
	ssR = def_search_space(n, p0 = pR0, a0 = aR0, b0 = bR0)
	ssT = def_search_space(n, p0 = pT0, a0 = aT0, b0 = bT0)

	#put this in another function
	#constraints:
	##alphaR/power => alphaR0
	# t_grid = ssR$t_grid
	# cx_grid = ssR$cx_grid
	# nodup_idx = ssR$nodup_idx
	# n_nodup = ssR$n_nodup
	# theta_x_l = ssR$theta_x_l
	# p0 = pR0
	# alpha = alphaR/(1-beta)

	# T1E = lapply(X = n_nodup, FUN = rep, x = NA)
	# T1E_lt_alpha = T1E
	# for(i in 1:length(n)){
		# for(j in 1:n_nodup[[i]]){
			# idx = nodup_idx[[i]][j]
			# if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				# T1E[[i]][j] = 0
			# } 
			# else if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				# T1E[[i]][j] = 1
			# } 
			# else {
				# cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
				# T1E[[i]][j] = p_success_mat(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
				# T1E_lt_alpha[[i]][j] = T1E[[i]][j] < alpha
			# }
		# }
	# }
	
	# T1E_R0 = calc_T12E_lt_ab(n_nodup = ssR$n_nodup, n = n, nodup_idx = ssR$nodup_idx, t_grid = ssR$t_grid, 
		# theta_x_l = ssR$theta_x_l, cx_grid = ssR$cx_grid, cx_grid_nm = "ssR$cx_grid", p0 = pR0, 
		# alpha = alphaR/(1-beta), env = environment())
	T1E_R0 = calc_T12E_lt_ab(n_nodup = ssR$n_nodup, n = n, nodup_idx = ssR$nodup_idx, t_grid = ssR$t_grid, 
		theta_x_l = ssR$theta_x_l, cx_grid = ssR$cx_grid, p0 = pR0, alpha = alphaR/(1-beta), passfun = "<")

	#add T1E for nontoxicity endpoint
	T1E_T0 = calc_T12E_lt_ab(n_nodup = ssT$n_nodup, n = n, nodup_idx = ssT$nodup_idx, t_grid = ssT$t_grid, 
		theta_x_l = ssT$theta_x_l, cx_grid = ssT$cx_grid, p0 = pT0, alpha = alphaT/(1-beta), passfun = "<")
	
	#check if alphaR and alphaT are satisfied
	
	# if(!any(unlist(T1E) < alpha)){return(null_df)}
	if(!any(unlist(T1E_R0$T1E) < alphaR/(1-beta))){return(null_df)}
	if(!any(unlist(T1E_T0$T1E) < alphaT/(1-beta))){return(null_df)}
	
	# nodup_alpha_pass_idx = lapply(X = 1:length(n), 
		# FUN = function(i, T1E, alpha, nodup_idx){nodup_idx[[i]][which(T1E[[i]] < alpha)]}, 
		# T1E = T1E, alpha = alpha, nodup_idx = nodup_idx)
	# T1E_pass = lapply(X = T1E, FUN = function(x,a){x[which(x<a)]}, a = alpha)
	# n_nodup_alpha_pass = lapply(X = nodup_alpha_pass_idx, FUN = length)
	
	#put in another function
	#constraints:
	##power <= alphaR1; power * alphaR0/alphaR <= alphaR1
	
	#for testing
	# theta_x_l = ssR$theta_x_l
	# t_grid = ssR$t_grid
	# p1 = pR1
	# T1E = T1E_R0$T1E
	# cx_grid = T1E_R0$cx_grid
	# nodup_alpha_pass_idx = T1E_R0$nodup_alpha_pass_idx
	# T1E_pass = T1E_R0$T1E_pass
	# n_nodup_alpha_pass = T1E_R0$n_nodup_alpha_pass

	
	# pow = lapply(X = n_nodup_alpha_pass, FUN = rep, x = NA)
	# T2E_gte_beta = pow
	# for(i in 1:length(n)){
		# for(j in 1:n_nodup_alpha_pass[[i]]){
			# idx = nodup_alpha_pass_idx[[i]][j]
			# if(any(t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]][which(T2E_gte_beta[[i]])] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]][which(T2E_gte_beta[[i]])] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				# pow[[i]][j] = 0
			# } 
			# else if(any(t_grid[[i]]$theta_T[nodup_alpha_pass_idx[[i]]][which(!T2E_gte_beta[[i]])] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_alpha_pass_idx[[i]]][which(!T2E_gte_beta[[i]])] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				# pow[[i]][j] = 1
			# } 
			# else {
				# cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
				# pow[[i]][j] = p_success_mat(n = n[[i]], p = p1, cx = cx_grid[[i]][,idx])
				# T2E_gte_beta[[i]][j] = 1-pow[[i]][j] >= beta
			# }
		# }
	# }
	
	#add power for nontoxicity endpoint
	
	
	
	#select ones that pass beta
	# if(!any(1-unlist(pow) < alpha)){return(null_df)}  ####!!!!this looks like an error. investigate.!!!!####
	
	# nodup_alpha_power_pass_idx = lapply(X = 1:length(n), 
		# FUN = function(i, pow, beta, nodup_alpha_pass_idx){nodup_alpha_pass_idx[[i]][which(1-pow[[i]] < beta)]}, 
		# pow = pow, beta = beta, nodup_alpha_pass_idx = nodup_alpha_pass_idx)
	
	# T1E_pass_power = lapply(X = 1:length(n), 
		# FUN = function(i, pow, beta, T1E_pass){T1E_pass[[i]][which(1-pow[[i]] < beta)]}, 
		# pow = pow, beta = beta, T1E_pass = T1E_pass)

	# power_pass = lapply(X = pow, FUN = function(x,a){x[which(1-x<a)]}, a = beta)
	
	#modular code
	T2E_R1 = calc_T12E_lt_ab(n_nodup = T1E_R0$n_nodup_alpha_pass, n = n, nodup_idx = T1E_R0$nodup_alpha_pass_idx, t_grid = ssR$t_grid, 
		theta_x_l = ssR$theta_x_l, cx_grid = T1E_R0$cx_grid, p0 = pR1, alpha = (1-beta), passfun = ">")

	T2E_T1 = calc_T12E_lt_ab(n_nodup = T1E_T0$n_nodup_alpha_pass, n = n, nodup_idx = T1E_T0$nodup_alpha_pass_idx, t_grid = ssT$t_grid, 
		theta_x_l = ssT$theta_x_l, cx_grid = T1E_T0$cx_grid, p0 = pT1, alpha = (1-beta), passfun = ">")
	
	if(!any(unlist(T2E_R1$T1E) > 1-beta)){return(null_df)}
	if(!any(unlist(T2E_T1$T1E) > 1-beta)){return(null_df)}

	T1E_pass_power_R = mapply(FUN = "[", T1E_R0$T1E, T2E_R1$nodup_alpha_pass_idx)
	T1E_pass_power_T = mapply(FUN = "[", T1E_T0$T1E, T2E_T1$nodup_alpha_pass_idx)
	
	#recalc marginal T1E/power for passing marginal designs
	#response
	power_need_recalc_R = lapply(X = T2E_R1$T1E_pass, FUN = function(x) {which(x == 0 | x == 1)})
	# lapply(X = T2E_R1$T1E_pass, FUN = function(x, idx) {idx[which(x == 0 | x == 1)]}, idx = T2E_R1$nodup_alpha_pass_idx)
	
	T1E_need_recalc_R = lapply(X = T1E_pass_power_R, FUN = function(x) {which(x == 0 | x == 1)})
	# T1E_need_recalc_idx = lapply(X = T1E_pass_power_R, FUN = function(x, idx) {idx[which(x == 0 | x == 1)]}, idx = T2E_R1$nodup_alpha_pass_idx)
	
	#set and save cx for each
	for(i in 1:length(power_need_recalc_R)){
		if(length(power_need_recalc_R[[i]])>0){
			for(j in power_need_recalc_R[[i]]){
				# idx = T1E_need_recalc_idx[[i]][j]
				idx = T2E_R1$nodup_alpha_pass_idx[[i]][j]
				# cat(i, j, idx, "\n")
				# cat(T1E_R0$cx_grid[[i]][,idx]$x_fail, "\n")
				# cat(T2E_R1$cx_grid[[i]][,idx]$x_fail, "\n")
				if(all(is.na(T2E_R1$cx_grid[[i]][,idx]$x_fail))){
					T2E_R1$cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], 
						theta_T = ssR$t_grid[[i]]$theta_T[idx], 
						theta_L = ssR$t_grid[[i]]$theta_L[idx], 
						theta_T_exp = ssR$theta_x_l[[i]]$theta_T, 
						theta_L_exp = ssR$theta_x_l[[i]]$theta_L, 
						x_exp = ssR$theta_x_l[[i]]$x_exp, 
						l_exp = ssR$theta_x_l[[i]]$l_exp)
				}
				# cat(T2E_R1$cx_grid[[i]][,idx]$x_fail, "\n")
				T2E_R1$T1E_pass[[i]][j] = p_success_mat(n = n[[i]], p = pR1, cx = T2E_R1$cx_grid[[i]][,idx])
				# cat(T2E_R1$T1E_pass[[i]][j], "\n\n")
			}
		}
	}
	# T2E_R1$T1E_pass
	
	for(i in 1:length(T1E_need_recalc_R)){
		if(length(T1E_need_recalc_R[[i]])>0){
			for(j in T1E_need_recalc_R[[i]]){
				idx = T2E_R1$nodup_alpha_pass_idx[[i]][j]
				if(all(is.na(T2E_R1$cx_grid[[i]][,idx]$x_fail))){
					T2E_R1$cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], 
						theta_T = ssR$t_grid[[i]]$theta_T[idx], 
						theta_L = ssR$t_grid[[i]]$theta_L[idx], 
						theta_T_exp = ssR$theta_x_l[[i]]$theta_T, 
						theta_L_exp = ssR$theta_x_l[[i]]$theta_L, 
						x_exp = ssR$theta_x_l[[i]]$x_exp, 
						l_exp = ssR$theta_x_l[[i]]$l_exp)
				}
				T1E_pass_power_R[[i]][j] = p_success_mat(n = n[[i]], p = pR0, cx = T2E_R1$cx_grid[[i]][,idx])
			}
		}
	}
	# T1E_pass_power_R

	#nontoxicity
	power_need_recalc_T = lapply(X = T2E_T1$T1E_pass, FUN = function(x) {which(x == 0 | x == 1)})
	T1E_need_recalc_T = lapply(X = T1E_pass_power_T, FUN = function(x) {which(x == 0 | x == 1)})

	for(i in 1:length(power_need_recalc_T)){
		if(length(power_need_recalc_T[[i]])>0){
			for(j in power_need_recalc_T[[i]]){
				# idx = T1E_need_recalc_idx[[i]][j]
				idx = T2E_T1$nodup_alpha_pass_idx[[i]][j]
				# cat(i, j, idx, "\n")
				# cat(T1E_T0$cx_grid[[i]][,idx]$x_fail, "\n")
				# cat(T2E_T1$cx_grid[[i]][,idx]$x_fail, "\n")
				if(all(is.na(T2E_T1$cx_grid[[i]][,idx]$x_fail))){
					T2E_T1$cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], 
						theta_T = ssR$t_grid[[i]]$theta_T[idx], 
						theta_L = ssR$t_grid[[i]]$theta_L[idx], 
						theta_T_exp = ssR$theta_x_l[[i]]$theta_T, 
						theta_L_exp = ssR$theta_x_l[[i]]$theta_L, 
						x_exp = ssR$theta_x_l[[i]]$x_exp, 
						l_exp = ssR$theta_x_l[[i]]$l_exp)
				}
				# cat(T2E_T1$cx_grid[[i]][,idx]$x_fail, "\n")
				T2E_T1$T1E_pass[[i]][j] = p_success_mat(n = n[[i]], p = pT1, cx = T2E_T1$cx_grid[[i]][,idx])
				# cat(T2E_T1$T1E_pass[[i]][j], "\n\n")
			}
		}
	}
	# T2E_T1$T1E_pass
	
	for(i in 1:length(T1E_need_recalc_T)){
		if(length(T1E_need_recalc_T[[i]])>0){
			for(j in T1E_need_recalc_T[[i]]){
				idx = T2E_T1$nodup_alpha_pass_idx[[i]][j]
				if(all(is.na(T2E_T1$cx_grid[[i]][,idx]$x_fail))){
					T2E_T1$cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], 
						theta_T = ssR$t_grid[[i]]$theta_T[idx], 
						theta_L = ssR$t_grid[[i]]$theta_L[idx], 
						theta_T_exp = ssR$theta_x_l[[i]]$theta_T, 
						theta_L_exp = ssR$theta_x_l[[i]]$theta_L, 
						x_exp = ssR$theta_x_l[[i]]$x_exp, 
						l_exp = ssR$theta_x_l[[i]]$l_exp)
				}
				T1E_pass_power_T[[i]][j] = p_success_mat(n = n[[i]], p = pT0, cx = T2E_T1$cx_grid[[i]][,idx])
			}
		}
	}
	# T1E_pass_power_T

	#apply (1-beta) * T1E / alphaR <= pow
	stopifnot(length(T1E_pass_power_R) == length(T2E_R1$T1E_pass))
	pass_R = lapply(X = 1:length(T1E_pass_power_R), FUN = function(i, beta, alpha, T1E, pow){which((1-beta) * T1E[[i]] / alpha <= pow[[i]])}, beta=beta, alpha = alphaR, T1E = T1E_pass_power_R, pow = T2E_R1$T1E_pass)
	
	# (1-beta) * T1E_pass_power_R / alphaR <= T2E_R1$T1E_pass

	#apply (1-beta) * T1E / alphaT <= pow
	stopifnot(length(T1E_pass_power_T) == length(T2E_T1$T1E_pass))
	pass_T = lapply(X = 1:length(T1E_pass_power_T), FUN = function(i, beta, alpha, T1E, pow){which((1-beta) * T1E[[i]] / alpha <= pow[[i]])}, beta=beta, alpha = alphaT, T1E = T1E_pass_power_T, pow = T2E_T1$T1E_pass)

	# pass_marg_grid = expand.grid(pass_Ri = pass_R[[10]], pass_Ti = pass_T[[10]])
	pass_marg_grid = lapply(X = 1:length(n), FUN = function(i, pass_R, pass_T){expand.grid(pass_Ri = pass_R[[i]], pass_Ti = pass_T[[i]])}, pass_R = pass_R, pass_T = pass_T)
	
	# T1E_R0T1 = T1E_pass_power_R[[10]][pass_marg_grid$pass_Ri] * T2E_T1$T1E_pass[[10]][pass_marg_grid$pass_Ti]
	T1E_R0T1 = lapply(X = 1:length(n), FUN = function(i, grid, probR, probT){probR[[i]][grid[[i]]$pass_Ri] * probT[[i]][grid[[i]]$pass_Ti]}, grid = pass_marg_grid, probR = T1E_pass_power_R, probT = T2E_T1$T1E_pass)
	# T1E_R1T0 = T2E_R1$T1E_pass[[10]][pass_marg_grid$pass_Ri] * T1E_pass_power_T[[10]][pass_marg_grid$pass_Ti]
	T1E_R1T0 = lapply(X = 1:length(n), FUN = function(i, grid, probR, probT){probR[[i]][grid[[i]]$pass_Ri] * probT[[i]][grid[[i]]$pass_Ti]}, grid = pass_marg_grid, probR = T2E_R1$T1E_pass, probT = T1E_pass_power_T)
	# pow_RT = T2E_R1$T1E_pass[[10]][pass_marg_grid$pass_Ri] * T2E_T1$T1E_pass[[10]][pass_marg_grid$pass_Ti]
	pow_RT = lapply(X = 1:length(n), FUN = function(i, grid, probR, probT){probR[[i]][grid[[i]]$pass_Ri] * probT[[i]][grid[[i]]$pass_Ti]}, grid = pass_marg_grid, probR = T2E_R1$T1E_pass, probT = T2E_T1$T1E_pass)
	
	
	#check for valid combos 01, 10, 11
	# pass_RT = T1E_R0T1 <= alphaR & T1E_R1T0 <= alphaT & pow_RT >= (1-beta)
	pass_RT = lapply(X = 1:length(n), FUN = function(i, T1E_R0T1, alphaR, T1E_R1T0, alphaT, pow_RT, beta){
		which(T1E_R0T1[[i]] <= alphaR & T1E_R1T0[[i]] <= alphaT & pow_RT[[i]] >= (1-beta))}, 
		T1E_R0T1 = T1E_R0T1, alphaR = alphaR, T1E_R1T0 = T1E_R1T0, alphaT = alphaT, pow_RT = pow_RT, beta = beta)
	
	#will need to check for the same n
	# alphaR0 * alphaT1 <= alphaR
	# lapply(X = 1:length(), FUN = function(i, alpha, T1E, pow){which(T1E[[i]] * pow[[i]] <= alpha)}, alpha = alphaR, T1E = T1E_pass_power_R, pow = T2E_T1$T1E_pass)
	# alphaR1 * alphaT0 <= alphaT
	# alphaR1 * alphaT1 >= (1-beta)
	
	# T1E_R0T1[[14]][41]
	pass_RT_T1E_R0T1 = lapply(X = 1:length(n), FUN = function(i, T1ET2E, pass_RT){T1ET2E[[i]][pass_RT[[i]]]}, T1ET2E = T1E_R0T1, pass_RT = pass_RT)
	
	# T1E_R1T0[[14]][41]
	pass_RT_T1E_R1T0 = lapply(X = 1:length(n), FUN = function(i, T1ET2E, pass_RT){T1ET2E[[i]][pass_RT[[i]]]}, T1ET2E = T1E_R1T0, pass_RT = pass_RT)
	
	# pow_RT[[14]][41]
	pass_RT_pow = lapply(X = 1:length(n), FUN = function(i, T1ET2E, pass_RT){T1ET2E[[i]][pass_RT[[i]]]}, T1ET2E = pow_RT, pass_RT = pass_RT)

	# pass_marg_grid[[14]][41,]
	# n[[14]]
	# pass_RT_Ri = pass_marg_grid[[14]][41,]$pass_Ri
	pass_RT_Ri = lapply(X = 1:length(n), FUN = function(i, pass_marg_grid, pass_RT){pass_marg_grid[[i]][pass_RT[[i]],]$pass_Ri}, pass_marg_grid = pass_marg_grid, pass_RT = pass_RT)
	# pass_RT_Ti = pass_marg_grid[[14]][41,]$pass_Ti
	pass_RT_Ti = lapply(X = 1:length(n), FUN = function(i, pass_marg_grid, pass_RT){pass_marg_grid[[i]][pass_RT[[i]],]$pass_Ti}, pass_marg_grid = pass_marg_grid, pass_RT = pass_RT)
	
	# pass_RT_Ridx = T2E_R1$nodup_alpha_pass_idx[[14]][pass_RT_Ri]
	pass_RT_Ridx = lapply(X = 1:length(n), FUN = function(i, nodup_alpha_pass_idx, pass_RT_i){nodup_alpha_pass_idx[[i]][pass_RT_i[[i]]]}, nodup_alpha_pass_idx = T2E_R1$nodup_alpha_pass_idx, pass_RT_i = pass_RT_Ri)
	# pass_RT_Tidx = T2E_T1$nodup_alpha_pass_idx[[14]][pass_RT_Ti]
	pass_RT_Tidx = lapply(X = 1:length(n), FUN = function(i, nodup_alpha_pass_idx, pass_RT_i){nodup_alpha_pass_idx[[i]][pass_RT_i[[i]]]}, nodup_alpha_pass_idx = T2E_T1$nodup_alpha_pass_idx, pass_RT_i = pass_RT_Ti)

	# pass_RT_theta_RT = ssR$t_grid[[14]]$theta_T[pass_RT_Ridx]
	pass_RT_theta_RT = lapply(X = 1:length(n), FUN = function(i, t_grid, pass_RT_idx){t_grid[[i]]$theta_T[pass_RT_idx[[i]]]}, t_grid = ssR$t_grid, pass_RT_idx = pass_RT_Ridx)
	# pass_RT_theta_RL = ssR$t_grid[[14]]$theta_L[pass_RT_Ridx]
	pass_RT_theta_RL = lapply(X = 1:length(n), FUN = function(i, t_grid, pass_RT_idx){t_grid[[i]]$theta_L[pass_RT_idx[[i]]]}, t_grid = ssR$t_grid, pass_RT_idx = pass_RT_Ridx)
	# pass_RT_theta_TT = ssT$t_grid[[14]]$theta_T[pass_RT_Tidx]
	pass_RT_theta_TT = lapply(X = 1:length(n), FUN = function(i, t_grid, pass_RT_idx){t_grid[[i]]$theta_T[pass_RT_idx[[i]]]}, t_grid = ssT$t_grid, pass_RT_idx = pass_RT_Tidx)
	# pass_RT_theta_TL = ssT$t_grid[[14]]$theta_L[pass_RT_Tidx]
	pass_RT_theta_TL = lapply(X = 1:length(n), FUN = function(i, t_grid, pass_RT_idx){t_grid[[i]]$theta_L[pass_RT_idx[[i]]]}, t_grid = ssT$t_grid, pass_RT_idx = pass_RT_Tidx)
	
	# pass_RT_cxR = T2E_R1$cx_grid[[14]][,pass_RT_Ridx]
	# pass_RT_cxR = lapply(X = 1:length(n), FUN = function(i, cx_grid, pass_RT_idx){cx_grid[[i]][,pass_RT_idx[[i]]]}, cx_grid = T2E_R1$cx_grid, pass_RT_idx = pass_RT_Ridx)
	pass_RT_cxR = lapply(X = 1:length(n), FUN = function(i, cx_grid, pass_RT_idx){subset(cx_grid[[i]], select = pass_RT_idx[[i]])}, cx_grid = T2E_R1$cx_grid, pass_RT_idx = pass_RT_Ridx)

	# pass_RT_cxT = T2E_T1$cx_grid[[14]][,pass_RT_Tidx]
	# pass_RT_cxT = lapply(X = 1:length(n), FUN = function(i, cx_grid, pass_RT_idx){cx_grid[[i]][,pass_RT_idx[[i]]]}, cx_grid = T2E_T1$cx_grid, pass_RT_idx = pass_RT_Tidx)
	pass_RT_cxT = lapply(X = 1:length(n), FUN = function(i, cx_grid, pass_RT_idx){subset(cx_grid[[i]], select = pass_RT_idx[[i]])}, cx_grid = T2E_T1$cx_grid, pass_RT_idx = pass_RT_Tidx)

	#make EN_RT_max function to get the max of E01 and E10
	# EN_RT(n = n[[14]], pR = pR0, pT = pT1, cxR = pass_RT_cxR, cxT = pass_RT_cxT)
	# EN_RT(n = n[[14]], pR = pR1, pT = pT0, cxR = pass_RT_cxR, cxT = pass_RT_cxT)
	# EN_RT_max(n = n[[14]], pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = pass_RT_cxR, cxT = pass_RT_cxT)
	# i=14
	exp_accrual = lapply(X = seq_len(length(n)), FUN = function(i, n_l, pR0, pR1, pT0, pT1, cxR_l, cxT_l){
			stopifnot(ncol(cxR_l[[i]]) == ncol(cxT_l[[i]]))
			vapply(X = seq_len(ncol(cxR_l[[i]])), FUN = function(j, n, pR0, pR1, pT0, pT1, cxR, cxT){
				# EN_RT_max(n = n, pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = cxR[,j], cxT = cxT[,j])
				EN_mat_RT_max(n = n, pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = cxR[,j], cxT = cxT[,j])
			}, FUN.VALUE = 1., n = n_l[[i]], pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = cxR_l[[i]], cxT = cxT_l[[i]])
		}, n_l = n, pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR_l = pass_RT_cxR, cxT_l = pass_RT_cxT)
	
	# exp_accrual = lapply(X = 1:length(n), function(i, nodup_alpha_power_pass_idx, n, p0, cx_grid){
			# vapply(X = nodup_alpha_power_pass_idx[[i]], FUN = function(idx, n_i, p0, cx_grid_i){
				# EN(n = n_i, p = p0, cx = cx_grid_i[,idx])
			# }, FUN.VALUE = 1., n_i=n[[i]], p0=p0, cx_grid_i=cx_grid[[i]])
		# }, nodup_alpha_power_pass_idx=nodup_alpha_power_pass_idx, n=n, p0=p0, cx_grid=cx_grid)
	
	exp_accrual_min_idx = lapply(X = exp_accrual, FUN = which.min)
	exp_accrual_min = lapply(X = 1:length(n), FUN = function(x, idx, val){val[[x]][idx[[x]]]}, idx = exp_accrual_min_idx, val = exp_accrual)
	
	optim_i = which.min(exp_accrual_min)
	# optim_idx = exp_accrual_min_idx[[optim_i]]
	# optim_n = n[[optim_i]]
	# optim_nodup_alpha_power_pass_idx = nodup_alpha_power_pass_idx[[optim_i]][optim_idx]
	# optim_cx = cx_grid[[optim_i]][,optim_nodup_alpha_power_pass_idx]
	# optim_cxR = pass_RT_cxR[[optim_i]][,optim_idx]
	# optim_cxT = pass_RT_cxT[[optim_i]][,optim_idx]
	# optim_t = t_grid[[optim_i]][optim_nodup_alpha_power_pass_idx,]
	# optim_theta_RT = pass_RT_theta_RT[[optim_i]][optim_idx]
	# optim_theta_RL = pass_RT_theta_RL[[optim_i]][optim_idx]
	# optim_theta_TT = pass_RT_theta_TT[[optim_i]][optim_idx]
	# optim_theta_TL = pass_RT_theta_TL[[optim_i]][optim_idx]
	
	# optim_EN = exp_accrual[[optim_i]][optim_idx]
	# optim_EN2 = EN(n = optim_n, p = p0, cx = optim_cx) #for unit testing:
	# optim_EN == optim_EN2
	# optim_T1E = p_success(n = optim_n, p = p0, cx = optim_cx)
	# optim_T1E = p_success_mat(n = optim_n, p = p0, cx = optim_cx)
	# optim_T1E_R0T1 = pass_RT_T1E_R0T1[[optim_i]][optim_idx]
	# optim_T1E_R1T0 = pass_RT_T1E_R1T0[[optim_i]][optim_idx]
	# optim_power = p_success(n = optim_n, p = p1, cx = optim_cx)
	# optim_power = p_success_mat(n = optim_n, p = p1, cx = optim_cx)
	# optim_power = pass_RT_pow[[optim_i]][optim_idx]
	# optim_pet = pet(n = optim_n, p = p0, cx = optim_cx)
	# optim_pet = pet_RT_min(n = optim_n, pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = optim_cxR, cxT = optim_cxT)
	# optim_n_cx = rm_stages(optim_n, optim_cx)
	# optim_n_cx = rm_stages_RT(optim_n, optim_cxR, optim_cxT)
	
	# optim_stats = data.frame(design = "optimal", n = paste0(optim_n, collapse=","), optim_t, x_fail=paste0(optim_cx$x_fail, collapse=","), T1E = optim_T1E, power = optim_power, EN = optim_EN, pet = optim_pet, stringsAsFactors=F)
	# optim_stats = data.frame(design = "optimal", 
		# n = paste0(optim_n_cx$n, collapse=","), 
		# theta_RT = optim_theta_RT, 
		# theta_RL = optim_theta_RL, 
		# theta_TT = optim_theta_TT, 
		# theta_TL = optim_theta_TL, 
		# cxR_fail=paste0(optim_n_cx$cxR$x_fail, collapse=","), 
		# cxT_fail=paste0(optim_n_cx$cxT$x_fail, collapse=","), 
		# T1E_R0T1 = optim_T1E_R0T1, 
		# T1E_R1T0 = optim_T1E_R1T0, 
		# power = optim_power, 
		# EN = optim_EN, 
		# pet = optim_pet, stringsAsFactors=F)
	optim_stats = design_stats_RT(i = optim_i, 
		design_name = "optimal", 
		exp_accrual_min_idx = exp_accrual_min_idx, 
		n = n, 
		pass_RT_cxR = pass_RT_cxR, 
		pass_RT_cxT = pass_RT_cxT, 
		pass_RT_theta_RT = pass_RT_theta_RT, 
		pass_RT_theta_RL = pass_RT_theta_RL, 
		pass_RT_theta_TT = pass_RT_theta_TT, 
		pass_RT_theta_TL = pass_RT_theta_TL, 
		exp_accrual = exp_accrual, 
		pass_RT_T1E_R0T1 = pass_RT_T1E_R0T1, 
		pass_RT_T1E_R1T0 = pass_RT_T1E_R1T0,
		pass_RT_pow = pass_RT_pow,
		pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1)

	# minimax
	i_pass = which(unlist(lapply(X = exp_accrual_min_idx, FUN = length)) > 0)
	n_nmax = unlist(lapply(X=n, FUN = max, na.rm=T))
	minimax_i = i_pass[which.min(n_nmax[i_pass])]
	minimax_stats = design_stats_RT(i = minimax_i, 
		design_name = "minimax", 
		exp_accrual_min_idx = exp_accrual_min_idx, 
		n = n, 
		pass_RT_cxR = pass_RT_cxR, 
		pass_RT_cxT = pass_RT_cxT, 
		pass_RT_theta_RT = pass_RT_theta_RT, 
		pass_RT_theta_RL = pass_RT_theta_RL, 
		pass_RT_theta_TT = pass_RT_theta_TT, 
		pass_RT_theta_TL = pass_RT_theta_TL, 
		exp_accrual = exp_accrual, 
		pass_RT_T1E_R0T1 = pass_RT_T1E_R0T1, 
		pass_RT_T1E_R1T0 = pass_RT_T1E_R1T0,
		pass_RT_pow = pass_RT_pow,
		pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1)
	
	optim_minimax_stats = rbind(optim_stats, minimax_stats)
	
	exp_accrual_min_no_mm_op = exp_accrual_min
	exp_accrual_min_no_mm_op[[optim_i]] = numeric(0)
	exp_accrual_min_no_mm_op[[minimax_i]] = numeric(0)
	alt1_i = which.min(exp_accrual_min_no_mm_op)
	# alt1_i = integer(0)
	
	if(length(alt1_i)>0){
		alt1_stats = design_stats_RT(i = alt1_i, 
			design_name = "alt1", 
			exp_accrual_min_idx = exp_accrual_min_idx, 
			n = n, 
			pass_RT_cxR = pass_RT_cxR, 
			pass_RT_cxT = pass_RT_cxT, 
			pass_RT_theta_RT = pass_RT_theta_RT, 
			pass_RT_theta_RL = pass_RT_theta_RL, 
			pass_RT_theta_TT = pass_RT_theta_TT, 
			pass_RT_theta_TL = pass_RT_theta_TL, 
			exp_accrual = exp_accrual, 
			pass_RT_T1E_R0T1 = pass_RT_T1E_R0T1, 
			pass_RT_T1E_R1T0 = pass_RT_T1E_R1T0,
			pass_RT_pow = pass_RT_pow,
			pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1)
		optim_minimax_stats = rbind(optim_minimax_stats, alt1_stats)

		exp_accrual_min_no_mm_op[[alt1_i]] = numeric(0)
		alt2_i = which.min(exp_accrual_min_no_mm_op)
		# alt2_i = integer(0)
	# }
	
		if(length(alt2_i)>0){
			alt2_stats = design_stats_RT(i = alt2_i, 
				design_name = "alt2", 
				exp_accrual_min_idx = exp_accrual_min_idx, 
				n = n, 
				pass_RT_cxR = pass_RT_cxR, 
				pass_RT_cxT = pass_RT_cxT, 
				pass_RT_theta_RT = pass_RT_theta_RT, 
				pass_RT_theta_RL = pass_RT_theta_RL, 
				pass_RT_theta_TT = pass_RT_theta_TT, 
				pass_RT_theta_TL = pass_RT_theta_TL, 
				exp_accrual = exp_accrual, 
				pass_RT_T1E_R0T1 = pass_RT_T1E_R0T1, 
				pass_RT_T1E_R1T0 = pass_RT_T1E_R1T0,
				pass_RT_pow = pass_RT_pow,
				pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1)
			optim_minimax_stats = rbind(optim_minimax_stats, alt2_stats)
		}
	}
	# optim_minimax_stats
	return(optim_minimax_stats)
}

#TODO: function for dependent case

def_search_space = function(n, p0, a0, b0){
	theta_x_l = lapply(X = n, FUN = calc_theta_grid, p0 = p0, a0 = a0, b0 = b0)
	t_grid = lapply(X = theta_x_l, FUN = function(x){rbind(data.frame(theta_T=unique(x$theta_T),theta_L=0), unique(x[,c("theta_T", "theta_L")]))})
	t_grid = lapply(X = t_grid, FUN = function(x){x[x$theta_L < 1,]})
	cx_grid = lapply(X = 1:length(n), FUN = function(i, t_grid, n, theta_x_l){
		mapply(FUN = calc_cutoff_grid_NA, t_grid[[i]]$theta_T, t_grid[[i]]$theta_L, 
			MoreArgs = list(n = n[[i]], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, 
			x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp))}, 
		t_grid=t_grid, n=n, theta_x_l=theta_x_l)

	cx_dup = lapply(X = cx_grid, FUN = function(cx){rep(F, ncol(cx))})
	nodup_idx = lapply(X = cx_dup, FUN = function(x){which(!x)})
	n_nodup = lapply(X = nodup_idx, FUN = length)
	
	return(list(
		theta_x_l = theta_x_l,
		t_grid = t_grid,
		cx_grid = cx_grid,
		nodup_idx = nodup_idx,
		n_nodup = n_nodup
	))
}

# calc_T12E_lt_ab = function(n_nodup, n, nodup_idx, theta_x_l, t_grid, cx_grid, cx_grid_nm, p0, alpha, env){
calc_T12E_lt_ab = function(n_nodup, n, nodup_idx, theta_x_l, t_grid, cx_grid, p0, alpha, passfun){
	fun = match.fun(passfun)
	T1E = lapply(X = n_nodup, FUN = rep, x = NA)
	T1E_lt_alpha = T1E
	for(i in 1:length(n)){
		for(j in 1:n_nodup[[i]]){
			idx = nodup_idx[[i]][j]
			if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(T1E_lt_alpha[[i]])] <= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 0
			} 
			else if(any(t_grid[[i]]$theta_T[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_T[idx] & t_grid[[i]]$theta_L[nodup_idx[[i]]][which(!T1E_lt_alpha[[i]])] >= t_grid[[i]]$theta_L[idx], na.rm=T)){
				T1E[[i]][j] = 1
			} 
			else {
				if(all(is.na(cx_grid[[i]][,idx]$x_fail))){
					cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
				}
				# assign(xmat_nm, x_n, envir = env)
				T1E[[i]][j] = p_success_mat(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
				T1E_lt_alpha[[i]][j] = T1E[[i]][j] < alpha
			}
		}
	}
	
	nodup_alpha_pass_idx = lapply(X = 1:length(n), 
		# FUN = function(i, T1E, alpha, nodup_idx){nodup_idx[[i]][which(T1E[[i]] < alpha)]}, 
		FUN = function(i, T1E, alpha, nodup_idx){nodup_idx[[i]][which(fun(T1E[[i]], alpha))]}, 
		T1E = T1E, alpha = alpha, nodup_idx = nodup_idx)
	# T1E_pass = lapply(X = T1E, FUN = function(x,a){x[which(x<a)]}, a = alpha)
	T1E_pass = lapply(X = T1E, FUN = function(x,a){x[which(fun(x,a))]}, a = alpha)
	n_nodup_alpha_pass = lapply(X = nodup_alpha_pass_idx, FUN = length)

	# assign(cx_grid_nm, cx_grid, envir = env)
	# return(T1E)
	return(list(
		T1E = T1E,
		cx_grid = cx_grid,
		nodup_alpha_pass_idx = nodup_alpha_pass_idx,
		T1E_pass = T1E_pass,
		n_nodup_alpha_pass = n_nodup_alpha_pass #,
		#T1E_lt_alpha = T1E_lt_alpha
	))
}

#function to remove redundant stages
rm_stages = function(n, cx){
	cx_r = cx$x_fail
	cx_t = cx$x_pass
	# idx_nodup = which((!is.na(cx_r) & !duplicated(cx_r)) | (!is.na(cx_t) & !duplicated(cx_t)))
	# idx_nodup = which(!is.na(cx_r) & !duplicated(cx_r))
	idx_nodup = which(!(is.na(cx_r) & is.na(cx_t)) & !(duplicated(cx_r) & duplicated(cx_t)))
	n_nodup = n[idx_nodup]
	cx_r_nodup = cx_r[idx_nodup]
	cx_t_nodup = cx_t[idx_nodup]
	cx_nodup = list(x_fail = cx_r_nodup, x_pass = cx_t_nodup)
	return(list(n = n_nodup, cx = cx_nodup))
}

rm_stages_RT = function(n, cxR, cxT){
	cxR_r = cxR$x_fail
	cxR_t = cxR$x_pass
	cxT_r = cxT$x_fail
	cxT_t = cxT$x_pass
	# idx_nodup = which((!is.na(cx_r) & !duplicated(cx_r)) | (!is.na(cx_t) & !duplicated(cx_t)))
	# idx_nodup = which(!is.na(cxR_r) & !duplicated(cxR_r) & !is.na(cxT_r) & !duplicated(cxT_r))
	idx_nodup = which(!(is.na(cxR_r) & is.na(cxT_r) & is.na(cxR_t) & is.na(cxT_t)) & 
		!(duplicated(cxR_r) & duplicated(cxT_r) & duplicated(cxR_t) & duplicated(cxT_t)))
	n_nodup = n[idx_nodup]
	cxR_r_nodup = cxR_r[idx_nodup]
	cxR_t_nodup = cxR_t[idx_nodup]
	cxT_r_nodup = cxT_r[idx_nodup]
	cxT_t_nodup = cxT_t[idx_nodup]
	cxR_nodup = list(x_fail = cxR_r_nodup, x_pass = cxR_t_nodup)
	cxT_nodup = list(x_fail = cxT_r_nodup, x_pass = cxT_t_nodup)
	return(list(n = n_nodup, cxR = cxR_nodup, cxT = cxT_nodup))
}

design_stats_RT = function(i, 
		design_name, 
		exp_accrual_min_idx, 
		n, 
		pass_RT_cxR, 
		pass_RT_cxT, 
		pass_RT_theta_RT, 
		pass_RT_theta_RL, 
		pass_RT_theta_TT, 
		pass_RT_theta_TL, 
		exp_accrual, 
		pass_RT_T1E_R0T1, 
		pass_RT_T1E_R1T0,
		pass_RT_pow,
		pR0, pR1, pT0, pT1){
	idx = exp_accrual_min_idx[[i]]
	n = n[[i]]
	cxR = pass_RT_cxR[[i]][,idx]
	cxT = pass_RT_cxT[[i]][,idx]
	theta_RT = pass_RT_theta_RT[[i]][idx]
	theta_RL = pass_RT_theta_RL[[i]][idx]
	theta_TT = pass_RT_theta_TT[[i]][idx]
	theta_TL = pass_RT_theta_TL[[i]][idx]
	EN = exp_accrual[[i]][idx]
	T1E_R0T1 = pass_RT_T1E_R0T1[[i]][idx]
	T1E_R1T0 = pass_RT_T1E_R1T0[[i]][idx]
	power = pass_RT_pow[[i]][idx]
	pet = pet_RT_min(n = n, pR0 = pR0, pR1 = pR1, pT0 = pT0, pT1 = pT1, cxR = cxR, cxT = cxT)
	n_cx = rm_stages_RT(n, cxR, cxT)
	
	stats = data.frame(design = design_name, 
		n = paste0(n_cx$n, collapse=","), 
		theta_RT = theta_RT, 
		theta_RL = theta_RL, 
		theta_TT = theta_TT, 
		theta_TL = theta_TL, 
		cxR_fail=paste0(n_cx$cxR$x_fail, collapse=","), 
		cxT_fail=paste0(n_cx$cxT$x_fail, collapse=","), 
		T1E_R0T1 = T1E_R0T1, 
		T1E_R1T0 = T1E_R1T0, 
		power = power, 
		EN = EN, 
		pet = pet, stringsAsFactors=F)
	return(stats)
}

# fast_T1E_power = function(){
	# for(){
		# if(){
			# return(0)
		# }
		# else if (){
			# return(1)
		# }
	# }
	# cx_grid[[i]][,idx] = calc_cutoff_grid(n = n[[i]], theta_T = t_grid[[i]]$theta_T[idx], theta_L = t_grid[[i]]$theta_L[idx], theta_T_exp = theta_x_l[[i]]$theta_T, theta_L_exp = theta_x_l[[i]]$theta_L, x_exp = theta_x_l[[i]]$x_exp, l_exp = theta_x_l[[i]]$l_exp)
	# T1E[[i]][j] = p_success_mat(n = n[[i]], p = p0, cx = cx_grid[[i]][,idx])
	# T1E_lt_alpha[[i]][j] = T1E[[i]][j] < alpha
	# return(T1E[[i]][j])
# }

##response + toxicity independent
##response + toxicity dependent
##response + toxicity prior
##response only prior

#fill matrix in a vectorized fashion
##response only
##response + toxicity independent
##response + toxicity dependent
##response + toxicity prior
##response only prior

#shiny

#other helpful functions
sort_df = function(x, coln=1, ...){
	x = x[order(x[,coln], ...),, drop = F]
	return(x)
}

#TODO:
#try using Reduce() to speed it up
pxn_mat = function(n, p, cx){
	# n = c(2,4); p = 0.5; cx = list(x_fail = c(0, 1), x_pass = c(NA, 2))
	# n = c(2,4,6); p = 0.5; cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3))
	# n = c(2,4,6); p = 0.5; cx = list(x_fail = c(NA, NA, NA), x_pass = c(NA, NA, 3))
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	d = diff(c(0, n))
	x_fail = cx$x_fail
	x_fail[is.na(x_fail)] = -1
	x_pass = cx$x_pass
	x_pass[is.na(x_pass)] = -1

	# x_fail = cx$x_fail
	# x_pass = cx$x_pass
	
	# keep_stages = c(which(!is.na(x_fail[-k]) & !duplicated(x_fail[-k])),k)
	# x_fail = x_fail[keep_stages]
	# x_pass = x_pass[keep_stages]
	# n = n[keep_stages]
	
	# k = length(n)
	# d = diff(c(0, n))

	# px2 = c(1,rep(0, nk)) %*% bdiag(diag(0), dbinom(outer((nk+1):1, (nk+1):1, FUN = "-"), size = d[1], prob = p))
	counts = outer((nk+1):1, (nk+1):1, FUN = "-")
	t_probs = dbinom(counts, size = d[1], prob = p)
	# mat = bdiag(diag(0), t_probs)
	px2 = c(1,rep(0, nk)) %*% t_probs
	if (k>1){
		for(i in 1:(k-1)){
			# px2 = px2 %*% bdiag(diag(x_fail[i]+1), dbinom(outer((nk-x_fail[i]):1, (nk-x_fail[i]):1, FUN = "-"), size = d[i+1], prob = p))
			counts = outer((nk-x_fail[i]):1, (nk-x_fail[i]):1, FUN = "-")
			t_probs = dbinom(counts, size = d[i+1], prob = p)
			# mat = bdiag(diag(x_fail[i]+1), t_probs)
			px2 = c(px2[1:(x_fail[i]+1)] %*% diag(x_fail[i]+1), px2[(x_fail[i]+2):(nk+1)] %*% t_probs)
		}
	}
	# px_mat = c(bdiag(diag(0), dbinom(outer((nk+1):1, (nk+1):1, FUN = "-"), size = d[1], prob = p)),
		# lapply(X = 1:(k-1), FUN = function(i, x_fail, nk, d, p){bdiag(diag(x_fail[i]+1), dbinom(outer((nk-x_fail[i]):1, (nk-x_fail[i]):1, FUN = "-"), size = d[i+1], prob = p))}, x_fail = x_fail, nk = nk, d = d, p = p))
	# px2 = Reduce(`%*%`, px_mat, c(1,rep(0, nk)))
	# %*% 
		# bdiag(diag(x_fail[1]+1), dbinom(outer((nk-x_fail[1]):1, (nk-x_fail[1]):1, FUN = "-"), size = d[2], prob = p)) %*% 
		# bdiag(diag(x_fail[2]+1), dbinom(outer((nk-x_fail[2]):1, (nk-x_fail[2]):1, FUN = "-"), size = d[3], prob = p))
	return(as.vector(px2))
}

p_success_mat = function(n, p, cx){
	k = length(n)
	stopifnot(all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	x_fail = cx$x_fail
	xk_pass = max(x_fail, -1, na.rm=T)+1
	px = pxn_mat(n=n, p=p, cx=cx)
	
	return(sum(px[(xk_pass+1):(nk+1)]))
}

pxn_RT_mat = function(n, pR, pT, cxR, cxT){
	# n = c(2,4); pR = 0.25; pT = 0.5; cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)); cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3))
	k = length(n)
	stopifnot(all(vapply(X = cxR, FUN = length, FUN.VALUE = 1) == k))
	stopifnot(all(vapply(X = cxT, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_fail[is.na(xR_fail)] = -1
	xR_pass = cxR$x_pass
	xR_pass[is.na(xR_pass)] = -1

	xT_fail = cxT$x_fail
	xT_fail[is.na(xT_fail)] = -1
	xT_pass = cxT$x_pass
	xT_pass[is.na(xT_pass)] = -1

	# A = matrix(c(1,rep(0, (nk+1)*(nk+1)-1)), ncol = nk+1, nrow = nk+1)
	a = c(1,rep(0, (nk+1)*(nk+1)-1))
	
	counts = outer((nk+1):1, (nk+1):1, FUN = "-")
	tR_probs = dbinom(counts, size = d[1], prob = pR)
	tT_probs = dbinom(counts, size = d[1], prob = pT)
	t_probs = tR_probs %x% tT_probs
	px2 = a %*% t_probs
	# i=1
	if (k>1){
		for(i in 1:(k-1)){
			Rcounts = outer((nk-xR_fail[i]):1, (nk-xR_fail[i]):1, FUN = "-")
			Tcounts = outer((nk-xT_fail[i]):1, (nk-xT_fail[i]):1, FUN = "-")
			tR_probs = dbinom(Rcounts, size = d[i+1], prob = pR)
			tT_probs = dbinom(Tcounts, size = d[i+1], prob = pT)
			t_probs = tR_probs %x% tT_probs
			RT_fail = rep(0:nk <= xR_fail[i], each = nk+1) | 
				rep(0:nk <= xT_fail[i], times = nk+1)
			px2 = c(px2[RT_fail], px2[!RT_fail] %*% t_probs)[order(c(which(RT_fail), which(!RT_fail)))]
		}
	}
	return(as.vector(px2)) #should we return a matrix?
}

p_success_RT_mat = function(n, pR, pT, cxR, cxT){
	return(p_success_mat(n=n, p=pR, cx=cxR) * p_success_mat(n=n, p=pT, cx=cxT))
}

pxn_RT_OR_mat = function(n, pR, pT, cxR, cxT, phi = 1){
	# n = c(2,4); pR = 0.25; pT = 0.5; cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)); cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)); phi = 1
	# n = c(2,4); pR = 0.25; pT = 0.5; cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)); cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)); phi = .1
	# n = c(2,4); pR = 0.25; pT = 0.5; cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)); cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)); phi = 10
	k = length(n)
	stopifnot(all(vapply(X = cxR, FUN = length, FUN.VALUE = 1) == k))
	stopifnot(all(vapply(X = cxT, FUN = length, FUN.VALUE = 1) == k))
	nk = n[k]
	d = diff(c(0, n))
	xR_fail = cxR$x_fail
	xR_fail[is.na(xR_fail)] = -1
	xR_pass = cxR$x_pass
	xR_pass[is.na(xR_pass)] = -1

	xT_fail = cxT$x_fail
	xT_fail[is.na(xT_fail)] = -1
	xT_pass = cxT$x_pass
	xT_pass[is.na(xT_pass)] = -1

	# A = matrix(c(1,rep(0, (nk+1)*(nk+1)-1)), ncol = nk+1, nrow = nk+1)
	a = c(1,rep(0, (nk+1)*(nk+1)-1))
	
	counts = outer((nk+1):1, (nk+1):1, FUN = "-")
	# tR_probs = dbinom(counts, size = d[1], prob = pR)
	# tT_probs = dbinom(counts, size = d[1], prob = pT)
	# t_probs = tR_probs %x% tT_probs
	# t_probs = kronecker(X = counts, Y = counts, FUN = sum_multinom, dl=d[1], pR=pR, pT=pT, phi=phi) #currently not working
	t_probs = my_kronecker(X = counts, Y = counts, FUN = sum_multinom, dl=d[1], pR=pR, pT=pT, phi=phi) #currently not working
	px2 = a %*% t_probs
	# i=1
	if (k>1){
		for(i in 1:(k-1)){
			Rcounts = outer((nk-xR_fail[i]):1, (nk-xR_fail[i]):1, FUN = "-")
			Tcounts = outer((nk-xT_fail[i]):1, (nk-xT_fail[i]):1, FUN = "-")
			# tR_probs = dbinom(Rcounts, size = d[i+1], prob = pR)
			# tT_probs = dbinom(Tcounts, size = d[i+1], prob = pT)
			# t_probs = tR_probs %x% tT_probs
			# t_probs = kronecker(X = Rcounts, Y = Tcounts, FUN = sum_multinom, dl=d[1], pR=pR, pT=pT, phi=phi)
			t_probs = my_kronecker(X = Rcounts, Y = Tcounts, FUN = sum_multinom, dl=d[1], pR=pR, pT=pT, phi=phi)
			RT_fail = rep(0:nk <= xR_fail[i], each = nk+1) | 
				rep(0:nk <= xT_fail[i], times = nk+1)
			px2 = c(px2[RT_fail], px2[!RT_fail] %*% t_probs)[order(c(which(RT_fail), which(!RT_fail)))]
		}
	}
	return(as.vector(px2)) #should we return a matrix?
}

sum_multinom = function(i, j, dl, pR, pT, phi = 1){
	if(i < 0 || j < 0 || i > dl || j > dl){
		return(0)
	}
	
	p = calc_p_OR(pT = pT, pR = pR, phi = phi)
	min_g = max(0,i+j-dl)
	max_g = min(i, j)
	prob = 0
	if (min_g <= max_g){
		for(g in min_g:max_g){
			prob = prob + my_dmultinom(x=c(dl-i-j+g, j-g, i-g, g), size = dl, prob=p)
		}
	}
	return(prob)
}

my_kronecker = function(X, Y, FUN = "*", ...){
	fun = match.fun(FUN)
	# sapply(X = X, FUN = function(y){sapply(X = Y, FUN = fun, x, ...)})
	# xrow = nrow(X)
	xcol = ncol(X)
	# yrow = nrow(Y)
	ycol = ncol(Y)
	xlen = length(x)
	ylen = length(y)
	# apply(X = x, MARGIN = 1:2, FUN = "*", y[1,1])
	# lapply(X = y[1,], FUN = function(y){apply(X = x, MARGIN = 1:2, FUN = "*", y)})
	# do.call(cbind, lapply(X = y[1,], FUN = function(y){apply(X = x, MARGIN = 1:2, FUN = "*", y)}))
	# lapply(X = 1:yrow, FUN = function(i) {do.call(cbind, lapply(X = y[i,], FUN = function(y_el){apply(X = x, MARGIN = 1:2, FUN = "*", y_el)}))})
	# do.call(rbind,lapply(X = 1:yrow, FUN = 
		# function(i) {do.call(cbind, lapply(X = Y[i,], FUN = 
			# function(y_el){apply(X = X, MARGIN = 1:2, FUN = fun, y_el, ...)}))}))
	# do.call(rbind,lapply(X = 1:xrow, FUN = 
		# function(i) {do.call(cbind, lapply(X = X[i,], FUN = 
			# function(x_el){apply(X = Y, MARGIN = 1:2, FUN = 
				# function(y, x) {fun(x,y, ...)}, x_el)}))}))
	# apply(X = y, MARGIN = 1:2, FUN = function(b,a){"*"(a,b)}, x[1,1])
	# do.call(cbind, lapply(X = x[1,], FUN = function(x_el){apply(X = y, MARGIN = 1:2, FUN = function(b,a){"*"(a,b)}, x_el)}))
	do.call(rbind, lapply(X = 1:xrow, FUN = 
		function(i) {do.call(cbind, lapply(X = X[i,], FUN = 
			function(x_el){apply(X = Y, MARGIN = 1:2, FUN = 
				function(b,a){fun(a,b, ...)}, x_el)}))}))
	# matrix(lapply(X=Y, FUN = function(y) {lapply(X=X, FUN = fun, y, ...)}), ncol = xcol * ycol)
	# apply(X = Y, MARGIN = 1:2, FUN = function(y) {sapply(X=X, FUN = "*", simplify = "matrix", y)})
	# apply(X = Y, MARGIN = 1:2, FUN = function(y) {apply(X=X, MARGIN = 1:2, FUN = "*", y)})

	#TODO: mapply may be more efficient
	# matrix(mapply(FUN = fun, rep(X, each = ylen), rep(Y, times = xlen), ...), ncol = xcol * ycol) #this put them in the wrong order

}

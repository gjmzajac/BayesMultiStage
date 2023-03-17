library(profvis)
source("BayesMultiStage.R")

#lee and liu
#table 2
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=c(10, 16, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36))
calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=10:36)
calc_cutoffs(theta_L = 0.001, theta_U = 0.99999, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=10:36)

calc_cutoffs(theta_L = 0.001, theta_U = 0.99999, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)

#table 3
calc_cutoffs(theta_L = 0.077, theta_T = 0.9435, p0=0.6, p1=0.8, n=10:35)

#table 4
calc_cutoffs(theta_L = 0.001, theta_T = 0.8465, p0=0.1, p1=0.3, n=10:25)

#2019 application
#delta = 0.95, gamma = 0.2
# r = c(8, 20)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(25, 50), p0 = 0.3, p1 = 0.5, a0 = 1, b0 = 1)
# r = c(4, 10, 20)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(15, 30, 50), p0 = 0.3, p1 = 0.5, a0 = 1, b0 = 1)
# r = c(2, 6, 10, 15, 20)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(10, 20, 30, 40, 50), p0 = 0.3, p1 = 0.5, a0 = 1, b0 = 1)

#NCT03377023
# r = c(6, 16)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(20, 40), p0 = 0.3, p1 = 0.5, a0 = 1, b0 = 1)
# r = c(1, 5)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(20, 40), p0 = 0.07, p1 = 0.2, a0 = 1, b0 = 1)

#NCT03611738
# r = c(2, 6)
calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = c(15, 30), p0 = 0.12, p1 = 0.32, a0 = 1, b0 = 1)

#pxn
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)

lapply(X = 0:36, FUN = pxn, n=lee_liu_n, p=0.2, cx=cx_ex1)
pxn(x=0, n=lee_liu_n, p=0.2, cx=cx_ex1)

for(i in 0:36){
	cat(i, pxn(x=i, n=lee_liu_n, p=0.2, cx=cx_ex1), "\n")
}
# 0.08777926
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))

cx_ex1.1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=1:36)
# 0.08777926
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=1:36, p=0.2, cx=cx_ex1.1))

pxn(x=0, n=1:36, p=0.2, cx=cx_ex1.1)
pxn(x=11, n=1:36, p=0.2, cx=cx_ex1.1)

#test T&R indep

pxn_RT(xR=11, xT=36, n=lee_liu_n, pR=0.2, pT=1, cxR=cx_ex1, cxT=cx_ex1)
pxn_RT(xR=11, xT=36, n=1:36, pR=0.2, pT=1, cxR=cx_ex1.1, cxT=cx_ex1.1)

#test T&R joint
pxn_RT_OR(xR=11, xT=36, n=lee_liu_n, pR=0.2, pT=1, cxR=cx_ex1, cxT=cx_ex1, phi = 1)
pxn_RT_OR(xR=11, xT=36, n=1:36, pR=0.2, pT=1, cxR=cx_ex1.1, cxT=cx_ex1.1, phi = 1)

cxT_miss = list(x_fail = rep(NA, 11), x_pass = rep(NA, 11))

pxn_RT_OR(xR=11, xT=20, n=lee_liu_n, pR=0.2, pT=0.6, cxR=cx_ex1, cxT=cxT_miss, phi = 0.1)
pxn_RT_OR(xR=11, xT=20, n=lee_liu_n, pR=0.2, pT=0.6, cxR=cx_ex1, cxT=cxT_miss, phi = 1)
pxn_RT_OR(xR=11, xT=20, n=lee_liu_n, pR=0.2, pT=0.6, cxR=cx_ex1, cxT=cxT_miss, phi = 10)

#example from page 1379 of bryant and day
# N1 = 19, N2, = 33, CR1 = 5, CR2 = 12, CT1 = 11, CT2 = 22.
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
cxNA1 = list(x_fail = NA, x_pass = NA)

pxn_RT(xR=13, xT=23, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT(xR=13, xT=23, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)

pxn_RT_OR(xR=13, xT=23, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT_OR(xR=13, xT=23, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)

#type 1 error, power
alpha_00 = 0
alpha_01 = 0
alpha_10 = 0
alpha_11 = 0
for(i in 13:33) {
	for(j in 23:33) {
		alpha_00 = alpha_00 + pxn_RT(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
		alpha_01 = alpha_01 + pxn_RT(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)
		alpha_10 = alpha_10 + pxn_RT(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
		alpha_11 = alpha_11 + pxn_RT(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)
	}
}
alpha_00
alpha_01
alpha_10
alpha_11

#accrual
p_01 = 0
p_10 = 0
for(i in 6:19) {
	for(j in 12:19) {
		p_01 = p_01 + pxn_RT(xR=i, xT=j, n=19, pR=0.3, pT=0.8, cxR=cxNA1, cxT=cxNA1)
		p_10 = p_10 + pxn_RT(xR=i, xT=j, n=19, pR=0.5, pT=0.6, cxR=cxNA1, cxT=cxNA1)
	}
}
max(19 + (33-19) * c(p_01, p_10))
# 26.2

#non-indep
for (phi in c(0.01, 0.1, 1, 10, 100)) {
	cat("phi =", phi, "\n")
	#type 1 error, power
	alpha_00 = 0
	alpha_01 = 0
	alpha_10 = 0
	alpha_11 = 0
	for(i in 13:33) {
		for(j in 23:33) {
			alpha_00 = alpha_00 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_01 = alpha_01 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_10 = alpha_10 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_11 = alpha_11 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
		}
	}
	cat("alpha_00 =", alpha_00, "\n")
	cat("alpha_01 =", alpha_01, "\n")
	cat("alpha_10 =", alpha_10, "\n")
	cat("alpha_11 =", alpha_11, "\n")
	cat("beta =", 1-alpha_11, "\n")

	#accrual
	p_00 = 0
	p_01 = 0
	p_10 = 0
	p_11 = 0
	for(i in 6:19) {
		for(j in 12:19) {
			p_00 = p_00 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.3, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_01 = p_01 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.3, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_10 = p_10 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.5, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_11 = p_11 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.5, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
		}
	}
	cat("PET_00 =", 1-p_00, "\n")
	cat("PET_01 =", 1-p_01, "\n")
	cat("PET_10 =", 1-p_10, "\n")
	cat("PET_11 =", 1-p_11, "\n")
	cat("E00 =", 19 + (33-19) * p_00, "\n")
	cat("E01 =", 19 + (33-19) * p_01, "\n")
	cat("E10 =", 19 + (33-19) * p_10, "\n")
	cat("E11 =", 19 + (33-19) * p_11, "\n")
	cat("accrual =", max(19 + (33-19) * c(p_01, p_10)), "\n")
	cat("\n")
}

#do the final stage probs add up to 1?
for (phi in c(0.01, 0.1, 1, 10, 100)) {
	cat("phi =", phi, "\n")
	#type 1 error, power
	prob_00 = 0
	prob_01 = 0
	prob_10 = 0
	prob_11 = 0
	for(i in 0:33) {
		for(j in 0:33) {
			prob_00 = prob_00 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_01 = prob_01 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_10 = prob_10 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_11 = prob_11 + pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
		}
	}
	cat("prob_00 =", prob_00, "\n")
	cat("prob_01 =", prob_01, "\n")
	cat("prob_10 =", prob_10, "\n")
	cat("prob_11 =", prob_11, "\n")
}

#lee and liu example 1
#test of lower and upper cutoffs at intermediate stages
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
pxn(x=0, n=lee_liu_n[1:3], p=0.2, cx=list(x_fail = 0:2, x_pass = rep(NA, 3)))
pxn(x=3, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = c(rep(NA, 10), 11)))
pxn(x=36, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = c(rep(NA, 10), 11)))
pxn(x=35, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = c(rep(NA, 10), 11)))
pxn(x=34, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = c(rep(NA, 10), 11)))

pxn(x=36, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n))
pxn(x=35, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n))
pxn(x=34, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n))

pxn(x=36, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n-1))
pxn(x=35, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n-1))
pxn(x=34, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n-1))

sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))
# [1] 0.08777926
sum(vapply(X = 0:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))
# [1] 1

sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n)))
sum(vapply(X = 0:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n)))

sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n-1)))
sum(vapply(X = 0:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=list(x_fail = 0:10, x_pass = lee_liu_n-1)))

#test of lower and upper cutoffs at intermediate stages dependent case

#example from page 1379 of bryant and day using dirichlet multinomial
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
cxNA1 = list(x_fail = NA, x_pass = NA)

pxn_RT_dm(xR=13, xT=23, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT_dm(xR=13, xT=23, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)

#type 1 error, power

#non-indep
for (phi in c(0.01, 0.1, 1, 10, 100)) {
	cat("phi =", phi, "\n")
	#type 1 error, power
	alpha_00 = 0
	alpha_01 = 0
	alpha_10 = 0
	alpha_11 = 0
	for(i in 13:33) {
		for(j in 23:33) {
			alpha_00 = alpha_00 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_01 = alpha_01 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_10 = alpha_10 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			alpha_11 = alpha_11 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
		}
	}
	cat("alpha_00 =", alpha_00, "\n")
	cat("alpha_01 =", alpha_01, "\n")
	cat("alpha_10 =", alpha_10, "\n")
	cat("alpha_11 =", alpha_11, "\n")
	cat("beta =", 1-alpha_11, "\n")

	#accrual
	p_00 = 0
	p_01 = 0
	p_10 = 0
	p_11 = 0
	for(i in 6:19) {
		for(j in 12:19) {
			p_00 = p_00 + pxn_RT_dm(xR=i, xT=j, n=19, pR=0.3, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_01 = p_01 + pxn_RT_dm(xR=i, xT=j, n=19, pR=0.3, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_10 = p_10 + pxn_RT_dm(xR=i, xT=j, n=19, pR=0.5, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
			p_11 = p_11 + pxn_RT_dm(xR=i, xT=j, n=19, pR=0.5, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
		}
	}
	cat("PET_00 =", 1-p_00, "\n")
	cat("PET_01 =", 1-p_01, "\n")
	cat("PET_10 =", 1-p_10, "\n")
	cat("PET_11 =", 1-p_11, "\n")
	cat("E00 =", 19 + (33-19) * p_00, "\n")
	cat("E01 =", 19 + (33-19) * p_01, "\n")
	cat("E10 =", 19 + (33-19) * p_10, "\n")
	cat("E11 =", 19 + (33-19) * p_11, "\n")
	cat("accrual =", max(19 + (33-19) * c(p_01, p_10)), "\n")
	cat("\n")
}

#do the final stage probs add up to 1?
for (phi in c(0.01, 0.1, 1, 10, 100)) {
	cat("phi =", phi, "\n")
	#type 1 error, power
	prob_00 = 0
	prob_01 = 0
	prob_10 = 0
	prob_11 = 0
	for(i in 0:33) {
		for(j in 0:33) {
			prob_00 = prob_00 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_01 = prob_01 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_10 = prob_10 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.6, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
			prob_11 = prob_11 + pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd, phi=phi)
		}
	}
	cat("prob_00 =", prob_00, "\n")
	cat("prob_01 =", prob_01, "\n")
	cat("prob_10 =", prob_10, "\n")
	cat("prob_11 =", prob_11, "\n")
}

#why don't they add up to 1?
cum_p = 0
for(i in 0:33) {
	for(j in 0:33) {
		p = pxn_RT_dm(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:33) {
	for(j in 0:33) {
		p = pxn_RT_OR(xR=i, xT=j, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

pxn_RT_dm(xR=33, xT=33, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# [1] 4.13862e-05
pxn_RT_dm(xR=32, xT=33, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# [1] 4.204097e-07
pxn_RT_dm(xR=33, xT=32, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# [1] 1.201171e-07
pxn_RT_dm(xR=32, xT=32, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# [1] 6.912504e-07

multi_MD(x=c(0, 0, 0, 33), a=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
# [1] 4.13862e-05
multi_MD(x=c(0, 0, 1, 32), a=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
# [1] 2.003537e-05
multi_MD(x=c(0, 1, 0, 32), a=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
# [1] 7.012381e-05
multi_MD(x=c(1, 0, 0, 32), a=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
# [1] 4.674921e-05

pxn_RT_OR(xR=33, xT=33, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT_OR(xR=33, xT=32, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT_OR(xR=32, xT=33, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
pxn_RT_OR(xR=32, xT=32, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)

dmultinom(x=c(0, 0, 0, 33), prob=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
dmultinom(x=c(0, 0, 1, 32), prob=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
dmultinom(x=c(0, 1, 0, 32), prob=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))
dmultinom(x=c(1, 0, 0, 32), prob=4*c((0.7)*(0.4), (0.7)*(0.6), (0.3)*(0.4), (0.3)*(0.6)))

#simpler test case
cum_p = 0
for(i in 0:2) {
	for(j in 0:2) {
		p = pxn_RT_dm(xR=i, xT=j, n=c(1,2), pR=0.3, pT=0.6, cxR=list(x_fail = c(0,1), x_pass = c(NA,2)), cxT=list(x_fail = c(0,1), x_pass = c(NA,2)))
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:2) {
	for(j in 0:2) {
		p = pxn_RT_OR(xR=i, xT=j, n=c(1,2), pR=0.3, pT=0.6, cxR=list(x_fail = c(0,1), x_pass = c(NA,2)), cxT=list(x_fail = c(0,1), x_pass = c(NA,2)))
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

#univariate DM
sum(vapply(X = 11:36, FUN = pxn_dm, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))
sum(vapply(X = 0:36, FUN = pxn_dm, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))

#does a 2-stage with no cutoff produce the same result as a 1-stage?
##indep
cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT(xR=i, xT=j, n=c(4), pR=0.3, pT=0.6, cxR=list(x_fail = c(1), x_pass = c(2)), cxT=list(x_fail = c(1), x_pass = c(2)))
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(NA,1), x_pass = c(NA,2)), cxT=list(x_fail = c(NA,1), x_pass = c(NA,2)))
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(-1,1), x_pass = c(NA,2)), cxT=list(x_fail = c(-1,1), x_pass = c(NA,2)))
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

##dependent
cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_OR(xR=i, xT=j, n=c(4), pR=0.3, pT=0.6, cxR=list(x_fail = c(1), x_pass = c(2)), cxT=list(x_fail = c(1), x_pass = c(2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_OR(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(NA,1), x_pass = c(NA,2)), cxT=list(x_fail = c(NA,1), x_pass = c(NA,2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_OR(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(-1,1), x_pass = c(NA,2)), cxT=list(x_fail = c(-1,1), x_pass = c(NA,2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

##DM
cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_dm2(xR=i, xT=j, n=c(4), pR=0.3, pT=0.6, cxR=list(x_fail = c(1), x_pass = c(2)), cxT=list(x_fail = c(1), x_pass = c(2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_dm2(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(NA,1), x_pass = c(NA,2)), cxT=list(x_fail = c(NA,1), x_pass = c(NA,2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

cum_p = 0
for(i in 0:4) {
	for(j in 0:4) {
		p = pxn_RT_dm2(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(-1,1), x_pass = c(NA,2)), cxT=list(x_fail = c(-1,1), x_pass = c(NA,2)), phi = 10)
		cum_p = cum_p + p
		cat("i", i, "j", j, "p", p, "cum_p", cum_p, "\n", sep = "\t")
	}
}

#univariate
##univariate indep
cum_p = 0
for(i in 0:4) {
	p = pxn(x=i, n=c(4), p=0.3, cx=list(x_fail = c(1), x_pass = c(2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

cum_p = 0
for(i in 0:4) {
	p = pxn(x=i, n=c(2,4), p=0.3, cx=list(x_fail = c(NA,1), x_pass = c(NA,2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

cum_p = 0
for(i in 0:4) {
	p = pxn(x=i, n=c(2,4), p=0.3, cx=list(x_fail = c(-1,1), x_pass = c(NA,2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

##univariate DM
cum_p = 0
for(i in 0:4) {
	p = pxn_dm(x=i, n=c(4), p=0.3, cx=list(x_fail = c(1), x_pass = c(2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

cum_p = 0
for(i in 0:4) {
	p = pxn_dm(x=i, n=c(2,4), p=0.3, cx=list(x_fail = c(NA,1), x_pass = c(NA,2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

cum_p = 0
for(i in 0:4) {
	p = pxn_dm(x=i, n=c(2,4), p=0.3, cx=list(x_fail = c(-1,1), x_pass = c(NA,2)))
	cum_p = cum_p + p
	cat("i", i, "p", p, "cum_p", cum_p, "\n", sep = "\t")
}

#see how T1E changes with fewer interim analyses
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.2, cx=cx_ex1))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-1], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-1], x_pass = cx_ex1$x_pass[-1])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:2)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:2)], x_pass = cx_ex1$x_pass[-(1:2)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:3)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:3)], x_pass = cx_ex1$x_pass[-(1:3)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:4)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:4)], x_pass = cx_ex1$x_pass[-(1:4)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:5)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:5)], x_pass = cx_ex1$x_pass[-(1:5)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:6)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:6)], x_pass = cx_ex1$x_pass[-(1:6)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:7)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:7)], x_pass = cx_ex1$x_pass[-(1:7)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:8)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:8)], x_pass = cx_ex1$x_pass[-(1:8)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:9)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:9)], x_pass = cx_ex1$x_pass[-(1:9)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:10)], p=0.2, cx=list(x_fail = cx_ex1$x_fail[-(1:10)], x_pass = cx_ex1$x_pass[-(1:10)])))

#see how power changes with fewer interim analyses
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n, p=0.4, cx=cx_ex1))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-1], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-1], x_pass = cx_ex1$x_pass[-1])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:2)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:2)], x_pass = cx_ex1$x_pass[-(1:2)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:3)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:3)], x_pass = cx_ex1$x_pass[-(1:3)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:4)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:4)], x_pass = cx_ex1$x_pass[-(1:4)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:5)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:5)], x_pass = cx_ex1$x_pass[-(1:5)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:6)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:6)], x_pass = cx_ex1$x_pass[-(1:6)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:7)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:7)], x_pass = cx_ex1$x_pass[-(1:7)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:8)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:8)], x_pass = cx_ex1$x_pass[-(1:8)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:9)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:9)], x_pass = cx_ex1$x_pass[-(1:9)])))
sum(vapply(X = 11:36, FUN = pxn, FUN.VALUE = 1, n=lee_liu_n[-(1:10)], p=0.4, cx=list(x_fail = cx_ex1$x_fail[-(1:10)], x_pass = cx_ex1$x_pass[-(1:10)])))

#check functions for T1E, power, PET, E
#response only
profvis({
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
p_success(n=lee_liu_n, p=0.2, cx=cx_ex1) #T1E
1-p_success(n=lee_liu_n, p=0.4, cx=cx_ex1) #T2E
})

pet(n=lee_liu_n, p=0.2, cx=cx_ex1)

EN(n=lee_liu_n, p=0.2, cx=cx_ex1)

profvis({
for(i in 1:1000){
	pet(n=lee_liu_n, p=0.2, cx=cx_ex1)
}
})

profvis({
lee_liu_n_1 = lee_liu_n[-11]
cx_ex1_1 = lapply(cx_ex1, "[", -11)
for(i in 1:1000){
	1-p_success(n=lee_liu_n_1, p=0.2, cx=cx_ex1_1)
}
})

profvis({
cx_ex1_2 = list(x_fail = c(cx_ex1_1$x_fail, 35), x_pass = rep(NA, 11))
for(i in 1:1000){
	1-p_success(n=lee_liu_n, p=0.2, cx=cx_ex1_2)
}
})

#indep
profvis({
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))

max(p_success_RT(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd),
	p_success_RT(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd))
p_success_RT(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)
})

#dependent
profvis({
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))

p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)
p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)
p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)

p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)

p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
})

# ptm <- proc.time(); p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10); proc.time() - ptm

#Dirichlet-multinomial
profvis({
# pxn_RT_dm2(xR=i, xT=j, n=c(2,4), pR=0.3, pT=0.6, cxR=list(x_fail = c(-1,1), x_pass = c(NA,2)), cxT=list(x_fail = c(-1,1), x_pass = c(NA,2)), phi = 10)
p_success_RT_dm(n=c(8,16), pR=0.3, pT=0.6, cxR=list(x_fail = c(1,2), x_pass = c(NA,2)), cxT=list(x_fail = c(1,2), x_pass = c(NA,2)), phi = 10, lambda = 100)
})
profvis({
for(phi in 10^(-2:2)){
	for(lambda in 10^(1:4)){
		a10 = p_success_RT_dm(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = phi, lambda = lambda)
		a01 = p_success_RT_dm(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = phi, lambda = lambda)
		a11 = p_success_RT_dm(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = phi, lambda = lambda)
		cat(phi, lambda, a10, a01, a11, "\n")
	}
}
})
#Dirichlet-multinomial response only
profvis({
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
for(lambda in 10^(1:4)){
	a0 = p_success_dm(n=lee_liu_n, p=0.2, cx=cx_ex1, lambda = lambda) #T1E
	beta = 1-p_success_dm(n=lee_liu_n, p=0.4, cx=cx_ex1, lambda = lambda) #T2E
	cat(lambda, a0, beta, "\n")
}
})

#dirichlet-multinomial PET
for (phi in 10^(-2:2)){
	cat("phi =", phi, "\n")
	for (pR in c(0.3, 0.5)){
		for (pT in c(0.6, 0.8)){
			pet = pet_RT_dm(n=n_bd, pR=pR, pT=pT, cxR=cxR_bd, cxT=cxT_bd, phi = phi, lambda=4)
			cat("pR =", pR, "pT =", pT, "pet =", pet, "\n")
		}
	}
	cat("\n")
}

# pR=0.3
# pT=0.6
# phi=1
# cxNA2 = list(x_fail = c(-1, -1), x_pass = c(NA, 33))

# pet_RT_dm(n=n_bd, pR=pR, pT=pT, cxR=cxR_bd, cxT=cxNA2, phi = phi, lambda=4)
# pet_dm(n=n_bd, p=pR, cx=cxR_bd, lambda=2)
for(lambda in 10^(1:6)){
	cat("lambda =", lambda, "pet =", pet_dm(n=n_bd, p=0.3, cx=cxR_bd, lambda=lambda), "\n")
}
pet(n=n_bd, p=0.3, cx=cxR_bd)

#dirichlet-multinomial Expected accrual
source("BayesMultiStage.R")
# phi=1
# pR=0.3
# pT=0.6
for (phi in 10^(-2:2)){
	cat("phi =", phi, "\n")
	for (pR in c(0.3, 0.5)){
		for (pT in c(0.6, 0.8)){
			EN = EN_RT_dm(n=n_bd, pR=pR, pT=pT, cxR=cxR_bd, cxT=cxT_bd, phi = phi, lambda=4)
			cat("pR =", pR, "pT =", pT, "EN =", EN, "\n")
		}
	}
	cat("\n")
}

#dirichlet-multinomial 1-dimensional Expected accrual
EN_dm(n=n_bd, p=0.3, cx=cxR_bd, lambda=2)
33 - 14*pet_dm(n=n_bd, p=0.3, cx=cxR_bd, lambda=2)

for(lambda in 10^(1:6)){
	en = EN_dm(n=n_bd, p=0.3, cx=cxR_bd, lambda=lambda)
	cat("lambda =", lambda, "EN =", en, "\n")
}
EN(n=n_bd, p=0.3, cx=cxR_bd)

#all trial stats in one function
source("BayesMultiStage.R")
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
trial_perf(n = lee_liu_n, p0=0.2, p1=0.4, cx = cx_ex1)
profvis({
	for(i in 1:1000){
		trial_perf(n = lee_liu_n, p0=0.2, p1=0.4, cx = cx_ex1)
	}
})

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))

trial_perf_RT(n = n_bd, pR0 = 0.3, pR1 = 0.5, pT0 = 0.6, pT1 = 0.8, cxR = cxR_bd, cxT = cxT_bd)

for(phi in 10^(-2:2)){
	cat(trial_perf_RT_OR(n = n_bd, pR0 = 0.3, pR1 = 0.5, pT0 = 0.6, pT1 = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = phi), "\n")
}

for(phi in 10^(-2:2)){
	cat(phi, "\n")
	for(lambda in c(4,10^(1:4))){
		cat(lambda, trial_perf_RT_dm(n = n_bd, pR0 = 0.3, pR1 = 0.5, pT0 = 0.6, pT1 = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = phi, lambda = lambda), "\n")
	}
}

for(lambda in c(2,10^(1:4))){
	cat(lambda, trial_perf_dm(n = lee_liu_n, p0=0.2, p1=0.4, cx = cx_ex1, lambda = lambda), "\n")
}

#test use of NA in cutoffs
source("BayesMultiStage.R")
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)

cx_ex1.1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=1:36)
pxn(x=10, n=1:36, p=0.2, cx=cx_ex1.1)
pxn(x=10, n=lee_liu_n, p=0.2, cx=cx_ex1)
p_success(n=1:36, p=0.2, cx=cx_ex1.1)
p_success(n=lee_liu_n, p=0.2, cx=cx_ex1)
pet(n=1:36, p=0.2, cx=cx_ex1.1)
pet(n=lee_liu_n, p=0.2, cx=cx_ex1)
EN(n=1:36, p=0.2, cx=cx_ex1.1)
EN(n=lee_liu_n, p=0.2, cx=cx_ex1)

pxn_dm(x=10, n=1:36, p=0.2, cx=cx_ex1.1)
pxn_dm(x=10, n=lee_liu_n, p=0.2, cx=cx_ex1)
p_success_dm(n=1:36, p=0.2, cx=cx_ex1.1)
p_success_dm(n=lee_liu_n, p=0.2, cx=cx_ex1)
pet_dm(n=1:36, p=0.2, cx=cx_ex1.1)
pet_dm(n=lee_liu_n, p=0.2, cx=cx_ex1)
EN_dm(n=1:36, p=0.2, cx=cx_ex1.1)
EN_dm(n=lee_liu_n, p=0.2, cx=cx_ex1)

#T and R
source("BayesMultiStage.R")
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))

# n_bdna = c(19, 26, 33)
n_bdna = c(10, 19, 33)
# cxR_bdna = list(x_fail = c(5, NA, 12), x_pass = rep(NA, 3))
cxR_bdna = list(x_fail = c(NA, 5, 12), x_pass = rep(NA, 3))
# cxT_bdna = list(x_fail = c(11, NA, 22), x_pass = rep(NA, 3))
cxT_bdna = list(x_fail = c(NA, 11, 22), x_pass = rep(NA, 3))

pxn_RT(xR=5, xT=10, n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pxn_RT(xR=5, xT=10, n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
p_success_RT(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
p_success_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
pet_RT(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pet_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
EN_RT(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
EN_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)

pxn_RT_OR(xR=5, xT=10, n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pxn_RT_OR(xR=5, xT=10, n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
p_success_RT_OR(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
pet_RT_OR(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
EN_RT_OR(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)

pxn_RT_dm(xR=5, xT=10, n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pxn_RT_dm(xR=5, xT=10, n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
p_success_RT_dm(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
p_success_RT_dm(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
pet_RT_dm(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
pet_RT_dm(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
EN_RT_dm(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)
EN_RT_dm(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)

#profile of grid search
source("BayesMultiStage.R")
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
# profvis({
	# alpha = 0.1
	# beta = 0.1
	# t_grid = expand.grid(theta_L = 0:100/100, theta_T = 1:99/100)
	# cx_grid = mapply(FUN = calc_cutoffs, t_grid$theta_L, t_grid$theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))
	# cx_dup = duplicated(t(cx_grid))
	# t_grid_nodup = t_grid[!cx_dup,]
	# cx_grid_nodup = cx_grid[,!cx_dup]
	# T1E = vapply(X = 1:ncol(cx_grid_nodup), FUN = function(idx){p_success(n = lee_liu_n, p = 0.2, cx = cx_grid_nodup[,idx])}, FUN.VALUE = 1.)
	# t_grid_nodup_alpha = t_grid_nodup[T1E < alpha,]
	# cx_grid_nodup_alpha = cx_grid_nodup[,T1E < alpha]
	# power = vapply(X = 1:ncol(cx_grid_nodup_alpha), FUN = function(idx){p_success(n = lee_liu_n, p = 0.4, cx = cx_grid_nodup_alpha[,idx])}, FUN.VALUE = 1.)
	# t_grid_nodup_alpha[1-power < beta,]
# })

#more efficient version
source("BayesMultiStage.R")
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)

profvis({
	alpha = 0.1
	beta = 0.1
	# t_grid = expand.grid(theta_L = 0:100/100, theta_T = 1:99/100)
	# cx_grid = mapply(FUN = calc_cutoffs, t_grid$theta_L, t_grid$theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))
	N_max = max(lee_liu_n)
	# m = N_max - lee_liu_n[1]
	# m_grid = expand.grid(i=0:m, x=0:lee_liu_n[1])
	# m_grid = expand.grid(i=0:m, x=0:lee_liu_n[1]) #l and m as well
	# theta_T = Bi(i=0:m, x=0, p0=0.2, a0=0.2, b0=0.8, N_max = N_max)
	# matrix(mapply(FUN = Bi, m_grid$i, m_grid$x, MoreArgs = list(p0=0.2, a0=0.2, b0=0.8, N_max = N_max)), nrow = 27)
	# for(l in 1:11){for(x in 0:lee_liu_n[l]){m = N_max - lee_liu_n[l]; cat(Bi(i=0:m, x=x, p0=0.2, a0=0.2, b0=0.8, N_max = N_max), "\n")}; cat("\n")}
	
	# theta_L = rev(cumsum(rev(P_Yi(i=0:m, x=0, lee_liu_n[1], a0=0.2, b0=0.8, N_max = N_max))))
	# for(l in 1:11){for(x in 0:lee_liu_n[l]){m = N_max - lee_liu_n[l]; cat(rev(cumsum(rev(P_Yi(i=0:m, x=x, lee_liu_n[l], a0=0.2, b0=0.8, N_max = N_max)))), "\n")}; cat("\n")}
	# calc_cutoffs(theta_L = theta_L[2], theta_T = theta_T[2], theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4)
	# mapply(FUN = calc_cutoffs, theta_L, theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))[1,]
	
	# m = N_max - lee_liu_n
	m_x_list = lapply(X = 1:length(lee_liu_n), FUN = function(l, n, N_max){
		nl = n[l]
		m = N_max - nl
		x = 0:nl
		data.frame(m, x, nl, l)
	}, n = lee_liu_n, N_max = max(lee_liu_n))
	
	m_x = do.call(rbind, m_x_list)

	theta_T = unlist(mapply(FUN = function(m,...){Bi(i=0:m, ...)}, m_x$m, m_x$x, MoreArgs = list(p0=0.2, a0=0.2, b0=0.8, N_max = N_max)))
	# theta_L = unlist(mapply(FUN = function(m,...){rev(cumsum(rev(P_Yi(i=0:m, ...))))}, m_x$m, m_x$x, m_x$nl, MoreArgs = list(p0=0.2, a0=0.2, b0=0.8, N_max = N_max)))

	theta_L = unlist(mapply(FUN = function(m, x, nl, a0, b0){pbbinom(0:m-1, size = m, alpha = a0 + x, beta = b0 + nl - x, lower.tail = F)}, m_x$m, m_x$x, m_x$nl, MoreArgs = list(a0=0.2, b0=0.8)))
	# theta_L3 = unlist(mapply(FUN = function(m, x, nl, a0, b0){pbbinom(0:m, size = m, alpha = a0 + x, beta = b0 + nl - x, lower.tail = F)}, m_x$m, m_x$x, m_x$nl, MoreArgs = list(a0=0.2, b0=0.8)))
	
	x_exp = rep(m_x$x, times = m_x$m+1)
	l_exp = rep(m_x$l, times = m_x$m+1)
	
	
	theta_x_l = data.frame(theta_T = theta_T, theta_L = theta_L, x_exp, l_exp)
	# theta_x_l = data.frame(theta_T = format(theta_T, digits = 20, nsmall=20, scientific = F), theta_L = format(theta_L, digits = 20, nsmall=20, scientific = F), x_exp, l_exp)
	data.frame(theta_x_l, select = theta_T == theta_T[1] & theta_L > theta_L[1])

	cbind(
		theta_T[which(theta_T == theta_T[1] & theta_L > theta_L[1])],
		theta_L[which(theta_T == theta_T[1] & theta_L > theta_L[1])],
		x_exp[which(theta_T == theta_T[1] & theta_L > theta_L[1])],
		l_exp[which(theta_T == theta_T[1] & theta_L > theta_L[1])]
	)
	
	#which set of cutoffs is correct?
	for(l in 1:length(lee_liu_n)){
		nl = lee_liu_n[l]
		for(x in 0:nl){
			## m = N_max - nl
			## sumpost = sum(Bi(i=0:m, x=x, p0=0.2, a0=0.2, b0=0.8, N_max=N_max) >= theta_T[12])
			## pbb = pbbinom(m - sumpost, size = N_max-nl, alpha = 0.2 + x, beta = 0.8 + nl - x, lower.tail = F)
			predprob = PP(theta_T = theta_T[12], x=x, n = nl, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
			predprob2 = PP2(theta_T = theta_T[12], x=x, n = nl, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
			cat(l, x, predprob, predprob2, predprob > theta_L[12], "\n")
			
			# predprob = PP(theta_T = 0.887, x=x, n = nl, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
			# cat(l, x, predprob, predprob > 0.001, "\n")
			
			## m = N_max - nl
			## sumpost = sum(Bi(i=0:m, x=x, p0=0.2, a0=0.2, b0=0.8, N_max=N_max) >= theta_T[1])
			## pbb = pbbinom(m - sumpost, size = N_max-nl, alpha = 0.2 + x, beta = 0.8 + nl - x, lower.tail = F)
			# predprob = PP(theta_T = theta_T[1], x=x, n = nl, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
			# predprob2 = PP2(theta_T = theta_T[1], x=x, n = nl, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
			# cat(l, x, format(predprob, nsmall=20), format(predprob2, nsmall=20), predprob > theta_L[1], "\n")
		}
		cat("\n")
	}
	
	# PP(theta_T = theta_T[12], x=5, n = 10, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
	# pbbinom(5, size = N_max-10, alpha = 0.2 + 5, beta = 0.8 + 10 - 5, lower.tail = F, log.p = FALSE)

	# PP(theta_T = theta_T[12], x=6, n = 10, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
	# pbbinom(4, size = N_max-10, alpha = 0.2 + 6, beta = 0.8 + 10 - 6, lower.tail = F, log.p = FALSE)

	# PP(theta_T = theta_T[12], x=6, n = 17, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
	# pbbinom(4, size = N_max-17, alpha = 0.2 + 6, beta = 0.8 + 17 - 6, lower.tail = F, log.p = FALSE)

	# PP(theta_T = theta_T[12], x=7, n = 17, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
	# pbbinom(3, size = N_max-17, alpha = 0.2 + 7, beta = 0.8 + 17 - 7, lower.tail = F, log.p = FALSE)

	# PP(theta_T = theta_T[12], x=10, n = 17, p0=0.2, a0=0.2, b0=0.8, N_max=N_max)
	# pbbinom(0, size = N_max-17, alpha = 0.2 + 10, beta = 0.8 + 17 - 10, lower.tail = F, log.p = FALSE)

	# dbbinom(i, m , alpha = a0 + x, beta = b0 + n - x)


	
	#should return at this point and start a new function
	theta = unique(data.frame(theta_T, theta_L))
	theta = theta[theta$theta_L < 1,]
	# theta = unique(data.frame(theta_T, theta_L2))
	#return(theta)
	
	theta_x_l_12 = theta_x_l[which(theta_T == theta$theta_T[12]),]
	theta_x_l_12_s = theta_x_l_12[order(theta_x_l_12$theta_L),]
	theta_x_l_12_s
	theta_x_l_12_cx = rep(NA, 11)
	for (i in 1:nrow(theta_x_l_12_s)){
		theta_x_l_12_cx[theta_x_l_12_s[i,]$l_exp] = theta_x_l_12_s[i,]$x_exp
		cat(theta_T[12], theta_x_l_12_s[i,]$theta_L, theta_x_l_12_cx, "\n")
	}
	
	#for each posterior = theta_T and each l, choose the smallest x that satisfies PP > theta_L and subtract 1
	for(i in 1:nrow(theta)){
		for(l in 1:length(lee_liu_n)){
			x_pass = x_exp[which(l_exp == l & theta_T == theta$theta_T[i] & theta_L > theta$theta_L[i])]
			nl = lee_liu_n[l]
			cat(c(x_pass-1,nl)[1], "")
			# cat(x_exp[which(l_exp == l & theta_T == theta$theta_T[i] & theta_L > theta$theta_L[i])[1]]-1, "")
		}
		cat("\n")
	}
	
	calc_cutoff_grid(n = lee_liu_n, theta_T = theta_T[13], theta_L = theta_L[13], theta_T_exp = theta_T, theta_L_exp = theta_L, x_exp = x_exp, l_exp = l_exp)
	
	grid_cutoffs = mapply(FUN = calc_cutoff_grid, theta$theta_T, theta$theta_L, MoreArgs = list(n = lee_liu_n, theta_T_exp = theta_T, theta_L_exp = theta_L, x_exp = x_exp, l_exp = l_exp))
	
	#change the name of this var
	cx_grid = mapply(FUN = calc_cutoffs, theta$theta_L, theta$theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))
	# cx_grid = mapply(FUN = calc_cutoffs, theta$theta_L2, theta$theta_T, MoreArgs = list(theta_U = 1, n = lee_liu_n, p0 = 0.2, p1 = 0.4))
	
	# theta_T = Bi(i=0:m, x=0, p0=0.2, a0=0.2, b0=0.8, N_max = N_max)
	# theta_L = rev(cumsum(rev(P_Yi(i=0:m, x=0, nl, a0=0.2, b0=0.8, N_max = N_max))))
	# return(cbind(theta_T, theta_L))
	
	cx_dup = duplicated(t(cx_grid))
	theta_nodup = theta[!cx_dup,]
	cx_grid_nodup = cx_grid[,!cx_dup]
	#fails due to NA in last term
	T1E = vapply(X = 1:ncol(cx_grid_nodup), FUN = function(idx){p_success(n = lee_liu_n, p = 0.2, cx = cx_grid_nodup[,idx])}, FUN.VALUE = 1.)
	T1E_nodup_alpha = T1E[T1E < alpha]
	theta_nodup_alpha = theta_nodup[T1E < alpha,]
	cx_grid_nodup_alpha = cx_grid_nodup[,T1E < alpha]
	power = vapply(X = 1:ncol(cx_grid_nodup_alpha), FUN = function(idx){p_success(n = lee_liu_n, p = 0.4, cx = cx_grid_nodup_alpha[,idx])}, FUN.VALUE = 1.)
	cx_grid_nodup_alpha_power = cx_grid_nodup_alpha[,1-power < beta]
	
	exp_accrual = vapply(X = 1:ncol(cx_grid_nodup_alpha_power), FUN = function(idx){EN(n = lee_liu_n, p = 0.2, cx = cx_grid_nodup_alpha_power[,idx])}, FUN.VALUE = 1.)
	
	cx_grid_nodup_alpha_power[1,]
	cbind(theta_nodup_alpha[1-power < beta,], T1E_nodup_alpha[1-power < beta], power[1-power < beta], exp_accrual)
})

source("BayesMultiStage.R")
lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
txl2 = calc_theta_grid(n = lee_liu_n, p0=0.2)

all.equal(theta_x_l ,txl2)

feasible_n(n=lee_liu_n, alpha=0.1, beta=0.1, p0=0.2, p1=0.4)
feasible_n(n=10:36, alpha=0.1, beta=0.1, p0=0.2, p1=0.4)
profvis({feasible_n(n=10:36, alpha=0.1, beta=0.1, p0=0.2, p1=0.4)})

search_n(alpha=0.1, beta=0.1, p0=0.2, p1=0.4)
search_n(alpha=0.1, beta=0.1, p0=0.2, p1=0.4, min_n1 = 10)
alpha=0.1; beta=0.1; p0=0.2; p1=0.4; k = NULL; min_n1 = 1; d = NULL; max_n = 100; a0 = p0; b0 = 1-p0

#easier trial
search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4)
search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, min_n1 = 10)

search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, d=5)
search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, d=5, min_n1 = 10)

search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, k=5)
search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, k=5, min_n1 = 10)

EN(n = c(10,15,16), p=0.2, cx = list(x_fail = c(1,3,4), x_pass = c(NA, NA, 5)))
EN(n = c(5, 10,15,16), p=0.2, cx = list(x_fail = c(NA, 1,3,4), x_pass = c(NA, NA, NA, 5)))
EN(n = c(10,15,19), p=0.2, cx = list(x_fail = c(0,2,5), x_pass = c(NA, NA, 6)))

p_success(n = c(10,15,16), p = 0.2, cx = list(x_fail = c(1,3,4), x_pass = c(NA, NA, 5)))
p_success(n = c(5, 10,15,16), p = 0.2, cx = list(x_fail = c(NA, 1,3,4), x_pass = c(NA, NA, NA, 5)))
p_success(n = c(10,15,19), p = 0.2, cx = list(x_fail = c(0,2,5), x_pass = c(NA, NA, 6)))

feasible_n(n=c(10,15,16), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)
feasible_n(n=c(5, 10,15,16), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)

feasible_n(n=c(10,15,19), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)
feasible_n(n=c(5, 10,15,19), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)

calc_cutoffs(theta_L = 0.01184985, theta_U = 1, theta_T = 0.828691, p0=0.2, p1=0.4, n=c(10,15,16))
calc_cutoffs(theta_L = 0.01184985, theta_U = 1, theta_T = 0.828691, p0=0.2, p1=0.4, n=c(5,10,15,16))
calc_cutoffs(theta_L = 0.005, theta_U = 1, theta_T = 0.828691, p0=0.2, p1=0.4, n=c(5,10,15,16))

calc_theta_grid(n=c(10,15,16), p0=0.2)
calc_theta_grid(n=c(5,10,15,16), p0=0.2)

profvis({feasible_n(n=c(10,15,20,25,30), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)})
profvis({feasible_n2(n=c(10,15,20,25,30), alpha=0.2, beta=0.2, p0=0.2, p1=0.4)})

search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, k=3)
profvis({search_n(alpha=0.2, beta=0.2, p0=0.2, p1=0.4, k=3)})

min_n1=1
max_n=100
# d=5
d=NULL
# k=NULL
k=5
for (nk in max(min_n1+1, 2, na.rm=T):max_n){
	cat(nk, "\n")
	n = calc_n(nk = nk, k = k, min_n1 = min_n1, d = d)
	cat(paste0(n, collapse=","), "\n")
}

k = NULL, min_n1 = 1, d = NULL
#bug catch
p_success(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_dm(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))

pet(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_dm(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))

EN(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_dm(n = lee_liu_n, p = 0.2, cx = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))

cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
p_success_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
p_success_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
p_success_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
p_success_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)

pet_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
pet_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
pet_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
pet_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)

EN_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
EN_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT_OR(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)
EN_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = cx_ex1, cxT = list(x_fail = rep(NA,11), x_pass = rep(NA,11)))
EN_RT_dm(n = lee_liu_n, pR = 0.2, pT = 0.4, cxR = list(x_fail = rep(NA,11), x_pass = rep(NA,11)), cxT = cx_ex1)

#enumerate all possible cutoffs
profvis({
	alpha = 0.1
	beta = 0.1
	lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
	lee_liu_n = c(17, 37)
	k=length(lee_liu_n)
	# k=2
	lee_liu_d = diff(c(0, lee_liu_n))
	for(i in -1:10){for(j in i:17){cat(i,j, "\n")}}
	# lapply(X=-1:10, FUN = function(x){lapply(X=x:17,FUN=function(y){cat(x,y,"\n")})})
	# lapply(X=c(10,17), FUN=seq)
	# lapply(X=c(10,7), FUN=function(x){lapply(X=x:y, FUN=function(){})})
	# do.call(expand.grid,lapply(X=c(10,7), FUN=seq, from=0))
	s1 = lapply(X=lee_liu_d, FUN=seq, from=0)
	s1[[1]] = c(-1,s1[[1]])
	s2 = do.call(expand.grid,s1)
	s3 = t(apply(X=s2, FUN = cumsum, MARGIN=1))
	s4 = split(s3, seq(nrow(s3)))
	cx = lapply(X=s4, FUN=function(x){list(x_fail = x, x_pass = c(rep(NA,k-1),x[k]+1))})
	T1E = vapply(X = cx, FUN = p_success, FUN.VALUE = 1., n = lee_liu_n, p = 0.2) #does not finish in a reasonable amount of time
	T1E_alpha = T1E[T1E < alpha]
	cx_alpha = cx[T1E < alpha]
	power = vapply(X = cx_alpha, FUN = p_success, FUN.VALUE = 1., n = lee_liu_n, p = 0.4)
	cx_alpha_power = cx_alpha[1-power < beta]
	exp_accrual = vapply(X = cx_alpha_power, FUN = EN, FUN.VALUE = 1., n = lee_liu_n, p = 0.2)
	cbind(T1E_alpha[1-power < beta], power[1-power < beta], exp_accrual)
})

#unusual bug (fixed)
cx_test1 = list(x_fail = c(10, 17, 14, 24, 27, 29, 10, 33, 34, 35, NA), x_pass = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
cx_test2 = list(x_fail = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36), x_pass = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
pxn(x=36, n = lee_liu_n, p = 0.2, cx = cx_test1)
p_success(n = lee_liu_n, p = 0.2, cx = cx_test1)
pxn(x=36, n = lee_liu_n, p = 0.2, cx = cx_test2)
p_success(n = lee_liu_n, p = 0.2, cx = cx_test2)

#test comparing cx across different n
source("BayesMultiStage.R")

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
compare_n_cx(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd, cx2 = cxT_bd, compfunc = "<=")
compare_n_cx(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd-1, cx2 = cxT_bd, compfunc = "<=")
compare_n_cx(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd+1, cx2 = cxT_bd, compfunc = "<=")
compare_n_cx(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd-14, cx2 = cxT_bd, compfunc = "<=")
compare_n_cx(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd+14, cx2 = cxT_bd, compfunc = "<=")

n_cx_lte(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd, cx2 = cxT_bd)
n_cx_lte(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd-1, cx2 = cxT_bd)
n_cx_lte(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd+1, cx2 = cxT_bd)
n_cx_lte(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd-14, cx2 = cxT_bd)
n_cx_lte(n1 = n_bd, cx1 = cxR_bd, n2 = n_bd+14, cx2 = cxT_bd)

compare_n_cx_v(n1 = n_bd, cx1 = cxR_bd, n2 = list(n_bd), cx2 = list(cxT_bd), compfunc = "<=")
compare_n_cx_v(n1 = n_bd, cx1 = cxR_bd, n2 = list(n_bd, n_bd-1, n_bd+1, n_bd-14, n_bd+14), cx2 = list(cxT_bd, cxT_bd, cxT_bd, cxT_bd, cxT_bd), compfunc = "<=")

n_cx_lte_v2(n1 = n_bd, cx1 = cxR_bd, n2 = list(n_bd), cx2 = list(cxT_bd))
n_cx_lte_v2(n1 = n_bd, cx1 = cxR_bd, n2 = list(n_bd, n_bd-1, n_bd+1, n_bd-14, n_bd+14), cx2 = list(cxT_bd, cxT_bd, cxT_bd, cxT_bd, cxT_bd))

lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)

compare_n_cx(n1 = n_bd-8, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1, compfunc = "<=")
compare_n_cx(n1 = n_bd-9, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1, compfunc = "<=")
compare_n_cx(n1 = n_bd-10, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1, compfunc = "<=")
compare_n_cx(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-8, cx2 = cxR_bd, compfunc = "<=")
compare_n_cx(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-9, cx2 = cxR_bd, compfunc = "<=")
compare_n_cx(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-10, cx2 = cxR_bd, compfunc = "<=")

n_cx_lte(n1 = n_bd-8, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1)
n_cx_lte(n1 = n_bd-9, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1)
n_cx_lte(n1 = n_bd-10, cx1 = cxR_bd, n2 = lee_liu_n, cx2 = cx_ex1)
n_cx_lte(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-8, cx2 = cxR_bd)
n_cx_lte(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-9, cx2 = cxR_bd)
n_cx_lte(n1 = lee_liu_n, cx1 = cx_ex1, n2 = n_bd-10, cx2 = cxR_bd)
# p_success(n = lee_liu_n, p = 0.2, cx = cx_grid_nodup[,11])
# pxn(x=37, n = lee_liu_n, p = 0.2, cx = cx_grid_nodup[,11])

#test use of environment to reuse matrices
# pxn(x=10, n = lee_liu_n, p=0.2, cx = cx_ex1)
# xn_test = matrix(NA, nrow=36+1, ncol = 11)
# pxn(x=10, n = lee_liu_n, p=0.2, cx = cx_ex1)
# xn_test
# pxn(x=10, n = lee_liu_n, p=0.2, cx = cx_ex1, xmat_nm = "xn_test", env = environment())
# xn_test

#second search algorithm
source("BayesMultiStage.R")
library(profvis)

profvis({
	search_n2(min_n1 = 19, d = 19, max_n = 33, alpha = 0.15, beta = 0.15, p0 = 0.3, p1 = 0.5) #bryant & day example (alternate)
})
search_n2(min_n1 = 19, d = 19, max_n = 33, alpha = 0.15, beta = 0.15, p0 = 0.3, p1 = 0.5) #bryant & day example (alternate)

profvis({
	search_n2(k = 2, min_n1 = 19, max_n = 33, alpha = 0.15, beta = 0.15, p0 = 0.3, p1 = 0.5) #bryant & day example
})
search_n2(k = 2, min_n1 = 19, max_n = 33, alpha = 0.15, beta = 0.15, p0 = 0.3, p1 = 0.5) #bryant & day example

search_n2(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
profvis({
	search_n2(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
})

search_n3(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
profvis({
	search_n3(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
})

search_n3(min_n1 = 10, max_n=50, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
profvis({
	search_n3(min_n1 = 10, max_n=50, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
})

search_n3(min_n1 = 10, max_n=42, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
profvis({
	search_n3(min_n1 = 10, max_n=42, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
})


#still takes too long
# search_n3(min_n1 = 10, k=11, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
# profvis({
	# search_n3(min_n1 = 10, k=11, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
# })

#using matrices for p(x) instead of recursion
source("BayesMultiStage.R")

pxn_mat(n = c(2,4,6), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3))) -
vapply(X = 0:6, FUN = pxn, FUN.VALUE = 1, n = c(2,4,6), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))

pxn_mat(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3))) -
vapply(X = 0:21, FUN = pxn, FUN.VALUE = 1, n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))

pxn_mat(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(5, 7, 8), x_pass = c(NA, NA, 9))) -
vapply(X = 0:21, FUN = pxn, FUN.VALUE = 1, n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(5, 7, 8), x_pass = c(NA, NA, 9)))

pxn_mat(n = c(2,4,6), p = 0.5, cx = list(x_fail = c(NA, NA, NA), x_pass = c(NA, NA, 3))) -
vapply(X = 0:6, FUN = pxn, FUN.VALUE = 1, n = c(2,4,6), p = 0.5, cx = list(x_fail = c(NA, NA, NA), x_pass = c(NA, NA, 3)))

lee_liu_n = c(10, 17, 21, 24, 27, 29, 31, 33, 34, 35, 36)
cx_ex1 = calc_cutoffs(theta_L = 0.001, theta_U = 1, theta_T = 0.887, p0=0.2, p1=0.4, a0=0.2, b0=0.8, n=lee_liu_n)
pxn_mat(n = lee_liu_n, p = 0.2, cx = cx_ex1) -
vapply(X = 0:36, FUN = pxn, FUN.VALUE = 1, n = lee_liu_n, p = 0.2, cx = cx_ex1)

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
pxn_mat(n = n_bd, p = 0.3, cx = cxR_bd) -
vapply(X = 0:33, FUN = pxn, FUN.VALUE = 1, n = n_bd, p = 0.3, cx = cxR_bd)
pxn_mat(n = n_bd, p = 0.5, cx = cxT_bd) -
vapply(X = 0:33, FUN = pxn, FUN.VALUE = 1, n = n_bd, p = 0.5, cx = cxT_bd)

p_success_mat(n = c(2,4,6), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))
p_success(n = c(2,4,6), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))

p_success_mat(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))
p_success(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(0, 1, 2), x_pass = c(NA, NA, 3)))

p_success_mat(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(5, 7, 8), x_pass = c(NA, NA, 9)))
p_success(n = c(10, 17, 21), p = 0.5, cx = list(x_fail = c(5, 7, 8), x_pass = c(NA, NA, 9)))

p_success_mat(n = lee_liu_n, p = 0.2, cx = cx_ex1)
p_success(n = lee_liu_n, p = 0.2, cx = cx_ex1)

p_success_mat(n = n_bd, p = 0.3, cx = cxR_bd)
p_success(n = n_bd, p = 0.3, cx = cxR_bd)

p_success_mat(n = n_bd, p = 0.5, cx = cxT_bd)
p_success(n = n_bd, p = 0.5, cx = cxT_bd)

#shiny app for simon 2 stage
source("App.R")
shinyApp(ui, server)

#shiny app for this method
source("BayesMultiStageApp.R")
shinyApp(ui, server)

source("BayesMultiStageApp_RT.R")
shinyApp(ui, server)

#shiny app for 2019 method
library(BayesianPredictiveFutility); Bayesian_Predictive_App()

#plotting changes in theta_L, theta_T
lee_liu_ex1_res = search_n3(min_n1 = 10, max_n = 42, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)
minimax = lee_liu_ex1_res[which(lee_liu_ex1_res$design == "minimax"),]
optimal = lee_liu_ex1_res[which(lee_liu_ex1_res$design == "optimal"),]
minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
minimax_t_L = minimax$theta_L
minimax_t_T = minimax$theta_T

calc_cutoffs(theta_L = minimax_t_L, theta_T = minimax_t_T, p0 = 0.2, p1 = 0.4, n = minimax_n)
# lapply()

#test matrix algorithm for pxn_RT
source("BayesMultiStage.R")

pxn_RT_mat(n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)))

#test matrix algorithm for pxn_RT_OR
pxn_RT_OR_mat(n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)))
pxn_RT_OR_mat(n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)), phi = 0.1)
pxn_RT_OR_mat(n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)), phi = 10)

#old function for comparison
unlist(lapply(X = 0:4, FUN = function(i) {lapply(X = 0:4, FUN = pxn_RT_OR, xR=i, n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)), phi = 1)}))
unlist(lapply(X = 0:4, FUN = function(i) {lapply(X = 0:4, FUN = pxn_RT_OR, xR=i, n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)), phi = .1)}))
unlist(lapply(X = 0:4, FUN = function(i) {lapply(X = 0:4, FUN = pxn_RT_OR, xR=i, n = c(2,4), pR = 0.25, pT = 0.5, cxR = list(x_fail = c(0, 1), x_pass = c(NA, 2)), cxT = list(x_fail = c(1, 2), x_pass = c(NA, 3)), phi = 10)}))

n_rm_test = 1:13
cxR_rm_test = list(x_fail = c(1,2,NA,4,NA,5 ,6,7,7,8,NA,10,10), x_pass = c(rep(NA, 12), 11))
cxT_rm_test = list(x_fail = c(1,2,3, 4,4, NA,5,6,6,7,8, NA,NA), x_pass = c(rep(NA, 12), 11))

rm_stages(n_rm_test, cxR_rm_test)
rm_stages(n_rm_test, cxT_rm_test)
rm_stages_RT(n_rm_test, cxR_rm_test, cxT_rm_test)
n = n_rm_test; cxR = cxR_rm_test; cxT = cxT_rm_test

#Bryant and Day examples (independent)
search_n_RT(min_n1 = 19, d = 19, max_n = 33, alphaR=0.15, alpha=0.15, beta=0.15, pR0=0.3, pR1=0.5, pT0=0.6, pT1 = 0.8)
profvis({
	search_n_RT(max_n = 33, alphaR=0.15, alpha=0.15, beta=0.15, pR0=0.3, pR1=0.5, pT0=0.6, pT1 = 0.8)
})
search_n_RT(min_n1 = 19, k=2, max_n = 33, alphaR=0.15, alpha=0.15, beta=0.15, pR0=0.3, pR1=0.5, pT0=0.6, pT1 = 0.8)

#EN vs EN_mat
EN(n = c(10,15,16), p=0.2, cx = list(x_fail = c(1,3,4), x_pass = c(NA, NA, 5))) -
EN_mat(n = c(10,15,16), p=0.2, cx = list(x_fail = c(1,3,4), x_pass = c(NA, NA, 5)))

EN(n = c(5, 10,15,16), p=0.2, cx = list(x_fail = c(NA, 1,3,4), x_pass = c(NA, NA, NA, 5))) -
EN_mat(n = c(5, 10,15,16), p=0.2, cx = list(x_fail = c(NA, 1,3,4), x_pass = c(NA, NA, NA, 5)))

EN(n = c(10,15,19), p=0.2, cx = list(x_fail = c(0,2,5), x_pass = c(NA, NA, 6))) -
EN_mat(n = c(10,15,19), p=0.2, cx = list(x_fail = c(0,2,5), x_pass = c(NA, NA, 6)))

n_bdna = c(10, 19, 33)
cxR_bdna = list(x_fail = c(NA, 5, 12), x_pass = rep(NA, 3))
cxT_bdna = list(x_fail = c(NA, 11, 22), x_pass = rep(NA, 3))
EN_RT(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna) -
EN_mat_RT(n = n_bdna, pR = 0.3, pT = 0.6, cxR = cxR_bdna, cxT = cxT_bdna)

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
EN_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd) -
EN_mat_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)

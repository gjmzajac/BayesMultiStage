source("BayesMultiStage.R")
#example from page 1379 of bryant and day
# N1 = 19, N2, = 33, CR1 = 5, CR2 = 12, CT1 = 11, CT2 = 22.
# pR0=0.3, pR1=0.5, pT0=0.6, pT1=0.8

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = rep(NA, 2))
cxT_bd = list(x_fail = c(11, 22), x_pass = rep(NA, 2))
cxNA1 = list(x_fail = NA, x_pass = NA)

for(tL in seq(0.0001, 0.1, 0.0001)){
	for(tT in seq(0.1, 0.9, 0.01)){
		cxR_2 = calc_cutoffs(theta_L = tL, theta_T = tT, p0=0.3, p1=0.5, n=n_bd)
		cxT_2 = calc_cutoffs(theta_L = tL, theta_T = tT, p0=0.6, p1=0.8, n=n_bd)
		# if(all(cxR_2$x_fail == cxR_bd$x_fail) || all(cxT_2$x_fail == cxT_bd$x_fail)){
		if(all(cxR_2$x_fail == cxR_bd$x_fail) && all(cxT_2$x_fail == cxT_bd$x_fail)){
		# if(all(cxT_2$x_fail == cxT_bd$x_fail)){
			cat("theta_L =", tL, "theta_T =", tT, "cxR =", cxR_2$x_fail, "cxT =", cxT_2$x_fail, "\n")
		}
	}
}

# theta_L = 0.072; theta_T = 0.85 #cxR = 5 12 cxT = 11 22
theta_L = 0.0717; theta_T = 0.86 #cxR = 5 12 cxT = 11 22
calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_bd)$x_fail
calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_bd)$x_fail

n_3 = round(seq(33/3, 33, by=33/3))
n_3
cxR_3 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_3)
cxR_3$x_fail
cxT_3 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_3)
cxT_3$x_fail

n_4 = round(seq(33/4, 33, by=33/4))
n_4
cxR_4 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_4)
cxR_4$x_fail
cxT_4 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_4)
cxT_4$x_fail

n_5 = round(seq(33/5, 33, by=33/5))
n_5
cxR_5 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_5)
cxR_5$x_fail
cxT_5 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_5)
cxT_5$x_fail

n_6 = round(seq(33/6, 33, by=33/6))
n_6
cxR_6 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_6)
cxR_6$x_fail
cxT_6 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_6)
cxT_6$x_fail

n_7 = round(seq(33/7, 33, by=33/7))
n_7
cxR_7 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_7)
cxR_7$x_fail
cxT_7 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_7)
cxT_7$x_fail

n_8 = round(seq(33/8, 33, by=33/8))
n_8
cxR_8 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_8)
cxR_8$x_fail
cxT_8 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_8)
cxT_8$x_fail

n_9 = round(seq(33/9, 33, by=33/9))
n_9
cxR_9 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_9)
cxR_9$x_fail
cxT_9 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_9)
cxT_9$x_fail

n_10 = round(seq(33/10, 33, by=33/10))
n_10
cxR_10 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.3, p1=0.5, n=n_10)
cxR_10$x_fail
cxT_10 = calc_cutoffs(theta_L = theta_L, theta_T = theta_T, p0=0.6, p1=0.8, n=n_10)
cxT_10$x_fail

#plot
plot(n_bd, cxR_bd$x_fail, xlim=c(0, 33), ylim = c(0,12))
lines(n_3, cxR_3$x_fail)
lines(n_4, cxR_4$x_fail)
lines(n_5, cxR_5$x_fail)
lines(n_6, cxR_6$x_fail)
lines(n_7, cxR_7$x_fail)
lines(n_8, cxR_8$x_fail)
lines(n_9, cxR_9$x_fail)
lines(n_10, cxR_10$x_fail)

plot(n_bd, cxT_bd$x_fail, xlim=c(0, 33), ylim = c(0,22))
lines(n_3, cxT_3$x_fail)
lines(n_4, cxT_4$x_fail)
lines(n_5, cxT_5$x_fail)
lines(n_6, cxT_6$x_fail)
lines(n_7, cxT_7$x_fail)
lines(n_8, cxT_8$x_fail)
lines(n_9, cxT_9$x_fail)
lines(n_10, cxT_10$x_fail)

#power, T1E
n_list = list(NA, n_bd, n_3, n_4, n_5, n_6, n_7, n_8, n_9, n_10)
cxR_list = list(NA, cxR_bd, cxR_3, cxR_4, cxR_5, cxR_6, cxR_7, cxR_8, cxR_9, cxR_10)
cxT_list = list(NA, cxT_bd, cxT_3, cxT_4, cxT_5, cxT_6, cxT_7, cxT_8, cxT_9, cxT_10)
for (k in 2:10){
	n = n_list[[k]]
	cxR = cxR_list[[k]]
	cxT = cxT_list[[k]]
	cat("k =", k, "\n")
	
	# for (phi in c(0.01, 0.1, 1, 10, 100)) {
	for (phi in 1) {
		cat("phi =", phi, "\n")
		#type 1 error, power
		alpha_00 = 0
		alpha_01 = 0
		alpha_10 = 0
		alpha_11 = 0
		for(i in 13:33) {
			for(j in 23:33) {
				alpha_00 = alpha_00 + pxn_RT_OR(xR=i, xT=j, n=n, pR=0.3, pT=0.6, cxR=cxR, cxT=cxT, phi=phi)
				alpha_01 = alpha_01 + pxn_RT_OR(xR=i, xT=j, n=n, pR=0.3, pT=0.8, cxR=cxR, cxT=cxT, phi=phi)
				alpha_10 = alpha_10 + pxn_RT_OR(xR=i, xT=j, n=n, pR=0.5, pT=0.6, cxR=cxR, cxT=cxT, phi=phi)
				alpha_11 = alpha_11 + pxn_RT_OR(xR=i, xT=j, n=n, pR=0.5, pT=0.8, cxR=cxR, cxT=cxT, phi=phi)
			}
		}
		cat("alpha_00 =", alpha_00, "\n")
		cat("alpha_01 =", alpha_01, "\n")
		cat("alpha_10 =", alpha_10, "\n")
		cat("alpha_11 =", alpha_11, "\n")
		cat("beta =", 1-alpha_11, "\n")

		#accrual
		# p_00 = 0
		# p_01 = 0
		# p_10 = 0
		# p_11 = 0
		# for(i in 6:19) {
			# for(j in 12:19) {
				# p_00 = p_00 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.3, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
				# p_01 = p_01 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.3, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
				# p_10 = p_10 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.5, pT=0.6, cxR=cxNA1, cxT=cxNA1, phi=phi)
				# p_11 = p_11 + pxn_RT_OR(xR=i, xT=j, n=19, pR=0.5, pT=0.8, cxR=cxNA1, cxT=cxNA1, phi=phi)
			# }
		# }
		# cat("PET_00 =", 1-p_00, "\n")
		# cat("PET_01 =", 1-p_01, "\n")
		# cat("PET_10 =", 1-p_10, "\n")
		# cat("PET_11 =", 1-p_11, "\n")
		# cat("E00 =", 19 + (33-19) * p_00, "\n")
		# cat("E01 =", 19 + (33-19) * p_01, "\n")
		# cat("E10 =", 19 + (33-19) * p_10, "\n")
		# cat("E11 =", 19 + (33-19) * p_11, "\n")
		# cat("accrual =", max(19 + (33-19) * c(p_01, p_10)), "\n")
		# cat("\n")
	}
}

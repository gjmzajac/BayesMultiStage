source("BayesMultiStage.R")

ex1_d = ""
ex2_d = ""

#example 1
# 𝑝_𝑅0=0.25,	𝑝_𝑅1=0.5
# 𝑝_𝑇0=0.5,		𝑝_𝑇1=0.75
n_ex1 = c(2,4)
cxR_ex1 = list(x_fail = c(0, 1), x_pass = c(NA, 2))
cxT_ex1 = list(x_fail = c(1, 2), x_pass = c(NA, 3))

#T1E
p_success_RT(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
p_success_RT(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
p_success_RT(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)

#power
p_success_RT(n = n_ex1, pR = 0.5, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)

#pet
pet_RT(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
pet_RT(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
pet_RT(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)
pet_RT(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)

#EN
EN_RT(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
EN_RT(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
EN_RT(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)
EN_RT(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1)

#phi=0.01
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 0.01),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 0.01),
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01)
), pet=c(
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01)
), EN=c(
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.01)
))

#phi=0.1
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 0.1),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 0.1),
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1)
), pet=c(
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1)
), EN=c(
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 0.1)
))

#phi=1
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 1),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 1),
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1)
), pet=c(
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1)
), EN=c(
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 1)
))

#phi=10
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 10),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 10),
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10)
), pet=c(
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10)
), EN=c(
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 10)
))

#phi=100
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 100),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, phi = 100),
	p_success_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	p_success_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100)
), pet=c(
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	pet_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	pet_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100)
), EN=c(
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	EN_RT_OR(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100),
	EN_RT_OR(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, phi = 100)
))

#decision matrix probabilities (stage 1)
for(i in 0:2){
	for(j in 0:2){
		# prob = pxn_RT(xR=i, xT=j, n = n_ex1[1], pR = 0.25, pT = 0.5, cxR = lapply(cxR_ex1, FUN = "[", 1), cxT = lapply(cxT_ex1, FUN = "[", 1))
		prob = pxn_RT_OR(xR=i, xT=j, n = n_ex1[1], pR = 0.25, pT = 0.5, cxR = lapply(cxR_ex1, FUN = "[", 1), cxT = lapply(cxT_ex1, FUN = "[", 1))
		cat(i, j, prob, "\n")
	}
}

# matrix(mapply(FUN = pxn_RT, xR=rep(0:2, times=3), xT=rep(0:2, each=3), MoreArgs = list(n = n_ex1[1], pR = 0.25, pT = 0.5, cxR = lapply(cxR_ex1, FUN = "[", 1), cxT = lapply(cxT_ex1, FUN = "[", 1))), ncol=3)
write.table(x=
	matrix(mapply(FUN = pxn_RT_OR, xR=rep(0:2, times=3), xT=rep(0:2, each=3), MoreArgs = list(n = n_ex1[1], pR = 0.25, pT = 0.5, cxR = lapply(cxR_ex1, FUN = "[", 1), cxT = lapply(cxT_ex1, FUN = "[", 1))), ncol=3),
	file = paste0(ex1_d, "stage1_decision_matrix_probs.txt"), col.names=F, row.names=F, quote=F, sep = "\t")
# outer(0:2, 0:2, FUN = pxn_RT, n = n_ex1[1], pR = 0.25, pT = 0.5, cxR = lapply(cxR_ex1, FUN = "[", 1), cxT = lapply(cxT_ex1, FUN = "[", 1))

#decision matrix probabilities (stage 2)
for(i in 0:4){
	for(j in 0:4){
		# prob = pxn_RT(xR=i, xT=j, n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
		prob = pxn_RT_OR(xR=i, xT=j, n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)
		cat(i, j, prob, "\n")
	}
}

# matrix(mapply(FUN = pxn_RT, xR=rep(0:4, times=5), xT=rep(0:4, each=5), MoreArgs = list(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)), ncol=5)
write.table(x=
	matrix(mapply(FUN = pxn_RT_OR, xR=rep(0:4, times=5), xT=rep(0:4, each=5), MoreArgs = list(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1, cxT = cxT_ex1)), ncol=5),
	file = paste0(ex1_d, "stage2_decision_matrix_probs.txt"), col.names=F, row.names=F, quote=F, sep = "\t")

pxn(x=0, n = n_ex1, p=0.25, cx = cxR_ex1)

pxn(x=0, n = n_ex1, p=0.5, cx = cxT_ex1)
pxn(x=1, n = n_ex1, p=0.5, cx = cxT_ex1)
pxn(x=2, n = n_ex1, p=0.5, cx = cxT_ex1)
pxn(x=3, n = n_ex1, p=0.5, cx = cxT_ex1)
pxn(x=4, n = n_ex1, p=0.5, cx = cxT_ex1)

pxn(x=0, n = n_ex1[1], p=0.5, cx = lapply(cxT_ex1, FUN = "[", 1))
pxn(x=1, n = n_ex1[1], p=0.5, cx = lapply(cxT_ex1, FUN = "[", 1))
pxn(x=2, n = n_ex1[1], p=0.5, cx = lapply(cxT_ex1, FUN = "[", 1))
pxn(x=3, n = n_ex1[1], p=0.5, cx = lapply(cxT_ex1, FUN = "[", 1))
pxn(x=4, n = n_ex1[1], p=0.5, cx = lapply(cxT_ex1, FUN = "[", 1))

#example 1 dirichlet-multinomial
#dirichlet-multinomial
n_ex1 = c(2,4)
cxR_ex1 = list(x_fail = c(0, 1), x_pass = c(NA, 2))
cxT_ex1 = list(x_fail = c(1, 2), x_pass = c(NA, 3))

#lambda=4
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 4),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 4),
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4)
), pet=c(
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4)
), EN=c(
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 4)
))

#lambda=10
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 10),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 10),
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10)
), pet=c(
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10)
), EN=c(
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 10)
))

#lambda=100
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 100),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 100),
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100)
), pet=c(
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100)
), EN=c(
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 100)
))

#lambda=1000
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 1000),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5, cxR = cxR_ex1,  cxT = cxT_ex1, lambda = 1000),
	p_success_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	p_success_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000)
), pet=c(
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	pet_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	pet_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000)
), EN=c(
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.5,  cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	EN_RT_dm(n = n_ex1, pR = 0.25, pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000),
	EN_RT_dm(n = n_ex1, pR = 0.5,  pT = 0.75, cxR = cxR_ex1, cxT = cxT_ex1, lambda = 1000)
))

#example 2
#example from page 1379 of bryant and day
# N1 = 19, N2, = 33, CR1 = 5, CR2 = 12, CT1 = 11, CT2 = 22.
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = c(NA, 13))
cxT_bd = list(x_fail = c(11, 22), x_pass = c(NA, 23))
# cxNA1 = list(x_fail = NA, x_pass = NA)

# pxn_RT(xR=13, xT=23, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# pxn_RT(xR=13, xT=23, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)

# pxn_RT_OR(xR=13, xT=23, n=n_bd, pR=0.3, pT=0.6, cxR=cxR_bd, cxT=cxT_bd)
# pxn_RT_OR(xR=13, xT=23, n=n_bd, pR=0.5, pT=0.8, cxR=cxR_bd, cxT=cxT_bd)

# calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = n_bd, p0 = 0.3, p1 = 0.5)
# calc_cutoffs(theta_L = 0.2, theta_U = 1, theta_T = 0.95, n = n_bd, p0 = 0.6, p1 = 0.8)

# all(vapply(X = cx, FUN = length, FUN.VALUE = 1) == k)
# identical(cxR_bd, cxR_bd)
# identical(cxT_bd, cxT_bd)
# identical(cxR_bd, cxT_bd)

for(theta_L in seq(from = 0.01, to = 0.99, by = 0.01)){
	for(theta_T in seq(from = 0.01, to = 0.99, by = 0.01)){
		if(
			identical(calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = theta_T, n = n_bd, p0 = 0.3, p1 = 0.5), cxR_bd) &&
			identical(calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = theta_T, n = n_bd, p0 = 0.6, p1 = 0.8), cxT_bd)
		){
			cat(theta_L, theta_T, "\n")
		}
	}
}
#theta_L: 0.08 - 0.17
#theta_T: 0.8 - 0.86

calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.3, p1 = 0.5)
calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.6, p1 = 0.8)

#T1E
p_success_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
p_success_RT(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
p_success_RT(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)

#power
p_success_RT(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)

#pet
pet_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
pet_RT(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
pet_RT(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)
pet_RT(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)

#EN
EN_RT(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
EN_RT(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd)
EN_RT(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)
EN_RT(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd)

#phi=0.01
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01)
), pet=c(
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01)
), EN=c(
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.01)
))

#phi=0.1
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)
), pet=c(
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)
), EN=c(
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 0.1)
))

#phi=1
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
), pet=c(
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
), EN=c(
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
))

#phi=10
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
), pet=c(
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
), EN=c(
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 10)
))

#phi=100
data.frame(row.names=c("00", "10", "01", "11"), alpha=c(
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100)
), pet=c(
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100)
), EN=c(
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100),
	EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 100)
))

#number of stages
# as.integer(seq(from=0, to=33, length.out = x+1)[-1])
n_list = lapply(X=1:16, FUN = function(x){as.integer(seq(from=0, to=33, length.out = x+1)[-1])})
# n_list[[17]] = n_bd

stages_stats_l = lapply(n_list, FUN =
	function(x){
		cat(x, "\n")
		n = x
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(k = length(n), n = paste0(n, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

stages_stats = do.call(rbind,stages_stats_l)
# stages_stats$n_stages = 1:16

write.table(stages_stats, file = paste0(ex2_d, "ex2.stages.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

##plots
png(file = paste0(ex2_d, "ex2.stages.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(stages_stats[,c("k", "T1E")], main = "Num of Stages vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(stages_stats[,c("k", "power")], main = "Num of Stages vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(stages_stats[,c("k", "pet")], main = "Num of Stages vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(stages_stats[,c("k", "EN")], main = "Num of Stages vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

# plot(stages_stats[,c("k", "T1E")], ylim = c(0, as.numeric(max(stages_stats[,"T1E"]))))
# plot(as.numeric(stages_stats[,"k"]), as.numeric(stages_stats[,"T1E"]))

#priors a + b = 1
prior_stats_l = lapply((1:9)/10, FUN =
	function(x){
		cat(x, "\n")
		a = x
		b = 1-a
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.3, p1 = 0.5, a0 = a, b0 = b)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.6, p1 = 0.8, a0 = a, b0 = b)
		
		T1E = max(
			p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(a = a, b = b, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

prior_stats = do.call(rbind,prior_stats_l)

write.table(prior_stats, file = paste0(ex2_d, "ex2.priorab1.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.priorab1.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(prior_stats[,c("a", "T1E")], main = "Prior a vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(prior_stats[,c("a", "power")], main = "Prior a vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(prior_stats[,c("a", "pet")], main = "Prior a vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(prior_stats[,c("a", "EN")], main = "Prior a vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#priors a + b varies
priorabsum_stats_l = lapply(1:10, FUN =
	function(x){
		cat(x, "\n")
		absum = x
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.3, p1 = 0.5, a0 = 0.3*absum, b0 = 0.7*absum)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.6, p1 = 0.8, a0 = 0.6*absum, b0 = 0.4*absum)
		
		T1E = max(
			p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c("a+b" = absum, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

priorabsum_stats = do.call(rbind,priorabsum_stats_l)

write.table(priorabsum_stats, file = paste0(ex2_d, "ex2.priorabsum.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.priorabsum.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(priorabsum_stats[,c("a+b", "T1E")], main = "prior a+b vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(priorabsum_stats[,c("a+b", "power")], main = "prior a+b vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(priorabsum_stats[,c("a+b", "pet")], main = "prior a+b vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(priorabsum_stats[,c("a+b", "EN")], main = "prior a+b vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#post prob threshold
post_stats_l = lapply((1:9)/10, FUN =
	function(x){
		cat(x, "\n")
		theta_T = x
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = theta_T, n = n_bd, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = theta_T, n = n_bd, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(theta_T = theta_T, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

post_stats = do.call(rbind,post_stats_l)

write.table(post_stats, file = paste0(ex2_d, "ex2.post.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.post.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(post_stats[,c("theta_T", "T1E")], main = "Posterior Threshold vs\nType 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(post_stats[,c("theta_T", "power")], main = "Posterior Threshold vs\nPower", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(post_stats[,c("theta_T", "pet")], main = "Posterior Threshold vs\nPET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(post_stats[,c("theta_T", "EN")], main = "Posterior Threshold vs\nEN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#pred prob threshold
pred_stats_l = lapply((1:9)/10, FUN =
	function(x){
		cat(x, "\n")
		theta_L = x
		cxR = calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(theta_L = theta_L, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

pred_stats = do.call(rbind,pred_stats_l)

write.table(pred_stats, file = paste0(ex2_d, "ex2.pred.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.pred.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(pred_stats[,c("theta_L", "T1E")], main = "Predictive Threshold vs\nType 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pred_stats[,c("theta_L", "power")], main = "Predictive Threshold vs\nPower", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pred_stats[,c("theta_L", "pet")], main = "Predictive Threshold vs\nPET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pred_stats[,c("theta_L", "EN")], main = "Predictive Threshold vs\nEN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#odds ratio
or_stats_l = lapply(10^(-4:4), FUN =
	function(x){
		cat(x, "\n")
		phi = x
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n_bd, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = phi),
			p_success_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = phi)
		)
		
		power = unname(p_success_RT_OR(n = n_bd, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = phi))

		pet = min(
			pet_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = phi),
			pet_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = phi)
		)

		EN = max(
			EN_RT_OR(n = n_bd, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = phi),
			EN_RT_OR(n = n_bd, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = phi)
		)
		
		return(c(phi = phi, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

or_stats = do.call(rbind,or_stats_l)

write.table(or_stats, file = paste0(ex2_d, "ex2.or.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.or.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(or_stats[,c("phi", "T1E")], main = "Odds Ratio vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(or_stats[,c("phi", "power")], main = "Odds Ratio vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(or_stats[,c("phi", "pet")], main = "Odds Ratio vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(or_stats[,c("phi", "EN")], main = "Odds Ratio vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
dev.off()

#timing of the interim analysis
n1_stats_l = lapply(2:32, FUN =
	function(x){
		cat(x, "\n")
		n1 = x
		n = c(n1,33)
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(n1 = n1, n = paste0(n, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

n1_stats = do.call(rbind,n1_stats_l)

write.table(n1_stats, file = paste0(ex2_d, "ex2.n1.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.n1.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(n1_stats[,c("n1", "T1E")], main = "n1 vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(n1_stats[,c("n1", "power")], main = "n1 vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(n1_stats[,c("n1", "pet")], main = "n1 vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(n1_stats[,c("n1", "EN")], main = "n1 vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#size nk
nk_stats_l = lapply(20:50, FUN =
	function(x){
		cat(x, "\n")
		nk = x
		n = c(19,nk)
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
		
		T1E = max(
			p_success_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			p_success_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		power = unname(p_success_RT_OR(n = n, pR = 0.5, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1))

		pet = min(
			pet_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			pet_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)

		EN = max(
			EN_RT_OR(n = n, pR = 0.5, pT = 0.6, cxR = cxR, cxT = cxT, phi = 1),
			EN_RT_OR(n = n, pR = 0.3, pT = 0.8, cxR = cxR, cxT = cxT, phi = 1)
		)
		
		return(c(nk = nk, n = paste0(n, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)

nk_stats = do.call(rbind,nk_stats_l)

write.table(nk_stats, file = paste0(ex2_d, "ex2.nk.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.nk.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(nk_stats[,c("nk", "T1E")], main = "nk vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(nk_stats[,c("nk", "power")], main = "nk vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(nk_stats[,c("nk", "pet")], main = "nk vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(nk_stats[,c("nk", "EN")], main = "nk vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#dirichlet-multinomial
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = c(NA, 13))
cxT_bd = list(x_fail = c(11, 22), x_pass = c(NA, 23))
lambda = sqrt(10)^(0:8)
lambda_stats_l = lapply(X = lambda, FUN = trial_perf_RT_dm, n = n_bd, pR0 = 0.3, pR1 = 0.5, pT0 = 0.6, pT1 = 0.8, cxR = cxR_bd, cxT = cxT_bd, phi = 1)
lambda_stats = do.call(rbind,lambda_stats_l)
lambda_stats = cbind(lambda = lambda, phi = 1, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR_bd$x_fail, collapse = ","), cxT = paste0(cxT_bd$x_fail, collapse = ","), lambda_stats)

write.table(lambda_stats, file = paste0(ex2_d, "ex2.lambda.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.lambda.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(lambda_stats[,c("lambda", "T1E")], main = "Lambda vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(lambda_stats[,c("lambda", "power")], main = "Lambda vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(lambda_stats[,c("lambda", "pet")], main = "Lambda vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
	plot(lambda_stats[,c("lambda", "EN")], main = "Lambda vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1, log = "x")
dev.off()

# for(theta_L in seq(from = 0.01, to = 0.99, by = 0.01)){
	# for(theta_T in seq(from = 0.01, to = 0.99, by = 0.01)){
		# cat(theta_L, theta_T, ":", unlist(calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = theta_T, n = n_bd, p0 = 0.3, p1 = 0.5)), "\n")
	# }
# }

#curves pred v post
# cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.3, p1 = 0.5)
# cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.6, p1 = 0.8)

# cx = data.frame(cxR$x_fail, cxT$x_fail)
# cx_u = unique(cx)
# cx_u$nl = which(!duplicated(cx))
# png()
# cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.3, p1 = 0.5)
# plot(which(!duplicated(cxR$x_fail)), unique(cxR$x_fail), 
	# xlim = c(1,33), 
	# ylim=c(0,12.5), 
	# type = "l", col = "red", lwd = 3)
# for(theta_L in c(0.05, (1:9)/10, 0.95)){
	# cxR = calc_cutoffs(theta_L = theta_L, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.3, p1 = 0.5)
	# lines(which(!duplicated(cxR$x_fail)), unique(cxR$x_fail), lty=3, col = "red")
	# text(max(which(!duplicated(cxR$x_fail)), na.rm=T), 
		# max(unique(cxR$x_fail), na.rm=T), pos = 3, theta_L)
# }
# dev.off()

# cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.3, p1 = 0.5)
# plot(which(!duplicated(cxR$x_fail)), unique(cxR$x_fail), 
	# xlim = c(1,35), ylim=c(0,14), 
	# type = "l", col = "red", lwd = 3)
# for(theta_T in c(0.05, (1:9)/10, 0.95)){
	# cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = theta_T, n = 1:33, p0 = 0.3, p1 = 0.5)
	# lines(which(!duplicated(cxR$x_fail)), unique(cxR$x_fail), lty=3, col = "red")
	# text(33, cxR$x_fail[33], pos = 4, theta_T)
# }

#Response vs theta_L
cxR_theta_L_l = lapply(X = c(0.05, (1:9)/10, 0.95), FUN = calc_cutoffs, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.3, p1 = 0.5)
cxR_fail_theta_L_l = lapply(X = cxR_theta_L_l, FUN = getElement, name = "x_fail")
cxR_fail_theta_L = do.call(data.frame, cxR_fail_theta_L_l)
names(cxR_fail_theta_L) = c(0.05, (1:9)/10, 0.95)
cxR_fail_theta_L$nl = 1:33
write.table(cxR_fail_theta_L, file = paste0(ex2_d, "ex2.cxR.theta_L.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.cxR.theta_L.png"), width = 7, height = 7, units = "in", res = 300)
plot(which(!duplicated(cxR_fail_theta_L$"0.1")), unique(cxR_fail_theta_L$"0.1"), 
	xlim = c(1,34), ylim=c(0,12.5), 
	main = "Response Cutoff by Pred Prob Threshold", xlab = "Number of Patients nl", ylab = "Response Cutoff CxR",
	type = "l", col = "red", lwd = 3, las = 1, cex.main = 1.5, cex.lab = 1.5, cex.axis=1.5)
lapply(X=cxR_fail_theta_L_l, FUN = function(x,...){lines(which(!duplicated(x)), unique(x),...)}, col = "red", lty = 3, lwd=2)
lapply(X=c(0.05, 0.95), FUN = function(x){
	cx = cxR_fail_theta_L[[as.character(x)]]
	text(max(which(!duplicated(cx)), na.rm=T), max(unique(cx), na.rm=T), pos = 3, cex=1.5, x)})
dev.off()

#response by theta_T
cxR_theta_T_l = lapply(X = c(0.05, (1:9)/10, 0.95), FUN = calc_cutoffs, theta_L = 0.1, theta_U = 1, n = 1:33, p0 = 0.3, p1 = 0.5)
cxR_fail_theta_T_l = lapply(X = cxR_theta_T_l, FUN = getElement, name = "x_fail")
cxR_fail_theta_T = do.call(data.frame, cxR_fail_theta_T_l)
names(cxR_fail_theta_T) = c(0.05, (1:9)/10, 0.95)
cxR_fail_theta_T$nl = 1:33
write.table(cxR_fail_theta_T, file = paste0(ex2_d, "ex2.cxR.theta_T.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.cxR.theta_T.png"), width = 7, height = 7, units = "in", res = 300)
plot(which(!duplicated(cxR_fail_theta_T$"0.8")), unique(cxR_fail_theta_T$"0.8"), 
	xlim = c(1,36), ylim=c(0,14), 
	main = "Response Cutoff by Post Prob Threshold", xlab = "Number of Patients nl", ylab = "Response Cutoff CxR",
	type = "l", col = "red", lwd = 3, las = 1, cex.main = 1.5, cex.lab = 1.5, cex.axis=1.5)
lapply(X=cxR_fail_theta_T_l, FUN = function(x,...){lines(which(!duplicated(x)), unique(x),...)}, col = "red", lty = 3, lwd=2)
lapply(X=c(0.05, c(2,4,6,8)/10, 0.95), FUN = function(x){text(33, cxR_fail_theta_T[[as.character(x)]][33], pos = 4, cex=1.5, x)})
dev.off()

#Nontoxicity vs theta_L
cxT_theta_L_l = lapply(X = c(0.05, (1:9)/10, 0.95), FUN = calc_cutoffs, theta_U = 1, theta_T = 0.8, n = 1:33, p0 = 0.6, p1 = 0.8)
cxT_fail_theta_L_l = lapply(X = cxT_theta_L_l, FUN = getElement, name = "x_fail")
cxT_fail_theta_L = do.call(data.frame, cxT_fail_theta_L_l)
names(cxT_fail_theta_L) = c(0.05, (1:9)/10, 0.95)
cxT_fail_theta_L$nl = 1:33
write.table(cxT_fail_theta_L, file = paste0(ex2_d, "ex2.cxT.theta_L.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.cxT.theta_L.png"), width = 7, height = 7, units = "in", res = 300)
plot(which(!duplicated(cxT_fail_theta_L$"0.1")), unique(cxT_fail_theta_L$"0.1"), 
	xlim = c(1,34), ylim=c(0,22.5), 
	main = "Nontoxicity Cutoff by Pred Prob Threshold", xlab = "Number of Patients nl", ylab = "Response Cutoff CxT",
	type = "l", col = "red", lwd = 3, las = 1, cex.main = 1.5, cex.lab = 1.5, cex.axis=1.5)
lapply(X=cxT_fail_theta_L_l, FUN = function(x,...){lines(which(!duplicated(x)), unique(x),...)}, col = "red", lty = 3, lwd=2)
# lapply(X=c(0.05, 0.95), FUN = function(x){
	# cx = cxT_fail_theta_L[[as.character(x)]]
	# text(max(which(!duplicated(cx)), na.rm=T), max(unique(cx), na.rm=T), pos = 3, cex=1.5, x)})
text(c(33.5,30), c(22,22), pos = 3, cex=1.5, c("0.05", "0.95"))
dev.off()

#Nontoxicity by theta_T
cxT_theta_T_l = lapply(X = c(0.05, (1:9)/10, 0.95), FUN = calc_cutoffs, theta_L = 0.1, theta_U = 1, n = 1:33, p0 = 0.6, p1 = 0.8)
cxT_fail_theta_T_l = lapply(X = cxT_theta_T_l, FUN = getElement, name = "x_fail")
cxT_fail_theta_T = do.call(data.frame, cxT_fail_theta_T_l)
names(cxT_fail_theta_T) = c(0.05, (1:9)/10, 0.95)
cxT_fail_theta_T$nl = 1:33
write.table(cxT_fail_theta_T, file = paste0(ex2_d, "ex2.cxT.theta_T.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.cxT.theta_T.png"), width = 7, height = 7, units = "in", res = 300)
plot(which(!duplicated(cxT_fail_theta_T$"0.8")), unique(cxT_fail_theta_T$"0.8"), 
	xlim = c(1,36), ylim=c(0,24), 
	main = "Nontoxicity Cutoff by Post Prob Threshold", xlab = "Number of Patients nl", ylab = "Response Cutoff CxT",
	type = "l", col = "red", lwd = 3, las = 1, cex.main = 1.5, cex.lab = 1.5, cex.axis=1.5)
lapply(X=cxT_fail_theta_T_l, FUN = function(x,...){lines(which(!duplicated(x)), unique(x),...)}, col = "red", lty = 3, lwd=2)
lapply(X=c(0.05, c(2,4,6,8)/10, 0.95), FUN = function(x){text(33, cxT_fail_theta_T[[as.character(x)]][33], pos = 4, cex=1.5, x)})
dev.off()

#changing pR
# n_bd = c(19, 33)
# cxR_bd = list(x_fail = c(5, 12), x_pass = c(NA, 13))
# cxT_bd = list(x_fail = c(11, 22), x_pass = c(NA, 23))

# trial_perf_RT(n = n_bd, pR0=0.3, pR1=0.5, pT0=0.6, pT1=0.8, cxR = cxR_bd, cxT = cxT_bd)
# trial_perf_RT(n = n_bd, pR0=0.1, pR1=0.1, pT0=0.6, pT1=0.8, cxR = cxR_bd, cxT = cxT_bd)

n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = c(NA, 13))
cxT_bd = list(x_fail = c(11, 22), x_pass = c(NA, 23))
# pR = (0:10)/10
# pR = (0:8)/10
# pR_stats_l = lapply(X = pR, FUN = function(pR, ...){trial_perf_RT(pR0=pR, pR1=pR, ...)}, n = n_bd, pT0 = 0.8, pT1 = 0.8, cxR = cxR_bd, cxT = cxT_bd)
pR_stats_l = lapply((0:10)/10, FUN =
	function(x){
		cat(x, "\n")
		pR = x
		n = n_bd
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
		
		T1E = p_success_RT(n = n, pR = pR, pT = 0.6, cxR = cxR, cxT = cxT)
		
		power = unname(p_success_RT(n = n, pR = pR, pT = 0.8, cxR = cxR, cxT = cxT))

		pet = pet_RT(n = n, pR = pR, pT = 0.6, cxR = cxR, cxT = cxT)

		EN = EN_RT(n = n, pR = pR, pT = 0.6, cxR = cxR, cxT = cxT)
		
		return(c(pR = pR, n = paste0(n, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)
pR_stats = do.call(rbind,pR_stats_l)
# pR_stats = cbind(pR = pR, phi = 1, n = paste0(n_bd, collapse = ","), cxR = paste0(cxR_bd$x_fail, collapse = ","), cxT = paste0(cxT_bd$x_fail, collapse = ","), pR_stats)

write.table(pR_stats, file = paste0(ex2_d, "ex2.pR.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.pR.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(pR_stats[,c("pR", "T1E")], main = "pR vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pR_stats[,c("pR", "power")], main = "pR vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pR_stats[,c("pR", "pet")], main = "pR vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pR_stats[,c("pR", "EN")], main = "pR vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

#changing pT
n_bd = c(19, 33)
cxR_bd = list(x_fail = c(5, 12), x_pass = c(NA, 13))
cxT_bd = list(x_fail = c(11, 22), x_pass = c(NA, 23))
pT_stats_l = lapply((0:10)/10, FUN =
	function(x){
		cat(x, "\n")
		pT = x
		n = n_bd
		cxR = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.3, p1 = 0.5)
		cxT = calc_cutoffs(theta_L = 0.1, theta_U = 1, theta_T = 0.8, n = n, p0 = 0.6, p1 = 0.8)
		
		T1E = p_success_RT(n = n, pR = 0.3, pT = pT, cxR = cxR, cxT = cxT)
		
		power = unname(p_success_RT(n = n, pR = 0.5, pT = pT, cxR = cxR, cxT = cxT))

		pet = pet_RT(n = n, pR = 0.3, pT = pT, cxR = cxR, cxT = cxT)

		EN = EN_RT(n = n, pR = 0.3, pT = pT, cxR = cxR, cxT = cxT)
		
		return(c(pT = pT, n = paste0(n, collapse = ","), cxR = paste0(cxR$x_fail, collapse = ","), cxT = paste0(cxT$x_fail, collapse = ","), T1E = T1E, power = power, pet = pet, EN = EN))
	}
)
pT_stats = do.call(rbind,pT_stats_l)
write.table(pT_stats, file = paste0(ex2_d, "ex2.pT.stats.txt"), col.names=T, row.names=F, quote=F, sep = "\t")

png(file = paste0(ex2_d, "ex2.pT.stats.png"), width = 7, height = 7, units = "in", res = 300)
	par(mfrow=c(2,2))
	plot(pT_stats[,c("pT", "T1E")], main = "pT vs Type 1 Error", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pT_stats[,c("pT", "power")], main = "pT vs Power", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pT_stats[,c("pT", "pet")], main = "pT vs PET", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
	plot(pT_stats[,c("pT", "EN")], main = "pT vs EN", cex.main = 1.5, cex.lab = 1.5, lwd = 1.5, cex = 1.5, col = "blue", las = 1)
dev.off()

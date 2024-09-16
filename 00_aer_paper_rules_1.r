omega_KZ = function(data){
	gamma = 3;
	n = ncol(data);
	t = nrow(data);
	mu_est_loc = mu_est(data)
	sigma_est_loc = sigma_est(data)
	omega_G_loc = omega_G(data)
	omega_H_loc = omega_H(data)

	mu_est_G = t(omega_G_loc)%*%mu_est_loc;
	sigma2_est_G = t(omega_G_loc)%*%sigma_est_loc%*%omega_G_loc;
	mu_est_H = t(omega_H_loc)%*%mu_est_loc
	sigma2_est_H = t(omega_H_loc)%*%sigma_est_loc%*%omega_H_loc
	theta2_est_H = (mu_est_H/(sigma2_est_H^(0.5)))^2

	#part_a = ((t-n-2)*theta2_est_H-n)/t
	#part_b = 2*(theta2_est_H^(n/2))*((1+theta2_est_H)^(-(t-2)/2))
	#part_c = t*B_func(theta2_est_H/(1+theta2_est_H),n/2,(t-n)/2)
	#theta2_est_Ha =  part_a + part_b/part_c 
		
	a_est_KZ = c((1/gamma)*((t-n-1)*(t-n-4)/(t*(t-2)))*(mu_est_G/sigma2_est_G))
	b_est_KZ = c(((t-n-1)*(t-n-4)/(t*(t-2)))*(theta2_est_H/(theta2_est_H+n/t)))
	return(a_est_KZ * omega_G_loc + b_est_KZ * omega_H_loc)
}

omega_CBR = function(data){
	gamma = 3;
	n = ncol(data);
	t = nrow(data);
	mu_est_loc = mu_est(data);
	sigma_est_loc = sigma_est(data);
	omega_G_loc = omega_G(data);
	omega_H_loc = omega_H(data);

	mu_est_G = t(omega_G_loc)%*%mu_est_loc;
	sigma2_est_G = t(omega_G_loc)%*%sigma_est_loc%*%omega_G_loc;
	mu_est_H = t(omega_H_loc)%*%mu_est_loc
	sigma2_est_H = t(omega_H_loc)%*%sigma_est_loc%*%omega_H_loc	
	theta2_est_H = (mu_est_H/(sigma2_est_H^(0.5)))^2

	#part_a = ((t-n-2)*theta2_est_H-n)/t
	#part_b = 2*(theta2_est_H^(n/2))*((1+theta2_est_H)^(-(t-2)/2))
	#part_c = t*B_func(theta2_est_H/(1+theta2_est_H),n/2,(t-n)/2)
	#theta2_est_Ha =  part_a + part_b/part_c 

	a_est_CBR = c((1/gamma)*((t-n-1)*(t-n-4)/(t*(t-2)))*(mu_est_G/sigma2_est_G))
	b_est_CBR = c((((t-n)*(t-n-5)*(t-n-7))/((t^2)*(t-2)))*(theta2_est_H/(theta2_est_H+n/t)))	
	return(a_est_CBR * omega_G_loc + b_est_CBR * omega_H_loc)
}


omega_Q = function(data){
	gamma = 3;
	n = ncol(data);
	t = nrow(data);
	
	mu_est_loc = mu_est(data)
	sigma_est_loc = sigma_est(data)
	omega_G_loc = omega_G(data)
	omega_H_loc = omega_H(data)

	mu_est_G = t(omega_G_loc)%*%mu_est_loc;
	sigma2_est_G = t(omega_G_loc)%*%sigma_est_loc%*%omega_G_loc;
	mu_est_H = t(omega_H_loc)%*%mu_est_loc
	sigma2_est_H = t(omega_H_loc)%*%sigma_est_loc%*%omega_H_loc
	theta2_est_H = (mu_est_H/(sigma2_est_H^(0.5)))^2

	#part_a = ((t-n-2)*theta2_est_H-n)/t
	#part_b = 2*(theta2_est_H^(n/2))*((1+theta2_est_H)^(-(t-2)/2))
	#part_c = t*B_func(theta2_est_H/(1+theta2_est_H),n/2,(t-n)/2)
	#theta2_est_Ha =  part_a + part_b/part_c 
		
	a_est_Q = c((1/gamma)*((t-n-1)/(t-2))*(mu_est_G/sigma2_est_G))
	b_est_Q = c(((t-n)*(t-n-3)/(t*(t-2)))*(theta2_est_H/(theta2_est_H+(n-1)/t)))
	return(a_est_Q * omega_G_loc + b_est_Q * omega_H_loc)
}

omega_M = function(data){
	gamma = 3;
	n = ncol(data);
	t = nrow(data);
	
	mu_est_loc = mu_est(data)
	sigma_est_loc = sigma_est(data)
	omega_G_loc = omega_G(data)
	omega_H_loc = omega_H(data)

	mu_est_G = t(omega_G_loc)%*%mu_est_loc;
	sigma2_est_G = t(omega_G_loc)%*%sigma_est_loc%*%omega_G_loc;
	mu_est_H = t(omega_H_loc)%*%mu_est_loc
	sigma2_est_H = t(omega_H_loc)%*%sigma_est_loc%*%omega_H_loc
	theta2_est_H = (mu_est_H/(sigma2_est_H^(0.5)))^2

	#part_a = ((t-n-2)*theta2_est_H-n)/t
	#part_b = 2*(theta2_est_H^(n/2))*((1+theta2_est_H)^(-(t-2)/2))
	#part_c = t*B_func(theta2_est_H/(1+theta2_est_H),n/2,(t-n)/2)
	#theta2_est_Ha =  part_a + part_b/part_c 
		
	a_est_M = c((1/gamma)*((t-n-1)/(t-2))*(mu_est_G/sigma2_est_G))
	b_est_M = c(((t-n)*(t-n-3)/(t*(t-2)))*(theta2_est_H/(theta2_est_H+(n-1)/t)))
	return(a_est_M * omega_G_loc + b_est_M * omega_H_loc)
	
}

omega_QS = function(data){
	gamma = 3;
	n = ncol(data);
	t = nrow(data);
	
	mu_est_loc = mu_est(data)
	sigma_est_loc = sigma_est(data)
	omega_G_loc = omega_G(data)
	omega_H_loc = omega_H(data)

	mu_est_G = t(omega_G_loc)%*%mu_est_loc;
	sigma2_est_G = t(omega_G_loc)%*%sigma_est_loc%*%omega_G_loc;
	mu_est_H = t(omega_H_loc)%*%mu_est_loc
	sigma2_est_H = t(omega_H_loc)%*%sigma_est_loc%*%omega_H_loc
	theta2_est_H = (mu_est_H/(sigma2_est_H^(0.5)))^2

	#part_a = ((t-n-2)*theta2_est_H-n)/t
	#part_b = 2*(theta2_est_H^(n/2))*((1+theta2_est_H)^(-(t-2)/2))
	#part_c = t*B_func(theta2_est_H/(1+theta2_est_H),n/2,(t-n)/2)
	#theta2_est_Ha =  part_a + part_b/part_c 
		
	a_est_QS = c((1/gamma)*((t-n-1)*(t-n-4)/(t*(t-2)))*(mu_est_G/sigma2_est_G))
	b_est_QS = c(((t-n)*(t-n-5)*(t-n-7)/((t^2)*(t-2)))*(theta2_est_H/(theta2_est_H+(n-1)/t)))
	return(a_est_QS * omega_G_loc + b_est_QS * omega_H_loc)
}
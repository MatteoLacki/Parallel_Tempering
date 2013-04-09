Dimension		<- 10

# Proposal covariance 
Sigma_Proposal 		<- diag(Dimension)
Sigma_Proposal_Chol 	<- chol(Sigma_Proposal)
Sigma_Under_Test 	<- diag(dimension)
No_of_Clusters 		<- 6;


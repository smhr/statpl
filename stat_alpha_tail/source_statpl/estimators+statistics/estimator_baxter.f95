subroutine baxterest(x,beta,xmin)
! Baxter's UMVU estimator for the exponent of an untruncated Pareto law
! Calculate the UMVU estimate for beta and the lower truncation limit
! for data from a Pareto distribution.
! See M.A. Baxter, "Minimum Variance Unbiased Estimation of the Parameters
! of the Pareto Distribution", 1980, Metrika 27, pp. 133-138
implicit none
double precision, dimension(:), intent(in) :: x
double precision, intent(out) :: beta,xmin
double precision :: n,t,y,a,sigma

n=float(size(x))
t=sum(log(x))
y=minval(x)
a=(n-2.d0)/(t-n*log(y))

beta=a+1.d0

sigma=a**2/(n-3.d0)
sigma=sqrt(sigma)

xmin=n*a*y/(n*a - 1.d0) ! ML estimate
xmin=xmin*(1 - 1/(n-1.d0)/a)

end subroutine baxterest

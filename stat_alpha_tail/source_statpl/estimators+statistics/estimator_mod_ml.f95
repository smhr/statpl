subroutine modmlest (x,beta,xmin,xmax)
! Modified ML estimator as described in Maschberger & Kroupa
implicit none
double precision, dimension(:), intent(in) :: x
double precision, intent(out) :: beta,xmin,xmax
double precision :: a,aa,sigma
double precision :: y,z
double precision :: n

n=float(size(x))
!print *,"salam modmlest"
 call mlest (x,beta,xmin,xmax)
aa=beta-1

a=(beta-1)*n/(n-2.)
beta=a+1.
sigma=a**2/(n-2)
sigma=sqrt(sigma)

y=minval(x)
z=maxval(x)

xmin=y
xmax=z
xmin=xmin*0.9999
xmax=xmax*1.0001

a=aa

if (a > 1.e-3) then
	xmin=xmin*(1.-1./(n-1.)/a)
	if (xmin < 0 ) xmin=y*0.99
	xmax=z* ( 1. + ( exp(a*log(z/y)) - 1. )/(n) )**(1./a)
end if

end subroutine modmlest

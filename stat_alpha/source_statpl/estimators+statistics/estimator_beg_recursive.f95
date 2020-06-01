subroutine begest_recursive (x,beta,xmin,xmax)
! Recursive calculation of Begs 1983 estimator (Maschberger & Kroupa 2007)
implicit none 
double precision, dimension(:), intent(in) :: x
double precision :: tmp1
double precision, intent(out) :: beta,xmin,xmax
double precision :: t,y,z,k
double precision :: oy,oyz,okyz
! variable named as in Beg 1983
integer :: j,n,jmax

n=size(x)
t=sum(log(x))
y=log(minval(x))
z=log(maxval(x))

k=(z-y)/(t-n*y)

j=1
do 
if ( j*k >=1. ) exit
j=j+1
end do
jmax=j-1

okyz=0.
do j=jmax,2,-1
tmp1=(1.-float(j)*k)**float(n-4)
okyz=float(n-j)/float(j-1)*(tmp1-okyz)
end do
okyz=(1-k)**(n-4)-okyz

oyz=0.
do j=jmax,2,-1
tmp1=(1-float(j)*k)**(n-3)
oyz=float(n-j)/float(j-1)*(tmp1-oyz)
end do
oyz=(1-k)**(n-3)-oyz

beta=(float(n)-3.)/(t-float(n)*y)
beta=beta*okyz/oyz
beta=beta+1

oy=0.
do j=jmax,1,-1
tmp1=(1-float(j)*k)**(n-2)
oy=float(n-j)/float(j)*(tmp1-oy)
end do
oy=1-oy

xmax=maxval(x)*(1 + (t-float(n)*y)/n/(n-1)*oy/oyz)

xmin=minval(x)

end subroutine begest_recursive

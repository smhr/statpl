! 1. Beg's estimator in its original form
subroutine begest (x,beta,xmin,xmax)
! calculate Begs estimator in the form as in Beg 1983
implicit none 
double precision, dimension(:), intent(in) :: x
double precision, intent(out) :: beta,xmin,xmax
double precision :: tmp1,tmp2,tmp3,tmp
double precision :: t,y,z,k
double precision :: oy,oyz,okyz
! crude variable names come from Beg 1983
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

oyz=0.
do j=0,jmax
tmp1=(-1)**j
tmp2=binko(n-2,j)
tmp3=(t-float(n)*y-float(j+1)*(z-y))**(n-3)
tmp=tmp1*tmp2*tmp3
oyz=oyz+tmp
end do

okyz=0.
do j=0,jmax
tmp1=(-1)**j
tmp2=binko(n-2,j)
tmp3=(t-float(n)*y-float(j+1)*(z-y))**(n-4)
tmp=tmp1*tmp2*tmp3
okyz=okyz+tmp
end do

beta=float(n-3)*okyz/oyz+1

oy=0.
do j=0,jmax
tmp1=(-1)**j
tmp2=binko(n-1,j)
tmp3=(t-float(n)*y-float(j)*(z-y))**(n-2)
tmp=tmp1*tmp2*tmp3
oy=oy+tmp
end do

xmax=maxval(x)*(1+oy/oyz/n/(n-1))

xmin=minval(x)

end subroutine begest

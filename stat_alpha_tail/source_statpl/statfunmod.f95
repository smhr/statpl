module statfunmod
use mathmod
implicit none

contains

function gaussian (x,mu,sigma)
! Returns the value of a Gaussian Distribution
implicit none
double precision :: x,mu,sigma
double precision :: gaussian
double precision :: twopi
parameter (twopi=6.2831853)
gaussian=1.d0/sqrt(twopi)/(sigma)*exp(- ((x-mu)/sigma)**2./2. )
end function gaussian


function average (x,m)
! calculate the average of data in an array
implicit none
double precision :: average
double precision, dimension(:), intent(in) :: x
logical, dimension(:), intent(in), optional :: m
!double precision :: average
!print *,size(x),"size"
!if (size(x) <=1.d0) print *, "",size(x)
average=sum(x)/float(size(x))
end function average

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

double precision function stddeviation (xin)
! calculate the standard deviation of data in an array
implicit none
double precision, dimension(:), intent(in) ::xin
double precision, dimension(size(xin)) :: x
double precision :: av
x=xin
av=sum(x)/float(size(x))
stddeviation=sum(( x- av )**2.)
stddeviation=sqrt(stddeviation/float(size(x)))
end function stddeviation

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

double precision function avdeviation (x)
! Calculate the average deviation of data in an array
implicit none
double precision, dimension(:), intent(in) ::x
double precision :: tmp,av
integer :: i
av=average(x)
tmp=0.
do i=1,size(x)
tmp=tmp+abs(x(i)-av)
end do
avdeviation=tmp/float(size(x))
end function avdeviation

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

subroutine linreg (x,y,delta,mask,a,b)
! calculate the 
implicit none
double precision, dimension(:), intent(in) :: x,y,delta
logical, dimension(size(x)), intent(in) :: mask
double precision, intent(out) :: a,b
double precision, dimension(size(x)) :: dd
double precision, dimension(size(x)) :: tmp
double precision :: sx,sy,sxx,sxy,n
integer ::i
!double precision :: s,d

n=0.
do i=1,size(mask)
if (mask(i)) n=n+1
end do
!where(mask) n=n+1.
dd=1/delta**2.
!s=sum(dd,mask)
!s=1.

sx=sum(x,mask)/n
sy=sum(y,mask)/n
tmp=(x-sx)*(y-sy)
sxy=sum(tmp,mask)
tmp=(x-sx)*(x-sx)
sxx=sum(tmp,mask)

!d=s*sxx-sx*2.
b=sxy/sxx
a=sy-b*sx

sx=sum(x/dd,mask)
sy=sum(y/dd,mask)
tmp=x*x/dd
sxx=sum(x**2/dd,mask)
sxy=sum(x*y/dd,mask)
!print *,sx
!print *,sy
!print *,sxx
!print *,sxy
!d=s*sxx -sx**2.

!a=(sxx*sy - sx*sxy)/d
!print *,a
!b=(s*sxy - sx*sy)/d
!print *,b

!pause

!sigma=1.

end subroutine linreg

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

subroutine histogram(x,limits,counts,intervals)
implicit none
double precision, dimension(:) :: x
double precision, dimension(:) :: limits
double precision, dimension(:) :: intervals, counts
!logical, dimension(n) ::zeroes
!double precision :: beta,a,b
integer :: n,i,j
double precision :: tmp,xmin,xmax,binsize
n=size(counts)
!sort the data
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do

!upper and lower limit of the bins
xmin=x(1)-(x(2)-x(1))/2.
xmin=xmin*0.99
xmax=x(size(x)) + (x(size(x))-x(size(x)-1))/2.
xmax=xmax*1.01
!size of the bins
binsize=(xmax-xmin)/float(n)

tmp=xmin
do i=1,n
limits(i)=tmp
intervals(i)=tmp+binsize/2.
tmp=tmp+binsize
end do
limits(n+1)=xmax


! centers of the intervals
do i=1,n
intervals(i)=limits(i) + (limits(i+1)-limits(i))/2.
end do

! count the number of data per interval
counts=0.
do i=1,size(x)
do j=1,size(limits)-1 
if ( x(i) >= limits(j) .AND. x(i) < limits(j+1)) then
counts(j)=counts(j)+1
end if
end do
end do

end subroutine histogram




end module statfunmod


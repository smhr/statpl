subroutine constbinestlr (n,data,beta)
! Estimator for the exponent of a Pareto law using linear regression with constant-size bins
implicit none
integer, intent(in) :: n ! number of bins
double precision, dimension(:), intent(in) :: data ! the data
double precision, intent(out) :: beta
double precision, dimension(size(data)) :: x
double precision, dimension(n+1) :: limits
double precision, dimension(n) :: intervals, counts,delta
logical, dimension(n) ::zeroes
double precision :: a,b
integer :: i,j
double precision :: xmin,xmax,binsize,tmp

x=data
!sort the data
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do

!upper and lower limit of the bins
xmin=x(1)-(x(2)-x(1))/2.
xmin=xmin*0.99
xmax=x(size(x)) + (x(size(x))-x(size(x)-1))/2.
xmax=xmax*1.11

!size of the bins
binsize=(log10(xmax)-log10(xmin))/n

!limits in log space
tmp=log10(xmin)
do i=1,n
limits(i)=tmp
intervals(i)=tmp+binsize/2.
tmp=tmp+binsize
end do
limits(n+1)=log10(xmax)

! limits un-log
limits=10.**limits

! centers of the intervals
do i=1,n
intervals(i)=log10(limits(i) + (limits(i+1)-limits(i))/2.)
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


! look for empty bins
zeroes=.true.
do i=1,size(counts)
if (counts(i)==0) zeroes(i)=.false.
end do

a=log10(exp(1.))**2.
do i=1,size(counts)
tmp=counts(i)*size(x)/(size(x)-counts(i))/a + epsilon(1.)
delta(i)=1./sqrt(tmp)
end do

do i=1,size(counts)
counts(i)=counts(i)/(limits(i+1)-limits(i))+ epsilon(1.)
counts(i)=log10(counts(i))
end do

 call linreg(intervals,counts,delta,zeroes,a,b)

beta=-b

end subroutine constbinestlr

subroutine varbinestchi (n,data,beta,mmax)
! Calculate the estimate of the exponent of a power-law pdf
! using bins with approximately the same number of data.
! See Maiz-Apellaniz and Ubeda (Astrophysical Journal 629, pp. 873-880, 2005)
implicit none
integer, intent(in) :: n ! number of bins
double precision, dimension(:), intent(in) :: data ! the input data
double precision, intent(out) :: beta ! the estimated exponent
double precision, intent(out) :: mmax ! the estimated upper limit
double precision, dimension(size(data)) :: x 
double precision, dimension(n+1) :: limits 
double precision, dimension(n) :: counts
double precision :: k,tmp
integer :: i,j,nperbin,ntmp,rest

! working array of the data
x=data

! look if the data are sorted, if not then sort
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do


! number of data per bin
nperbin=floor(float(size(x))/float(n))
! remaining data if the number of data and the number of bins do not fit
rest=size(x)-n*nperbin

! calculate the number of data per bin and the limits
counts=0
! lower limit of the bins
j=1
i=1
limits(j)=x(i)-(x(i+1)-x(i))/2.

ntmp=0
counts(j)=1
if (rest /= 0) nperbin=nperbin+1
do i=2,size(x)
if (i==ntmp+nperbin+1) then
j=j+1
ntmp=ntmp+nperbin
rest=rest-1
if (rest==0) nperbin=nperbin-1
limits(j)=x(i)-(x(i)-x(i-1))/2.
end if
counts(j)=counts(j)+1
end do

! upper limit of the bins
i=size(x)
j=j+1
limits(j)=x(i)+(x(i)-x(i-1))/2.

! calculate the chi square
 call chisquare_plbinw (counts,limits,beta,k)

beta=-beta

mmax=maxval(x)
tmp=float(size(x))*(1-beta)/k + minval(x)**(1-beta)
if (tmp>0) then
mmax=( float(size(x))*(1-beta)/k + minval(x)**(1-beta) )**(1./(1.-beta))
end if



end subroutine varbinestchi


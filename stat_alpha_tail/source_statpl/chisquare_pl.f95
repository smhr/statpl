subroutine chisquare_plbinw (counts,limits,beta,k)
! Calculate the estimate of the exponent for a power-law pdf 
! from binned data using the weights of Maiz-Apellaniz and Ubeda
! (Astrophysical Journal 629, pp. 973-880, 2005)
implicit none
double precision, dimension(:), intent(in) :: counts ! number of counts per bin
double precision, dimension(size(counts)+1), intent(in) :: limits ! limits of the bins
double precision, intent(out) :: beta ! estimated exponent of the power law
double precision, intent(out) :: k ! estimated normalisation constant
double precision, dimension(3) :: y
double precision, dimension(3,2) :: p
double precision ::tmp
integer :: iter,itertmp,restart
double precision :: ftol
! Tolerance for the amoeba
ftol=1.0d-10

! Calculate an estimate of the normalisation
tmp=sum(counts)*(1.-2.3)/(limits(size(limits))**(-1.3) - limits(1)**(-1.3))

p(1,:)=(/10.d0,tmp/)

! Find the minimum
itertmp=0.
iter=1001
restart=1.
do 
	! Restart at least 3 times
	if (iter<1000 .and. restart>3) exit
	! If after 1000 iterations no convergence is reached, then
	! restart.
	! If after 100000 iterations = 10 restarts no convergence is reached,
	! then exit with the last result
	if (itertmp>10000) exit
	! Initialise
	p(1,:)=(/p(1,1),p(1,2)/)
	p(2,:)=(/p(1,1),0.01d0/)
	p(3,:)=(/0.01d0,p(1,2)/)
	y(1)=chisquareplbinw(p(1,:),counts,limits)
	y(2)=chisquareplbinw(p(2,:),counts,limits)
	y(3)=chisquareplbinw(p(3,:),counts,limits)
	
	 call amoebavarbinestchi (p,y,ftol,iter,counts,limits)
	itertmp=itertmp+iter
	restart=restart+1
end do

!!! the error if no convergence was reached does somehow not work, thus excluded
!!! if (itertmp>10000) call error("No convergence in chisquare_varbin, estimate is not accurate")

! The factor 1 is added in chisquare and chisquare uses only the absolute, see below
beta=-abs(p(1,1))-1.

k=abs(p(1,2))

if (itertmp>10000) write(5000,*) itertmp,beta,sum(counts),limits(1),limits(size(limits))

end subroutine chisquare_plbinw


function chisquareplbinw(x,counts,limits)
! Auxiliary function for the routine chisquare_plbinw
implicit none 
double precision, dimension(2), intent(in) :: x
double precision, dimension(:), intent(in) :: counts ! number of counts per bin
double precision, dimension(size(counts)+1), intent(in) :: limits ! limits of the bins
double precision :: chisquareplbinw
double precision :: beta,tmp,stmp,a
integer :: i
! Here we add 1 so that 1-beta is always larger than 0,
! else the summand can be infinite.
! The exponent and normalisation have to be positive,
! thus the absolute
beta=abs(x(1))+1.0000001
a=abs(x(2))
stmp=0.
do i=1,size(counts)
	tmp=0
	if (counts(i)>0) then
		! Calculate the number of objects between limits(i) and limits(i+1)
		tmp=A/(1.-beta)*(limits(i+1)**(1.-beta)-limits(i)**(1.-beta))
		! Calculate the difference squared
		tmp=(log10(tmp)-log10(counts(i)) )**2.
		! Multiply with the weights
		tmp=tmp*counts(i)*sum(counts)/(sum(counts)-counts(i))/log10(exp(1.0))**2.
	end if
	! Add it to the sum
	stmp=stmp+tmp
end do
chisquareplbinw=stmp
end function chisquareplbinw


subroutine amoebavarbinestchi(p,y,ftol,iter,counts,limits)
implicit none
double precision, dimension(:), intent(in) :: counts ! number of counts per bin
double precision, dimension(size(counts)+1), intent(in) :: limits ! limits of the bins
double precision, dimension(:), intent(inout) :: y
double precision, dimension(:,:), intent(inout) :: p
double precision, intent(in) :: ftol
integer, intent(out) :: iter
integer, parameter :: itmax=1000
integer :: ihi,ndim
double precision, dimension(size(p,2)) :: psum

if (size(p,1)/=size(y)) call error("wrong dimensions")

call amoeba_private

contains

function func(x)
! Auxiliary function for the routine chisquare_plbinw
implicit none 
double precision, dimension(2), intent(in) :: x
double precision :: func
double precision :: beta,tmp,stmp,a
integer :: i
! Here we add 1 so that 1-beta is always larger than 0,
! else the summand can be infinite.
! The exponent and normalisation have to be positive,
! thus the absolute
beta=abs(x(1))+1.0000001
!!!!
!beta=x(1)+1.0000001
a=abs(x(2))
!beta=x(1)
!a=x(2)
stmp=0.
do i=1,size(counts)
	tmp=0
		if (counts(i)>0) then
		! Calculate the number of objects between limits(i) and limits(i+1)
		tmp=A/(1.-beta)*(limits(i+1)**(1.-beta)-limits(i)**(1.-beta))
		! Calculate the difference squared
		tmp=(log10(tmp)-log10(counts(i)) )**2.
		! Multiply with the weights
		tmp=tmp*counts(i)*sum(counts)/(sum(counts)-counts(i))/log10(exp(1.0))**2.
		end if
	! Add it to the sum
	stmp=stmp+tmp
end do
func=stmp
end function func




SUBROUTINE amoeba_private
IMPLICIT NONE
INTEGER :: i,ilo,inhi
double precision  :: rtol,ysave,ytry,ytmp
ndim=size(p,1)-1
iter=0
psum(:)=sum(p(:,:),dim=1)
do
	ilo=iminloc(y(:))
	ihi=imaxloc(y(:))
	ytmp=y(ihi)
	y(ihi)=y(ilo)
	inhi=imaxloc(y(:))
	y(ihi)=ytmp
	rtol=2.0*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo))+1.0d-10)
	if (rtol < ftol) then
		call swap(y(1),y(ilo))
		call swap(p(1,:),p(ilo,:))
		RETURN
	end if
!		if (iter >= ITMAX) call error('ITMAX exceeded in amoeba',iter)
	if (iter >= ITMAX) RETURN
	ytry=amotry(-1.0d0)
	iter=iter+1
	if (ytry <= y(ilo)) then
		ytry=amotry(2.0d0)
		iter=iter+1
	else if (ytry >= y(inhi)) then
		ysave=y(ihi)
		ytry=amotry(0.5d0)
		iter=iter+1
		if (ytry >= ysave) then
			p(:,:)=0.5*(p(:,:)+spread(p(ilo,:),1,size(p,1)))
			do i=1,ndim+1
				if (i /= ilo) y(i)=func(p(i,:))
			end do
			iter=iter+ndim
			psum(:)=sum(p(:,:),dim=1)
		end if
	end if
end do
END SUBROUTINE amoeba_private


FUNCTION amotry(fac)
IMPLICIT NONE
double precision, INTENT(IN) :: fac
double precision :: amotry
double precision :: fac1,fac2,ytry
double precision, DIMENSION(size(p,2)) :: ptry
fac1=(1.0-fac)/ndim
fac2=fac1-fac
ptry(:)=psum(:)*fac1-p(ihi,:)*fac2
ytry=func(ptry)
if (ytry < y(ihi)) then
	y(ihi)=ytry
	psum(:)=psum(:)-p(ihi,:)+ptry(:)
	p(ihi,:)=ptry(:)
end if
amotry=ytry
END FUNCTION amotry

end subroutine amoebavarbinestchi
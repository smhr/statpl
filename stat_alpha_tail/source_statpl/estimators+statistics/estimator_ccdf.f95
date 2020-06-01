subroutine ccdest (data,beta,mmine,mmaxe)
! Estimator for the parameters of a Pareto law using the complementary cumulative density plot
implicit none
double precision, dimension(:), intent(in) :: data
double precision, intent(out) :: beta,mmine,mmaxe

 call chisquare_ccd(data,beta,mmine,mmaxe)

end subroutine ccdest

subroutine chisquare_ccd (data,beta,xmin,xmax)
! Auxiliary subroutine for the subroutine ccdest
implicit none
double precision, dimension(:), intent(in) :: data 
double precision, intent(out) :: xmin,xmax,beta
double precision, dimension(4) :: y
double precision, dimension(4,3) :: p
integer :: iter,itertmp,restart,restartmax,itermax
double precision :: ftol,maxdat
! Tolerance for the amoeba
ftol=1.e-9

! Calculate an estimate of the normalisation
maxdat=maxval(data)
p(1,:)=(/2.d0,0.9d0*minval(data),1.1d0*maxval(data)/)

! Find the minimum
itertmp=0.
itermax=300
iter=itermax+1
restartmax=3
restart=1.

do 
! Restart at least 3 times
if (iter<itermax .and. restart>restartmax) exit
! If after 1000 iterations no convergence is reached, then
! restart.
! If after 100000 iterations = 10 restarts no convergence is reached,
! then exit with the last result
if (itertmp> itermax*restartmax) exit
! Initialise
p(1,:)=(/p(1,1),p(1,2),p(1,3)/)
p(2,:)=(/p(1,1),0.001d0,0.001d0/)
p(3,:)=(/0.001d0,p(1,2),0.001d0/)
p(4,:)=(/0.001d0,0.001d0,p(1,3)/)
y(1)=chisquareccd(p(1,:))
y(2)=chisquareccd(p(2,:))
y(3)=chisquareccd(p(3,:))
y(4)=chisquareccd(p(4,:))

 call amoebaccd(p,y,ftol,iter,maxdat,data)

itertmp=itertmp+iter
restart=restart+1
!if (abs(p(1,3)) > 4) restart=
end do




beta=abs(p(1,1))+1.
xmin= abs(p(1,2))
xmax=abs(p(1,3))+maxdat

!if (xmax>10*maxdat) print *,beta,xmax,maxdat,itertmp
if (itertmp>restartmax*itermax) print *,"XXXXXXXX CCD", itertmp,beta


if (xmax>10*maxdat) xmax=10.*maxdat

if (itertmp>restartmax*itermax) write(6000,*) itertmp,beta



contains

function chisquareccd(x)
! Auxiliary function for the subroutine chisquare_ccd
implicit none 
double precision, dimension(3) :: x
double precision :: chisquareccd
double precision :: betat,pmint,pmaxt,stmp,tmp
integer :: i
betat=-( abs(x(1)) + 0.00001)
pmint= abs(x(2))
pmaxt= abs(x(3))+maxdat
stmp=0.
do i=1,size(data)
	tmp=(data(i)**betat-pmint**betat)/(pmaxt**betat-pmint**betat)
	tmp=( log10(1.-tmp)-log10(1.-(float(i)-0.5)/float(size(data))) )**2.
	stmp=stmp+tmp
end do
 chisquareccd=stmp
end function chisquareccd

end subroutine chisquare_ccd



subroutine amoebaccd(p,y,ftol,iter,maxdat,data)
implicit none
double precision, dimension(:), intent(inout) :: y
double precision, dimension(:,:), intent(inout) :: p
double precision, intent(in) :: ftol
integer, intent(out) :: iter
double precision, dimension(:) :: data 
double precision :: maxdat
integer, parameter :: itmax=1000
integer :: ihi,ndim
double precision, dimension(size(p,2)) :: psum

if (size(p,1)/=size(y)) call error("wrong dimensions")

call amoeba_private

contains


function func(x)
! Auxiliary function for the subroutine chisquare_ccd
implicit none 
double precision, dimension(3) :: x
double precision :: func
double precision :: betat,pmint,pmaxt,stmp,tmp
integer :: i
betat=-( abs(x(1)) + 0.00001)
pmint= abs(x(2))
pmaxt= abs(x(3))+maxdat

stmp=0.
do i=1,size(data)
tmp=(data(i)**betat-pmint**betat)/(pmaxt**betat-pmint**betat)
tmp=(log10(1.-tmp)-log10(1.-(float(i)-0.5)/float(size(data))) )**2.
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

end subroutine amoebaccd



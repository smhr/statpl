module statpl_mod
! Module containing general subroutines for the program statpl
use utilmod
use mffunmod
use mathmod
use statfunmod
use mfestmod
implicit none
integer :: nestimators
parameter (nestimators=8)
integer :: ntests
parameter (ntests=14)
integer, parameter :: imodml=7


contains


subroutine calceststat (montecarlosize,n,beta,mmin,mmax,&
nbin,est_mask,eststat,significance,critval_inf,critval_trunc,power, &
stats_inf,stats_trunc)
! calculate the standard deviations for the estimators and critical values for the statistics
implicit none
integer, intent(in) :: montecarlosize
integer, intent(in) :: n ! number of data points
double precision, intent(in) :: beta,mmin,mmax
integer, intent(in) :: nbin
integer :: i,j,idum
double precision, dimension(nestimators), intent(inout) :: est_mask
double precision, dimension(nestimators,9) :: eststat
double precision, intent(in) :: significance
double precision, dimension(ntests), intent(out) :: critval_inf,critval_trunc,power
double precision, dimension(montecarlosize,ntests), intent(out) :: stats_inf,stats_trunc
double precision :: betatmp,mmintmp,mmaxtmp
double precision, dimension(:), allocatable :: masses
character (len=50), dimension(ntests) :: names
double precision, dimension(montecarlosize,ntests) :: alt
double precision, dimension(:,:,:), allocatable :: estout
double precision :: tmp,infinity
infinity=1.e30
idum=-1
tmp=ran2(idum)

critval_inf=0.
critval_trunc=0.
power=0.
stats_inf=0.
stats_trunc=0.

names=""

allocate(masses(n))

allocate(estout(nestimators,montecarlosize,3))
est_mask(imodml)=1.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do  j=1,montecarlosize ! Monte-Carlo loop for null hypothesis (not truncated power law)
	if (mod(j,10)==0) write (*,*) "Null monte carlo step=",j
	do i=1,size(masses)
		if (mod(i,10000)==0) write (*,*) "Null mass number=",i
		tmp=genmasspl(idum,beta,mmin,infinity)
		masses(i)=tmp
	end do ! generate masses
	call sort(masses)
	call modmlest (masses,betatmp,mmintmp,mmaxtmp)
	if (betatmp < 0 )then
		print *,"estimator did not work, beta=",betatmp," setting to input value",beta
		betatmp=beta
	end if
	mmaxtmp=infinity
	call calculate_statistics(masses,betatmp,mmintmp,mmaxtmp,stats_inf(j,:))
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call calculate_critval(stats_inf,significance,critval_inf,names)

alt=0.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do  j=1,montecarlosize ! Monte-Carlo loop for alternative hypothesis (truncated power law) and estimators
    ! smhr
    if (mod(j,10)==0) write (*,*) "Alternative monte carlo step=",j
    if (modulo(j,1) == 0) print *,j,float(j)/float(montecarlosize)
    do i=1,size(masses)
	if (mod(i,10000)==0) write (*,*) "Alternative mass number=",i
	tmp=genmasspl(idum,beta,mmin,mmax)
	masses(i)=tmp
    end do ! generate masses
    call sort(masses)
    call estimate_parameters(est_mask,masses,nbin,estout(:,j,:))
    betatmp=estout(imodml,j,1)
    if (betatmp < 0 )then
	print *,"estimator did not work, beta=",betatmp," setting to input value",beta
	betatmp=beta
    end if
    mmintmp=estout(imodml,j,2)
    mmaxtmp=estout(imodml,j,3)
    call calculate_statistics(masses,betatmp,mmintmp,mmaxtmp,stats_trunc(j,:))

    mmaxtmp=infinity
    call calculate_statistics(masses,betatmp,mmintmp,mmaxtmp,alt(j,:))

! call plotwhatever(masses,betamodml(j),mminmodml(j),mmaxmodml(j),2)
end do ! monte-carlo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do j=1,nestimators
	if (est_mask(j) > 0) then
		eststat(j,1)=average(estout(j,:,1))
		eststat(j,2)=stddeviation(estout(j,:,1))
		eststat(j,3)=eststat(j,1)-beta
		eststat(j,4)=average(estout(j,:,2))
		eststat(j,5)=stddeviation(estout(j,:,2))
		eststat(j,6)=eststat(j,4)-mmin
		eststat(j,7)=average(estout(j,:,3))
		eststat(j,8)=stddeviation(estout(j,:,3))
		eststat(j,9)=eststat(j,7)-mmax
	end if
end do


deallocate(estout)

 call calculate_critval(stats_trunc,significance,critval_trunc,names)

 call calculate_power(alt,critval_inf,power,names)
deallocate(masses)

end subroutine calceststat



subroutine name_statistics (names)
implicit none
character (len=50), dimension(:) :: names
integer :: i
i=1
write(names(i),'(A)') "D" ! 1
i=i+1
write(names(i),'(A)') "SD" ! 2
i=i+1
write(names(i),'(A)') "CC" ! 3
i=i+1
write(names(i),'(A)') "SCC" ! 4
i=i+1
write(names(i),'(A)') "AA" ! 5
i=i+1
write(names(i),'(A)') "RR" ! 6
i=i+1
write(names(i),'(A)') "kk" ! 7
i=i+1
write(names(i),'(A)') "kk0" ! 8
i=i+1
write(names(i),'(A)') "Skk" ! 9
i=i+1
write(names(i),'(A)') "Skk0" ! 10
i=i+1
write(names(i),'(A)') "W" ! 11
i=i+1
write(names(i),'(A)') "T" ! 12
i=i+1
write(names(i),'(A)') "LikRat" ! 13
i=i+1
write(names(i),'(A)') "X" ! 14
end subroutine name_statistics

subroutine calculate_statistics(dataset,beta,xmin,xmax,stats)
implicit none
double precision, dimension(:) :: dataset
double precision :: beta,xmin,xmax
double precision, dimension(:) :: stats
integer :: i
i=1
stats(i)=kspl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=kssppl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=wsquaredpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=wsquaredsppl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=asquaredpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=rsquaredpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=ksquaredpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=k0squaredpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=ksquaredsppl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=k0spsquaredsppl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=shapirowilkpl(dataset,beta,xmin,xmax)
i=i+1
stats(i)=-jackson(dataset,beta,xmin,xmax)
i=i+1
stats(i)=likelihoodratio(dataset,beta,xmin,xmax)
i=i+1
stats(i)=mmaxtest(dataset,beta,xmin,xmax)
!i=i+1
!stats(i)=loglikelihoodratio(dataset,beta,xmin,xmax)

end subroutine calculate_statistics



subroutine calculate_critval(null,significance,critval,names)
implicit none
double precision, dimension(:,:) :: null
double precision :: significance
double precision, dimension(size(null,2)) :: critval
double precision, dimension(size(null,1)) :: tmpn
integer :: i,icrit
character (len=50), dimension(:) :: names
icrit=floor(size(null,1)*(1.-significance))
!write(*,*) "Critical Values, alpha=",significance
do i=1,size(null,2)
	tmpn=null(:,i)
	call sort(tmpn)
	critval(i)=tmpn(icrit)
	null(:,i)=tmpn
end do
end subroutine calculate_critval



subroutine calculate_power(alt,critval,power,names)
implicit none
double precision, dimension(:,:) :: alt
double precision, dimension(size(alt,2)) :: power
double precision, dimension(size(alt,1)) :: tmpp
double precision, dimension(size(alt,2)) :: critval
integer :: i
character (len=50), dimension(:) :: names
do i=1,size(alt,2)
tmpp=1.
where ( alt(:,i) < critval(i) ) tmpp=0.
power(i)=sum(tmpp)/float(size(tmpp))*100
end do
end subroutine calculate_power


subroutine name_estimators(names)
implicit none
character (len=50), dimension(:) :: names
integer :: i
i=1 
write(names(i),'(A)') "Beg" ! 1
i=i+1
write(names(i),'(A)') "Beg rec" ! 2
i=i+1
write(names(i),'(A)') "Baxter (ML infinity)" ! 3
i=i+1
write(names(i),'(A)') "Const bin LR" ! 4
i=i+1
write(names(i),'(A)') "Var bin chi" ! 5
i=i+1
write(names(i),'(A)') "ML" ! 6
i=i+1
write(names(i),'(A)') "Mod ML" ! 7
i=i+1
write(names(i),'(A)') "CCDF" ! 8
end subroutine name_estimators


subroutine estimate_parameters(est_mask,masses,nbin,estout)
implicit none
double precision, dimension(:) :: est_mask
double precision, dimension(:) :: masses
double precision, dimension(:,:) :: estout
integer :: i,nbin
! print *,"salam estimate_parameters"
i=1
if (est_mask(i) > 0) call begest (masses,estout(i,1),estout(i,2),estout(i,3))
i=i+1
if (est_mask(i) > 0) call begest_recursive (masses,estout(i,1),estout(i,2),estout(i,3))
i=i+1
if (est_mask(i) > 0) call baxterest(masses,estout(i,1),estout(i,2));
i=i+1 
if (est_mask(i) > 0) call constbinestlr (nbin,masses,estout(i,1))
i=i+1
if (est_mask(i) > 0) call varbinestchi (nbin,masses,estout(i,1),estout(i,3))
i=i+1
if (est_mask(i) > 0) call mlest(masses,estout(i,1),estout(i,2),estout(i,3))
i=i+1
if (est_mask(i) > 0) call modmlest (masses,estout(i,1),estout(i,2),estout(i,3))
i=i+1
if (est_mask(i) > 0) call ccdest (masses,estout(i,1),estout(i,2),estout(i,3))
end subroutine estimate_parameters

end module statpl_mod

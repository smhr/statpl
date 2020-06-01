module mfestmod
! Subroutines to estimate the exponent and limits of a power-law probability
! density (known as Salpeter mass functn in Astronomy or Pareto law in economics)
!
! The exponent is estimated for x^(-beta), i.e. in without sign

use utilmod
use mffunmod
use mathmod
use statfunmod
implicit none

contains

! Calculate stabilized quantiles for one-tailed distributions
! Only a half transformation is used, i.e. [0.5,1] is mapped on [0.5,1]
function stabquant(quant)
implicit none
double precision :: stabquant,quant
double precision :: tmp
double precision, parameter :: pi=3.141592654
tmp=quant
tmp=0.5d0+0.5d0*tmp
tmp=2.d0/pi*asin(sqrt(tmp))
tmp=2.d0*tmp-1.d0
stabquant=tmp
end function stabquant

! Routines to perform the chisquare minimisation
include "chisquare_pl.f95"

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
!!!!!!! Estimators                              !!!!!!!
!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 1. Beg's estimator in its original form
include "estimators+statistics/estimator_beg.f95"
! 2. Beg's estimator in the recursive form
include "estimators+statistics/estimator_beg_recursive.f95"
! 3. Baxters uniformly minimum variance unbiased estimator for a untruncated power law
include "estimators+statistics/estimator_baxter.f95" 
! 4. Constant-size binning, using linear regression
include "estimators+statistics/estimator_constbins_lr.f95"
! 7. Variable-size binnung, using chi-square
include "estimators+statistics/estimator_varbins_chi.f95"
! 9. Maximum likelihood estimator
include "estimators+statistics/estimator_ml.f95"
! 11. A modified ML estimator, described in Maschberger and Kroupa
include "estimators+statistics/estimator_mod_ml.f95"
! 12. Complementary cumulative density estimator
include "estimators+statistics/estimator_ccdf.f95"

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
!!!!!!! Statistical tests                      !!!!!!!
!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! There are a lot more tests programmed that actually used in the paper.


! 1. Kolmogorov-Smirnov statistic
function kspl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: kspl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmpx(i)=tmp
	tmp=(float(i)-0.5)/float(size(x))
	tmpy(i)=tmp
end do
kspl=maxval(abs(tmpx-tmpy))
end function kspl

function kspluspl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: kspluspl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmp=(float(i)-0.5)/float(size(x))
	tmpx(i)=tmp
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmpy(i)=tmp
end do
kspluspl=maxval(tmpx-tmpy)
end function kspluspl

function ksminuspl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: ksminuspl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmp=(float(i)-0.5)/float(size(x))
	tmpx(i)=tmp
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmpy(i)=tmp
end do
ksminuspl=maxval(tmpy-tmpx)
end function ksminuspl


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 2. Kolmogorov Smirnov statistic for the stabilized probability plot
function kssppl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: kssppl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmp=stabquant(tmp)
	tmpx(i)=tmp
	tmp=(float(i)-0.5)/float(size(x))
	tmp=stabquant(tmp)
	tmpy(i)=tmp
end do
kssppl=maxval(abs(tmpx-tmpy))
end function kssppl


function ksplussppl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: ksplussppl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
ksplussppl=0.
do i=1,size(x)
	tmp=(float(i)-0.5)/float(size(x))
	tmp=stabquant(tmp)
	tmpx(i)=tmp
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmp=stabquant(tmp)
	tmpy(i)=tmp
end do
ksplussppl=maxval((tmpx-tmpy))
end function ksplussppl

function ksminussppl (x,beta,pmin,pmax)
implicit none
double precision, dimension(:), intent(inout) :: x
double precision :: ksminussppl
double precision :: beta,pmin,pmax
double precision, dimension(size(x)) :: tmpx,tmpy
double precision :: tmp
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmp=(float(i)-0.5)/float(size(x))
	tmp=stabquant(tmp)
	tmpx(i)=tmp
	tmp=intpdfpl(x(i),beta,pmin,pmax)
	tmp=stabquant(tmp)
	tmpy(i)=tmp
end do
ksminussppl=maxval(-(tmpx-tmpy))
end function ksminussppl



! 3.
function wsquaredpl (x,beta,pmin,pmax)
! W squared (Cramer-von Mises) statistic for a power law pdf
implicit none
double precision :: wsquaredpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: z
integer :: i
wsquaredpl=0.
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	z=intpdfpl(x(i),beta,pmin,pmax)
	z=z-float(2*i-1)/float(2*size(x))
	z=z**2.
	wsquaredpl=wsquaredpl+z
end do
wsquaredpl=wsquaredpl+1./float(12*size(x))
end function wsquaredpl


function wsquaredsppl (x,beta,pmin,pmax)
! W squared (Cramer-von Mises) statistic for a power law pdf
implicit none
double precision :: wsquaredsppl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: z,tmpa,tmpb
integer :: i
wsquaredsppl=0.
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
do i=1,size(x)
	tmpa=intpdfpl(x(i),beta,pmin,pmax)
	tmpa=stabquant(tmpa)
	tmpb=(float(i)-0.5)/float(size(x))
	tmpb=stabquant(tmpb)
	z=tmpa-tmpb
	z=z**2.
	wsquaredsppl=wsquaredsppl+z
end do
end function wsquaredsppl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 4.
function asquaredpl (x,beta,pmin,pmax)
! A squared (Anderson-Darling) statistic for a power law pdf
implicit none
double precision :: asquaredpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: z,z1,z2
integer :: i
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
asquaredpl=0.
do i=1,size(x)
	z1=log(intpdfpl(x(i),beta,pmin,pmax))
	z2=log(1.-intpdfpl(x(size(x)+1-i),beta,pmin,pmax))
	z=z1+z2
	z=float(2*i-1)*z
	asquaredpl=asquaredpl+z
end do
asquaredpl=-asquaredpl/float(size(x)) - float(size(x))
end function asquaredpl



!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


! 5.
function rsquaredpl (y,beta,pmin,pmax)
! correlation test qq plot
implicit none
double precision :: rsquaredpl
double precision, dimension(:), intent(inout) :: y
double precision, dimension(size(y)) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: xbar,qbar,rr,rrx,rrq,tmp
integer :: i
x=y
x=x/maxval(y)

xbar=0.
qbar=0.
do i=1,size(x)
xbar=xbar+x(i)
tmp=float(i)/float(size(x)+1)
qbar=qbar+invintpdfpl(tmp,beta,pmin,pmax)
end do
xbar=xbar/float(size(x))
qbar=qbar/float(size(x))

rr=0.
rrx=0.
rrq=0.
do i=1,size(x)
tmp=float(i)/float(size(x)+1)
tmp=invintpdfpl(tmp,beta,pmin,pmax)

rr=rr+(x(i)-xbar)*(tmp-qbar)
rrx=rrx + (x(i)-xbar)**2.
rrq=rrq + (tmp-qbar)**2.
end do

rsquaredpl=rr**2./rrx/rrq
rsquaredpl=rr/sqrt(rrx)/sqrt(rrq)
rsquaredpl=1.-rsquaredpl
end function rsquaredpl




!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 6.
function ksquaredpl (x,beta,pmin,pmax)
! k squared  statistic for a power law pdf
implicit none
double precision :: ksquaredpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: zp,zz,zbar,pp,pbar,tmp,z,p
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do
ksquaredpl=0.
n=size(x)

zbar=0.
pbar=0.
do i=1,n
tmp=intpdfpl(x(i),beta,pmin,pmax)
zbar=zbar+tmp
tmp=(float(i)-0.5)/float(n)
pbar=pbar+tmp
end do
zbar=zbar/float(n)
pbar=pbar/float(n)

zp=0.
pp=0.
zz=0.
do i=1,size(x)
z=intpdfpl(x(i),beta,pmin,pmax)
p=(float(i)-0.5)/float(n)
zp=zp+(z-zbar)*(p-pbar)
zz=zz+(z-zbar)**2.
pp=pp+(p-pbar)**2.
end do
ksquaredpl=zp**2./zz/pp
ksquaredpl=1.-ksquaredpl
end function ksquaredpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!


function ksquaredsppl (x,beta,pmin,pmax)
! k squared  statistic for a power law pdf stabilized probability
implicit none
double precision :: ksquaredsppl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: zp,zz,zbar,pp,pbar,tmp,z,p
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do
ksquaredsppl=0.
n=size(x)
zbar=0.
pbar=0.
do i=1,n
tmp=intpdfpl(x(i),beta,pmin,pmax)
tmp=stabquant(tmp)
zbar=zbar+tmp
tmp=(float(i)-0.5)/(float(n))
tmp=stabquant(tmp)
pbar=pbar+tmp
end do
zbar=zbar/float(n)
pbar=pbar/float(n)

zp=0.
pp=0.
zz=0.
do i=1,size(x)
tmp=intpdfpl(x(i),beta,pmin,pmax)
tmp=stabquant(tmp)
z=tmp
tmp=(float(i)-0.5)/(float(n))
tmp=stabquant(tmp)
p=tmp
zp=zp+(z-zbar)*(p-pbar)
zz=zz+(z-zbar)**2.
pp=pp+(p-pbar)**2.
end do
ksquaredsppl=zp**2./zz/pp
if (zz < 1.e-10) ksquaredsppl=0.99
ksquaredsppl=1.-ksquaredsppl
end function ksquaredsppl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 6.
function k0squaredpl (x,beta,pmin,pmax)
! k squared  statistic for a power law pdf
implicit none
double precision :: k0squaredpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: zp,zz,zbar,pp,pbar,tmp,z,p
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do
k0squaredpl=0.
n=size(x)

zbar=0.
pbar=0.
do i=1,n
tmp=intpdfpl(x(i),beta,pmin,pmax)
zbar=zbar+tmp
tmp=float(i)/(float(n)+1.)
pbar=pbar+tmp
end do
zbar=zbar/float(n)
pbar=pbar/float(n)

zbar=0.5
zbar=0.5


zp=0.
pp=0.
zz=0.
do i=1,size(x)
z=intpdfpl(x(i),beta,pmin,pmax)
p=float(i)/(float(n)+1.)
zp=zp+(z-zbar)*(p-pbar)
zz=zz+(z-zbar)**2.
pp=pp+(p-pbar)**2.
end do
k0squaredpl=zp**2./zz/pp
k0squaredpl=1.-k0squaredpl

end function k0squaredpl


function k0spsquaredsppl (x,beta,pmin,pmax)
! k squared  statistic for a power law pdf stabilized probability
implicit none
double precision :: k0spsquaredsppl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: zp,zz,zbar,pp,pbar,tmp,z,p
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do
k0spsquaredsppl=0.
n=size(x)
zbar=0.
pbar=0.
do i=1,n
tmp=intpdfpl(x(i),beta,pmin,pmax)
tmp=stabquant(tmp)
zbar=zbar+tmp
tmp=float(i)/(float(n)+1.)
tmp=stabquant(tmp)
pbar=pbar+tmp
end do
zbar=zbar/float(n)
pbar=pbar/float(n)

zbar=0.5
pbar=0.5

zp=0.
pp=0.
zz=0.
do i=1,size(x)
tmp=intpdfpl(x(i),beta,pmin,pmax)
tmp=stabquant(tmp)
z=tmp
tmp=float(i)/(float(n)+1.)
tmp=stabquant(tmp)
p=tmp
zp=zp+(z-zbar)*(p-pbar)
zz=zz+(z-zbar)**2.
pp=pp+(p-pbar)**2.
end do
k0spsquaredsppl=zp**2./zz/pp
k0spsquaredsppl=1.-k0spsquaredsppl
end function k0spsquaredsppl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 7.
function zpl (x,beta,pmin,pmax)
! z statistic for a power law pdf (Brain&Shapiro 1983, modified)
implicit none
double precision :: zpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: y,yy,yyy
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do

zpl=0.
n=size(x)
yy=0.
yyy=0.

y=float(n)*( log(x(1)) -log(pmin))
yy=yy+(float(1)-float(n)/2.)*y
yyy=yyy+y
do i=2,n
y=float(n-i+1)*( log(x(i)) - log(x(i-1)) ) ! BS
yy=yy+(float(i)-float(n)/2.)*y
yyy=yyy+y
end do
zpl=sqrt(12./(float(n)-2.))*yy/yyy

end function zpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 8.
function zqpl (x,beta,pmin,pmax)
! z statistic for a power law pdf (Brain+Shapiro 1983, modified)
implicit none
double precision :: zqpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
double precision :: y,yyy,yya
integer :: i,n
do i=1,size(x)-1
	if (x(i)>x(i+1)) call sort(x)
end do
zqpl=0.
n=size(x)
yyy=0.
yya=0.

y=float(n)*( log(x(1)) - log(pmin) )
yyy=yyy+y
yya=yya+(float(1)-float(n)/2.)**2.*y

do i=2,n
	y=float(n-i+1)*( log(x(i)) - log(x(i-1)) )
	yyy=yyy+y
	yya=yya+(float(i)-float(n)/2.)**2.*y
end do
zqpl=sqrt(5./4./( float(n+1)*float(n-2)*float(n-3) ) )*( 12.*yya - float(n)*float(n-2)*yyy )/yyy
end function zqpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
! 9.
function zstarpl (x,beta,pmin,pmax)
! z statistic for a power law pdf
implicit none
double precision :: zstarpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
zstarpl=zqpl(x,beta,pmin,pmax)**2. + zpl(x,beta,pmin,pmax)**2.
end function zstarpl

! 9.
function zstarstarpl (x,beta,pmin,pmax)
! z statistic for a power law pdf
implicit none
double precision :: zstarstarpl
double precision, dimension(:), intent(inout) :: x
double precision, intent(in) :: beta,pmin,pmax
zstarstarpl=zqpl(x,beta,pmin,pmax) + zpl(x,beta,pmin,pmax)
end function zstarstarpl


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

! 9.
function jackson(x,beta,pmin,pmax)

implicit none
double precision :: jackson
double precision, dimension(:), intent(inout) ::x
double precision, intent(in) :: beta,pmin,pmax
double precision :: t,xx,xxx
integer :: i,j,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do

jackson=0.
n=size(x)
xxx=0.
do i=1,n
xxx=xxx+log(x(i)/pmin)
end do
xx=0.
do i=1,n
t=0.
do j=1,i
t=t+1./float(n-j+1)
end do
xx=xx+t*log(x(i)/pmin)
end do
jackson=xx/xxx
end function jackson



function shapirowilkpl (x,beta,pmin,pmax)
implicit none
double precision :: shapirowilkpl
double precision, dimension(:), intent(inout) :: x
double precision, dimension(size(x)) :: y
double precision, intent(in) :: beta,pmin,pmax
double precision :: n,ybar,ss
integer :: i
y=x
do i=1,size(x)-1
if (y(i)>y(i+1)) call sort(y)
end do
y=log(y)
n=float(size(x))
ybar=sum(y)/n
ss=0.
do i=1,size(y)
ss=ss+(y(i)-ybar)**2
end do
shapirowilkpl=n*(ybar-y(1))**2./(n-1.)/ss
end function shapirowilkpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function moranpl (x,beta,pmin,pmax)
implicit none
double precision :: moranpl
double precision, dimension(:), intent(inout) :: x
double precision, dimension(size(x)) :: y
double precision, intent(in) :: beta,pmin,pmax
double precision :: yy,yyy,n
integer :: i
y=x
do i=1,size(x)-1
if (y(i)>y(i+1)) call sort(y)
end do
y=log(y/pmin)
n=float(size(y)-1)
yy=sum(y)/n

yyy=0.
do i=2,size(y)
yyy=yyy + ( log(y(i)) - log(yy) )
end do
moranpl=-2*yyy
end function moranpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function greenwoodpl (x,beta,pmin,pmax)
implicit none
double precision :: greenwoodpl
double precision, dimension(:), intent(in) :: x
double precision, dimension(size(x)) :: y
double precision, intent(in) :: beta,pmin,pmax
double precision :: yy,yyy
integer :: i
y=x
do i=1,size(x)-1
if (y(i)>y(i+1)) call sort(y)
end do
y=log(y/pmin)
yy=sum(y)
yyy=0.
do i=1,size(x)
yyy=yyy+ (y(i)/yy)**2.
end do
greenwoodpl=yyy

end function greenwoodpl

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

function epsteinpl (x,beta,pmin,pmax)
implicit none
double precision :: epsteinpl
double precision, dimension(:), intent(inout) :: x
double precision, dimension(size(x)) :: y
double precision, intent(in) :: beta,pmin,pmax
double precision :: yy,yyy,n,pmint,ytmp,tmp
integer :: i
y=x
do i=1,size(x)-1
if (y(i)>y(i+1)) call sort(y)
end do
pmint=pmin
if (pmin == y(1) ) pmint=pmin*0.999
y=log(y/pmint)
n=float(size(y))
yy=0.
yyy=0.
do i=1,size(y)-1
ytmp=y(i+1)
if (y(i+1) == y(i)) ytmp=y(i+1)*1.001
tmp=float(size(y)-i+1)*(ytmp-y(i))
yy=yy+log(tmp)
yyy=yyy+tmp
end do


epsteinpl=log(yyy/n)-yy/n
epsteinpl=2*n*epsteinpl/(1+(n+1)/6/n)

end function epsteinpl




function ppresidual (x,beta,pmin,pmax)
! k squared  statistic for a power law pdf
implicit none
double precision :: ppresidual
double precision, dimension(:), intent(inout) :: x
double precision, dimension(size(x)) :: y
double precision, intent(in) :: beta,pmin,pmax
double precision :: yy,yyy,ybar,p,z,tmp
integer :: i,n
do i=1,size(x)-1
if (x(i)>x(i+1)) call sort(x)
end do
n=size(x)
do i=1,size(x)
z=intpdfpl(x(i),beta,pmin,pmax)
!z=-log(1-z)
p=float(i)/(float(n)+1.)
y(i)=z-p
end do
ybar=sum(y)
!write(500,*) ybar
ybar=0.
yy=0.
yyy=0.
do i=1,size(x)
tmp=(y(i)-ybar)
yyy=yyy+tmp
yy=yy+tmp**2.
!yyy=yyy+tmp**4
end do
yy=1.
yy=yy/n
yyy=yyy/n
ppresidual=-yyy/yy**(2/2)
end function ppresidual


function mmaxtest(x,beta,pmin,pmax)
! Returns the largest data point
implicit none
double precision :: mmaxtest
double precision, dimension(:), intent(in) :: x
double precision, intent(in) :: beta,pmin,pmax
mmaxtest=-log10(maxval(x))
end function mmaxtest



function loglikelihoodratio(x,beta,pmin,pmax)
! Likelihood ratio test for truncated and not truncated PL
implicit none
double precision :: loglikelihoodratio
double precision, dimension(:), intent(in) :: x
double precision, intent(in) :: beta,pmin,pmax ! estimated values, truncated PL
double precision :: lh_trunc,lh_inf ! likelihoods
double precision :: beta_inf,pmin_inf
double precision :: tmp,kappa
integer :: i,n
n=size(x)
kappa=(1.d0-beta)/(pmax**(1.d0-beta) - pmin**(1.d0-beta))
lh_trunc=0.d0
do i=1,n
tmp=kappa*x(i)**(-beta)
lh_trunc=lh_trunc + log10(tmp)
end do

! estimate infinite parameters
call baxterest(x,beta_inf,pmin_inf)
kappa=-(1.d0-beta_inf)/pmin_inf**(1.d0-beta_inf)
lh_inf=0.d0
do i=1,n
tmp=kappa*x(i)**(-beta_inf)
lh_inf=lh_inf + log10(tmp)
end do
loglikelihoodratio=(lh_inf-lh_trunc)
end function loglikelihoodratio

function likelihoodratio(x,beta,pmin,pmax)
! Likelihood ratio test for truncated and not truncated PL
implicit none
double precision :: likelihoodratio
double precision, dimension(:), intent(in) :: x
double precision, intent(in) :: beta,pmin,pmax ! estimated values, truncated PL
double precision :: lh_trunc,lh_inf ! likelihoods
double precision :: beta_inf,pmin_inf,xmin
double precision :: kappa,kappa_inf
double precision :: t
integer :: n
n=size(x)
xmin=minval(x)
kappa=(1.d0-beta)/(pmax**(1.d0-beta) - xmin**(1.d0-beta))
lh_trunc=0.d0
t=0;
t=sum(log10(x))
lh_trunc=float(n)*log10(kappa)-beta*t
! estimate infinite parameters
call baxterest(x,beta_inf,pmin_inf)
kappa_inf=-(1.d0-beta_inf)/pmin_inf**(1.d0-beta_inf)
kappa_inf=-(1.d0-beta_inf)/xmin**(1.d0-beta_inf)
lh_inf=float(n)*log10(kappa_inf)-beta_inf*t
likelihoodratio=(lh_inf-lh_trunc)
end function likelihoodratio



end module mfestmod




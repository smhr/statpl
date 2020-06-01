!**************************************************************
! Statpl
! Author: Thomas Maschberger 
! Ref: Thomas Maschberger & Pavel Kroupa, 2009, MNRAS, 395

! Modification: S.Mohammad Hoseini Rad
! smhr313@gmail.com
! Nov 2012, IASBS, Zanjan
! Last modification: 16 Jul 2016
!**************************************************************
program statpl
use statfunmod
use mffunmod
use mfestmod
use statpl_mod
use utilmod
implicit none

integer :: i,j
double precision :: tmp,tmpx,tmpy,step
character(len=20) :: chartmp

double precision :: infinity

double precision, dimension(:), allocatable :: masses
! smhr !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
double precision, dimension(:), allocatable :: low_masses
double precision, dimension(:), allocatable :: high_masses
double precision, dimension(:), allocatable :: low_masses_in_rh
double precision, dimension(:), allocatable :: high_masses_in_rh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! input parameters
character (len=50) :: datafile     ! Name of main input file (output file of naus or nbody_reader)
! smhr !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 character (len=50)::low_masses_datafile
 character (len=50)::high_masses_datafile
 integer::nlow, nhigh
 integer::nlow_in_rh, nhigh_in_rh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer :: calcstddev
integer :: montecarlosize
integer :: nbin, nbin_orig
integer :: makeplots
double precision :: significance
double precision, dimension(nestimators) :: est_mask



! Results
integer :: ndata
! smhr !!!!!!!!!!!!!!!!!!!!!!!!!!
double precision :: lbeta,lmmin,lmmax!,stddev
double precision :: hbeta,hmmin,hmmax!,stddev
double precision :: lbeta_in_rh,lmmin_in_rh,lmmax_in_rh
double precision :: hbeta_in_rh,hmmin_in_rh,hmmax_in_rh
double precision :: betbinl,betbinh,betbinlh,betbinhh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
double precision :: dsp_inf,dsp_trunc


double precision, dimension(ntests) :: critval_inf,critval_trunc,power
double precision, dimension(ntests) :: statistics_data_inf,statistics_data_trunc
double precision, dimension(ntests,3,2) :: statval_data
double precision, dimension(:,:), allocatable :: stats_inf,stats_trunc
character (len=50), dimension(ntests) :: stat_names
character (len=50), dimension(nestimators) :: est_names
double precision, dimension(nestimators,3) :: estout
double precision, dimension(nestimators,9) :: eststat

! smhr !!!!!!!!!!!!!!!!!!!!!!!!!!!
integer::ll, hh, llh, hhh
real*8::rt, rh ! Tidal & half mass radius 
real*8::sim_time ! Time of simulation
real*8::Mtot, MNow, MNow_to_Mtot ! Total mass, Mass in the time of "sim_time"
real*8,dimension(:),allocatable::mx, my, mz , mr2d, mr3d ! x,y,z and position of stars
real*8,dimension(:),allocatable::vx, vy, vz, kstar, lum, radius
integer,dimension(:),allocatable::NAME
real*8,dimension(:),allocatable::lmx, lmy, lmz, hmx, hmy, hmz ! x,y and z of low mass and high mass stars
real*8,dimension(:),allocatable::lmx_in_rh, lmy_in_rh, lmz_in_rh, hmx_in_rh, hmy_in_rh, hmz_in_rh ! x,y and z of low mass and high mass stars inside Rh.

real*8::imf_mmin, imf_mmax
real*8::total_high_masses
imf_mmin = 0.55
imf_mmax = 0.85
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
!!!!!!! Initialise
!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
infinity=1.e30
est_names=""
call name_estimators (est_names)
stat_names=""
call name_statistics (stat_names)

eststat=0.
statistics_data_inf=0.
statistics_data_trunc=0.

total_high_masses = 0.
! open(1000,file="statmfout.txt",action="write")



!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
!!!!!!! Read the parameters
!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
! Name of the data files
! write(*,*) "Data file:"
read(*,*) datafile
! smhr !!!!!!!!!!!
read(*,*) low_masses_datafile
read(*,*) high_masses_datafile
! open(500,file=low_masses_datafile)
! open(600,file=high_masses_datafile)
write(*,*) "using ",datafile
! write(1000,*) "Data file: "
! write(1000,*) datafile

read(*,*) nbin_orig
! 
! do i=1,nestimators
! 	read(*,*) est_mask(i)
! end do

! read(*,*) calcstddev
! read(*,*) montecarlosize
! read(*,*) significance
! read(*,*) makeplots
!!!!!!!!!!!!!!!!!
! smhr
read(*,*) Mtot
read(*,*) MNow
read(*,*) rt ! Tidal radius
read(*,*) rh ! Half-mass radius
read(*,*) sim_time ! Time of simulation
!!!!!!!!!!!!!!!!!

! allocate(stats_inf(montecarlosize,ntests))
! allocate(stats_trunc(montecarlosize,ntests))


!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
!!!!!!! Read the data
!!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!

ndata=numberoflines(datafile)
write(*,*) "Number of data"
write(*,*) ndata

allocate(masses(ndata))
! smhr !!!!!!!!!!!!!!!!!!!!!!!!
allocate (mx(ndata)); allocate (my(ndata)); allocate (mz(ndata))
allocate (mr2d(ndata)); allocate (mr3d(ndata))
allocate (vx(ndata)); allocate (vy(ndata)); allocate (vz(ndata))
allocate (kstar(ndata)); allocate (lum(ndata)); allocate (radius(ndata)); allocate(NAME(ndata))

nlow = 0; nhigh = 0; nlow_in_rh = 0; nhigh_in_rh = 0
open(1,file=datafile,action="read",status="old")

!kstar = 0 ! I donn't have kstar, so all stars are considered

do i=1,ndata     ! Reading all data from datafile and saving them in appropriate arrays.
!     read (1,*) NAME(i), mx(i), my(i), mz(i), vx(i), vy(i), vz(i), kstar(i), lum(i), radius(i), masses(i)
    read (1,*) NAME(i), masses(i), mx(i), my(i), mz(i), vx(i), vy(i), vz(i), kstar(i), lum(i), radius(i)
!     read (1,*) NAME(i), masses(i), mx(i), my(i), mz(i), vx(i), vy(i), vz(i)
	mr3d(i) = dsqrt(mx(i)*mx(i) + my(i)*my(i) + mz(i)*mz(i)) ! 3-D radius
        mr2d(i) = dsqrt((mx(i)*mx(i) + my(i)*my(i)))! + sqrt(mx(i)*mx(i) + mz(i)*mz(i)) + sqrt(my(i)*my(i) + mz(i)*mz(i)))/3. ! Mean projected radius
	if ( ( mr3d(i) - rt ) <= 1e-5 ) then ! Check if the star is in the 3D tidal radius, then do:
		if ( kstar(i) < 10 ) then
			if ( (masses(i) - imf_mmin) < 1e-5 ) then
				nlow = nlow + 1
! 				if ( ( mr3d(i) - rh ) <= 1e-5 ) nlow_in_rh = nlow_in_rh + 1 
			elseif ( (masses(i) - imf_mmin) >= 1e-5 .and. masses(i) <= imf_mmax ) then
				nhigh = nhigh + 1
! 				if ( ( mr3d(i) - rh ) <= 1e-5 ) nhigh_in_rh = nhigh_in_rh + 1
			endif
		endif
	endif
end do
print*,"nlow & nhigh =", nlow, nhigh
print*,"nlow_in_rh & nhigh_in_rh =", nlow_in_rh, nhigh_in_rh
!!!!!!!!!!!!!!!!!!!!!!
allocate(low_masses(nlow)); allocate(high_masses(nhigh)) ! Allocate low_masses & high_masses arrays inside tidal radius.
allocate (lmx(nlow)); allocate (lmy(nlow)); allocate (lmz(nlow)) ! Allocate low mass coordinates arrays.
allocate (hmx(nhigh)); allocate (hmy(nhigh)); allocate (hmz(nhigh)) ! Allocate high masse coordinates arrays
!!!!!!!!!!!!!!!!!!!!!!
! allocate(low_masses_in_rh(nlow_in_rh)); allocate(high_masses_in_rh(nhigh_in_rh)) ! Allocate low_masses & high_masses arrays inside Rh.
! allocate (lmx_in_rh(nlow_in_rh)); allocate (lmy_in_rh(nlow_in_rh)); allocate (lmz_in_rh(nlow_in_rh)) ! Allocate low mass coordinates arrays inside Rh.
! allocate (hmx_in_rh(nhigh_in_rh)); allocate (hmy_in_rh(nhigh_in_rh)); allocate (hmz_in_rh(nhigh_in_rh)) ! Allocate high masse coordinates arrays inside Rh.
!!!!!!!!!!!!!!!!!!!!!!

ll = 0; hh = 0; llh = 0; hhh = 0
do i=1,ndata     ! Writing data in appropriate arrays according to their masses and positions inside or outside of Rh. 
	if ( ( mr3d(i) - rt ) <= 1e-5 ) then ! Check if the star is in the 2D tidal radius, then do:
		if ( kstar(i) < 10 ) then
			if ( (masses(i) - imf_mmin) < 1e-5 ) then
				ll = ll + 1
				low_masses(ll) = masses(i); lmx(ll) = mx(i); lmy(ll) = my(i); lmz(ll) = mz(i)
!  				write (*,*) low_masses(ll), lmx(ll), lmy(ll), lmz(ll)
! 				if ( ( mr3d(i) - rh ) <= 1e-5 ) then
! 					llh = llh + 1
! 					print*,"llh", llh
! 					low_masses_in_rh(llh) = masses(i); lmx_in_rh(llh) = mx(i); lmy_in_rh(llh) = my(i); lmz_in_rh(llh) = mz(i)
! 				endif
			elseif ( (masses(i) - imf_mmin) >= 1e-5 .and. masses(i) <= imf_mmax ) then
				hh = hh + 1
				high_masses(hh) = masses(i); hmx(hh) = mx(i); hmy(hh) = my(i); hmz(hh) = mz(i)
                                total_high_masses = total_high_masses + high_masses(hh)
! 				write (600,*) high_masses(hh), hmx(hh), hmy(hh), hmz(hh)
! 				if ( ( mr3d(i) - rh ) <= 1e-5 ) then
! 					hhh = hhh + 1
! 					print*,"hhh", hhh
! 					high_masses_in_rh(hhh) = masses(i); hmx_in_rh(hhh) = mx(i); hmy_in_rh(hhh) = my(i); hmz_in_rh(hhh) = mz(i)
! 				endif
			endif
		endif
	endif
end do

 close(1)
!  close(500)
!  close(600)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 call sort(low_masses)
 call sort(high_masses)
!  call sort(low_masses_in_rh)
!  call sort(high_masses_in_rh)
! 
write(*,*) "Number of low_masses data is", size(low_masses)
write(*,*) "Minimum of low_masses data is", low_masses(1)
write(*,*) "Maximum of low_masses data is", low_masses(nlow)
write(*,*) "Number of high_masses data is", size(high_masses)
write(*,*) "Minimum of high_masses data is", high_masses(1)
write(*,*) "Maximum of high_masses data is", high_masses(nhigh)

! 
! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
! !!!!!!! Call the Estimators
! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!! !!!!!!!
! 

!!!!!!!!!!!!!!!!!!!!!!!!! For low mass stars inside Rt
 nbin = nbin_orig
 estout=0
 call modmlest (low_masses,lbeta,lmmin,lmmax)
 if (nbin_orig<0) nbin=floor(2*size(low_masses)**(2./5.))
 write(*,*) "Used number of low mass star bins:", nbin
 call constbinestlr (nbin,low_masses,betbinl)

!!!!!!!!!!!!!!!!!!!!!!!!!!! For high mass stars inside Rt
 nbin = nbin_orig
 estout=0
 call modmlest (high_masses,hbeta,hmmin,hmmax)
 if (nbin_orig<0) nbin=floor(2*size(high_masses)**(2./5.))
 write(*,*) "Used number of high mass star bins:", nbin
 call constbinestlr (nbin,high_masses,betbinh)
 print*,"salam1",nbin,betbinh
!!!!!!!!!!!!!!!!!!!!!!!!!! For low mass stars inside Rh
!  nbin = nbin_orig
!  estout=0
!  print*,"========="
!  print*,"low_masses_in_rh",low_masses_in_rh
!  
!  call modmlest (low_masses_in_rh,lbeta_in_rh,lmmin_in_rh,lmmax_in_rh)
!  print*,"salam2",lbeta_in_rh,lmmax_in_rh
!  if (nbin_orig<0) nbin=floor(2*size(low_masses_in_rh)**(2./5.))
!  write(*,*) "Used number of low mass star bins inside Rh:", size(low_masses_in_rh), nbin
!  call constbinestlr (nbin,low_masses_in_rh,betbinlh)

!!!!!!!!!!!!!!!!!!!!!!!!!!
!  nbin = nbin_orig
!  estout=0
!  call modmlest (high_masses_in_rh,hbeta_in_rh,hmmin_in_rh,hmmax_in_rh)
!  if (nbin_orig<0) nbin=floor(2*size(high_masses_in_rh)**(2./5.))
!  write(*,*) "Used number of high mass star bins inside Rh:", size(high_masses_in_rh), nbin
!  call constbinestlr (nbin,high_masses_in_rh,betbinhh)

!!!!!!!!!!!!!!!!!!!!!!!!!!
MNow_to_Mtot = MNow/Mtot
!!!!!!!!!!!!!!!!!!!!!!!!!
lbeta_in_rh = 0; hbeta_in_rh = 0
lmmin_in_rh = 0; lmmax_in_rh = 0
hmmin_in_rh = 0; hmmax_in_rh = 0


open(700, file='alpha_mml', ACCESS='append')
open(800, file='alpha_constbin', ACCESS='append')
write(*,'(a72)')"======================================================================="
write(*,'(a70)')"     Time       MTot         MNow           Mnow/Mtot  mmin< Mnow <mm"
write (*,'(5f13.4)') sim_time, MTot, MNow, MNow_to_Mtot, total_high_masses
write(*,'(a39)')"===============   MML   ==============="
write(*,'(a42)')"    lbeta, hbeta, lbeta_in_rh, hbeta_in_rh"
write (*,'(4f10.3)') lbeta, hbeta, lbeta_in_rh, hbeta_in_rh
write(*,'(a39)')"============   ConstBin   ============="
write(*,'(a39)')"   betbinl, betbinh, betbinlh, betbinhh"
write (*,'(4f10.3)') betbinl,betbinh,betbinlh,betbinhh 
write(*,'(a44)')"============================================"
write (700,'(15f10.3)') sim_time, MNow_to_Mtot, lbeta, hbeta, lbeta_in_rh, hbeta_in_rh, lmmin, lmmax, hmmin, hmmax, lmmin_in_rh,&
&lmmax_in_rh, hmmin_in_rh , hmmax_in_rh, total_high_masses
write (800,'(f13.3,6f10.3)') sim_time, MNow_to_Mtot,betbinl,betbinh,betbinlh,betbinhh,total_high_masses

end program statpl

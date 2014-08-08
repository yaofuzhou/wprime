c----------------------------------------------------------------------
C  couplings.f 
c----------------------------------------------------------------------
c  This files takes the inputs of the standard model from a Les Houches 
c  file (param_card.dat) and calculates all the couplings that end up
c  in the Feynman rules, i.e., in the HELAS routines.
c   
c  With the convention of the New Web Generation MadEvent in this
c  part no non-trivial computation is made. The SM model secondary
c  parameters have been all calculated by the SM calculator, SMCalc
c  which produced param_card.dat.
c
c  The only exception to the above rule is for alpha_S. In the case
c  where a pdf is used in the initial state, then the values as(MZ)
c  set in the les houches card is superseeded.
c  Scales and pdf's are set in the run_card.dat.
c
c This file contains the following routines:
c 
c- madgraph original call to set the parameters
c- lh_readin is called from here.
c  It talks to the lh_readin through the common block values.
c      subroutine setpara
c
c-routine to read the LH parameters in
c      subroutine lh_readin
c
c-to set
c      subroutine set_it(npara,ivalue,value,name,id,
c      subroutine case_trap(string,length)
c      subroutine no_spaces(buff,nchars)
c---------------------------------------------------------------------- 


      subroutine setpara(param_name,readlha)
c***********************************************************************
c This subroutine sets up the HELAS couplings of the STANDARD MODEL.
c***********************************************************************
      implicit none
c
c local
c
      character*(*) param_name
      logical readlha
      integer i
      real*8 dum
c
c     common file with the couplings
c
      include 'coupl.inc'
      include 'input.inc'
      include 'CHECKMODEL.inc'
c
c     local
c
      double precision  v
      double precision  ee, ee2, ez, ey, sw, cw, sc2, sin2w, wm
      double precision  gwne, gwud, lambda, lam4, xt, rew, rqcd
      double precision  alphas, alfa, alfaw, mfrun
      double precision  wpwidthf, tmpreal, tmpimag
      external          alphas, alfa, alfaw, mfrun
      external          wpwidthf
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
c     !CKM matrix elements
c      common/values/    alpha,gfermi,alfas,   
c     &                  mtMS,mbMS,mcMS,mtaMS,
c     &                  Vud

      common/values/    alpha,gfermi,alfas,
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb


c
c constants
c
      double complex  ci
      parameter( ci = ( 0.0d0, 1.0d0 ) )
      double precision  Zero, One, Two, Three, Four, Half, Rt2
      parameter( Zero = 0.0d0, One = 1.0d0, Two = 2.0d0 )
      parameter( Three = 3.0d0, Four = 4.0d0, Half = 0.5d0 )
      parameter( Rt2   = 1.414213562d0 )
      double precision  Pi, Fourpi
      parameter( Pi = 3.14159265358979323846d0 )
      parameter( Fourpi = Four * Pi )
c
c     alfas and run
c************
c Uncomment the following lines in order to use alphas from the PDF
c      include '../alfas.inc'
c      include '../run.inc'
c***********
c------------------------------------------
c Start calculating the couplings for HELAS
c------------------------------------------
c
      if(readlha) then 
         call lh_readin(param_name)
         G = DSQRT(4d0*PI*ALFAS) ! use setting of the param_card.dat @ NLO
      endif
c     

      GG(1) = -G
      GG(2) = -G     
c
c auxiliary local values
c
      wm = sqrt(zmass**2/Two+
     $     sqrt(zmass**4/Four-Pi/Rt2*alpha/gfermi*zmass**2))
      sin2w  = One-(wm/zmass)**2
      cw  = sqrt( One - sin2w )
      ee2 = alpha * Fourpi
      sw  = sqrt( sin2w )
      ee  = sqrt( ee2 )
      ez  = ee/(sw*cw)
      ey  = ee*(sw/cw)
      sc2 = sin2w*( One - sin2w )
      v   = Two*wm*sw/ee   ! the wmass is used to calculate v
      lambda = hmass**2 / (Two * v**2)
c
c vector boson couplings
c
      gw   = ee/sw
      gwwa = ee
      gwwz = ee*cw/sw
c
c gauge & higgs boson coupling constants
c
      gwwh  = dcmplx( ee2/sin2w*Half*v, Zero )
      gzzh  = dcmplx( ee2/sc2*Half*v, Zero )
      ghhh  = dcmplx( -hmass**2/v*Three, Zero )
      gwwhh = dcmplx( ee2/sin2w*Half, Zero )
      gzzhh = dcmplx( ee2/sc2*Half, Zero)
      ghhhh = ghhh/v
c
c fermion-fermion-vector couplings
c
      gal(1) = dcmplx(  ee          , Zero )
      gal(2) = dcmplx(  ee          , Zero )
      gau(1) = dcmplx( -ee*Two/Three, Zero )
      gau(2) = dcmplx( -ee*Two/Three, Zero )
      gad(1) = dcmplx(  ee/Three    , Zero )
      gad(2) = dcmplx(  ee/Three    , Zero )

ca      gwf(1) = dcmplx( -ee/sqrt(Two*sin2w), Zero )
      gwf(1) = dcmplx(  -dsqrt(2d0*dsqrt(2d0)*(wmass**2)*gfermi), Zero )
      gwf(2) = dcmplx(  Zero              , Zero )
cc      write(*,*) gwf(1)
      gsmsu2l=gwf(1)

      gzn(1) = dcmplx( -ez*Half                     , Zero )
      gzn(2) = dcmplx(  Zero                        , Zero )
      gzl(1) = dcmplx( -ez*(-Half + sin2w)          , Zero )
      gzl(2) = dcmplx( -ey                          , Zero )
      gzu(1) = dcmplx( -ez*( Half - sin2w*Two/Three), Zero )
      gzu(2) = dcmplx(  ey*Two/Three                , Zero )
      gzd(1) = dcmplx( -ez*(-Half + sin2w/Three)    , Zero )
      gzd(2) = dcmplx( -ey/Three                    , Zero )

c fermion-fermion-Higgs couplings (complex) hff(2)
c
c NOTE: the running mass is evaluated @ the same order 
c nloop of alpha_s set by the PDF choice
c 

      if(mtMS.gt.1d0) then
         ghtop(1) = dcmplx( -mtMS/v, Zero )
      else
         ghtop(1) = dcmplx( Zero,Zero)
      endif
      ghtop(2) = ghtop(1)

      if(mbMS.gt.1d0) then
         ghbot(1) = dcmplx( -mbMS/v, Zero )
      else
         ghbot(1) = dcmplx( Zero, Zero )
      endif
      ghbot(2) = ghbot(1)
      
      if(mcMS.gt.1d0) then
         ghcha(1) = dcmplx( -mcMS/v, Zero )
      else
         ghcha(1) = dcmplx( Zero, Zero )
      endif
      ghcha(2) = ghcha(1)

      ghtau(1) = dcmplx( -mtaMS/v, Zero )
      ghtau(2) = ghtau(1)

c
c     CKM matrix: 
c     symmetric 3x3 matrix, Vud=Vcs, Vus=Vcd Vcb=Vub=0
c
c     >>>>>>>>>>>>>>>***** NOTE****<<<<<<<<<<<<<<<<<<<<<<<<<
c     these couplings matter only when interaction_CKM.dat
c     is used to generate all the diagrams with off-diagonal
c     couplings. The default of MadEvent is a diagonal
c     CKM matrix.

c	  Vus=DSQRT(1d0-Vud**2)
c      do i=1,2
c         gwfc(i) = gwf(i)*Vud
c         gwfs(i) = gwf(i)*Vus
c         gwfm(i) =-gwf(i)*Vus
c      enddo

c from Models/smckm/couplings.f
      do i=1,2
         gwfud(i) = gwf(i)*Vud
         gwfus(i) = gwf(i)*Vus
         gwfub(i) = gwf(i)*Vub
         gwfcd(i) = gwf(i)*Vcd
         gwfcs(i) = gwf(i)*Vcs
         gwfcb(i) = gwf(i)*Vcb
         gwftd(i) = gwf(i)*Vtd
         gwfts(i) = gwf(i)*Vts
         gwftb(i) = gwf(i)*Vtb
      enddo
cc                        print *,'  gwf(1)=',gwf(1)
cc                        print *,'     vtb=',vtb
cc                        print *,'gwftb(1)=',gwftb(1)

         smqckm(1)=gwfud(1)
         smqckm(2)=gwfus(1)
         smqckm(3)=gwfub(1)
         smqckm(4)=gwfcd(1)
         smqckm(5)=gwfcs(1)
         smqckm(6)=gwfcb(1)
         smqckm(7)=gwftd(1)
         smqckm(8)=gwfts(1)
         smqckm(9)=gwftb(1)

cc                        do i=1,9
cc                        print *,smqckm(i)
cc                        enddo

         smlckm(1)=gwf(1)
         smlckm(2)=dcmplx(0d0,Zero)
         smlckm(3)=dcmplx(0d0,Zero)
         smlckm(4)=dcmplx(0d0,Zero)
         smlckm(5)=gwf(1)
         smlckm(6)=dcmplx(0d0,Zero)
         smlckm(7)=dcmplx(0d0,Zero)
         smlckm(8)=dcmplx(0d0,Zero)
         smlckm(9)=gwf(1)

c---------------------------------------------------------
c Set Photon Width to Zero, used by symmetry optimization
c---------------------------------------------------------

      awidth = 0d0
c************************************            
c W' couplings
c************************************

      WPVUD(1)=gwfud(1)*glscale
      WPVUS(1)=gwfus(1)*glscale
      WPVUB(1)=gwfub(1)*glscale
      WPVCD(1)=gwfcd(1)*glscale
      WPVCS(1)=gwfcs(1)*glscale
      WPVCB(1)=gwfcb(1)*glscale
      WPVTD(1)=gwftd(1)*glscale
      WPVTS(1)=gwfts(1)*glscale
      WPVTB(1)=gwftb(1)*glscale

      WPVUD(2)=gwfud(1)*grscale
      WPVUS(2)=gwfus(1)*grscale
      WPVUB(2)=gwfub(1)*grscale
      WPVCD(2)=gwfcd(1)*grscale
      WPVCS(2)=gwfcs(1)*grscale
      WPVCB(2)=gwfcb(1)*grscale
      WPVTD(2)=gwftd(1)*grscale
      WPVTS(2)=gwfts(1)*grscale
      WPVTB(2)=gwftb(1)*grscale

      WPVEVE(1)=glscale*gwf(1)
      WPVMVM(1)=glscale*gwf(1)
      WPVTVT(1)=glscale*gwf(1)
      WPVEVM(1)=dcmplx(0d0,Zero)
      WPVEVT(1)=dcmplx(0d0,Zero)
      WPVMVE(1)=dcmplx(0d0,Zero)
      WPVMVT(1)=dcmplx(0d0,Zero)
      WPVTVE(1)=dcmplx(0d0,Zero)
      WPVTVM(1)=dcmplx(0d0,Zero)

      WPVEVE(2)=dcmplx(0d0,Zero)
      WPVMVM(2)=dcmplx(0d0,Zero)
      WPVTVT(2)=dcmplx(0d0,Zero)
      WPVEVM(2)=dcmplx(0d0,Zero)
      WPVEVT(2)=dcmplx(0d0,Zero)
      WPVMVE(2)=dcmplx(0d0,Zero)
      WPVMVT(2)=dcmplx(0d0,Zero)
      WPVTVE(2)=dcmplx(0d0,Zero)
      WPVTVM(2)=dcmplx(0d0,Zero)

      if(model.ne.0)then
      CALL READMODELF
      CALL CONVERTMODELF
      
      endif

      wpwidth = WPWIDTHF(0)

cc      CALL LOGMODELF
c----------------------------
c end subroutine coupsm
c----------------------------


      return
      end
 

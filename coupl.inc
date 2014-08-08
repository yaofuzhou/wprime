

c Define common block containing all coupling constants and masses
c  which are used in the HELAS routines.
c
c  These include masses, widths and real/complex couplings.
c
c  This file can be built automatically from particles.dat and
c  interactions.dat
c
c====================================================================
c
c
c
c     QCD 
c
      double complex       gg(2)
      double precision     g
      common /COUPL_QCD/   g,gg   
c
c    kinematical masses
c
      double precision     hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
      common /COUPL_MASS/  hmass, wmass, zmass, amass,
     &                     tmass, bmass, lmass, cmass
c
c   widths
c
      double precision     hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
      common /COUPL_WIDTH/ hwidth, wwidth, zwidth, 
     &                     twidth, lwidth, awidth
c
c  couplings in the feynman rules
c
      double complex       gal(2), gad(2), gau(2), gwf(2),
     &                     gzn(2), gzl(2), gzd(2), gzu(2)
      double precision     gw, gwwa, gwwz
      common /COUPL_GAUGE/ gal   , gad   , gau   , gwf   ,
     &                     gzn   , gzl   , gzd   , gzu   ,
     &                     gw, gwwa, gwwz
c
c CKM Matrix with non-diagonal elements
      double complex       gwfud(2),  gwfcd(2), gwftd(2),
     &                     gwfus(2),  gwfcs(2), gwfts(2),
     &                     gwfub(2),  gwfcb(2), gwftb(2)
      common /coupl_ckm/   gwfud,     gwfcd   , gwftd,
     &                     gwfus,     gwfcs   , gwfts,
     &                     gwfub,     gwfcb   , gwftb
c
      double complex       gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
      common /COUPL_SCAL/  gwwh, gzzh, gwwhh, gzzhh, ghhh, ghhhh
c
      double complex       ghtop(2), ghbot(2), ghtau(2), ghcha(2)
      common /COUPL_YUK/   ghtop   , ghbot   , ghtau   , ghcha

c W-Prime Boson GCKM Matrices for qq'W' and ll'W' couplings
      double complex   WPVUD(2),WPVCS(2),WPVTB(2),
     &                 WPVCD(2),WPVTD(2),WPVUS(2),
     &                 WPVTS(2),WPVUB(2),WPVCB(2),
     &                 WPVEVE(2),WPVMVM(2),WPVTVT(2),
     &                 WPVEVM(2),WPVEVT(2),WPVMVE(2),
     &                 WPVMVT(2),WPVTVE(2),WPVTVM(2)
      common /DC_COUPL/ WPVUD,WPVCS,WPVTB,
     &                  WPVCD,WPVTD,WPVUS,
     &                  WPVTS,WPVUB,WPVCB,
     &                  WPVEVE,WPVMVM,WPVTVT,
     &                  WPVEVM,WPVEVT,WPVMVE,
     &                  WPVMVT,WPVTVE,WPVTVM
      double precision WPMASS
      double precision WPWIDTH
      common /WP_MASS/  WPMASS
      common /WP_DECAY/ WPWIDTH

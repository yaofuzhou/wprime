c WPWIDTHF(dummy) takes W' Boson GCKM Matrices
c W' mass, tmass, gwf, glscale, grscale, etc.
c from common blocks and returns the W' width.
c Partial widths, and branching ratios, R_{u},
c R_{t} are also calculated and are stored in
c common blocks.
      double precision function WPWIDTHF(dummy)
c      program WPWIDTHF
      implicit none

      include 'CHECKMODEL.inc'
      include 'input.inc'
      
c W-Prime Boson GCKM Matrices for qq'W' and ll'W' couplings
c from coupl.inc
      include 'coupl.inc'

      double precision lwpwidth, rwpwidth
      double precision PI
      parameter(PI=3.141592653589793238d0)

      double precision dummy
      double precision msq
      double precision coef1, coef2, coef3, topbetasq
      double precision v2rq(9), v2lq(9), v2ll(9) 
      double precision smqckm2(9), smlckm2(9)
      double precision gratio
      integer i, j
    
  
      print *,'Calculating W'' width...'
c Calculating W' width...
      coef1 = WPMASS/8d0/PI

      rqwidth(1) = coef1*MSQ(WPVUD(2))
      rqwidth(2) = coef1*MSQ(WPVUS(2))
      rqwidth(3) = coef1*MSQ(WPVUB(2))
      rqwidth(4) = coef1*MSQ(WPVCD(2))
      rqwidth(5) = coef1*MSQ(WPVCS(2))
      rqwidth(6) = coef1*MSQ(WPVCB(2))
      lqwidth(1) = coef1*MSQ(WPVUD(1))
      lqwidth(2) = coef1*MSQ(WPVUS(1))
      lqwidth(3) = coef1*MSQ(WPVUB(1))
      lqwidth(4) = coef1*MSQ(WPVCD(1))
      lqwidth(5) = coef1*MSQ(WPVCS(1))
      lqwidth(6) = coef1*MSQ(WPVCB(1))

      coef2 = coef1/3d0

      llwidth(1) = coef2*MSQ(WPVEVE(1))
      llwidth(2) = coef2*MSQ(WPVEVM(1))
      llwidth(3) = coef2*MSQ(WPVEVT(1))
      llwidth(4) = coef2*MSQ(WPVMVE(1))
      llwidth(5) = coef2*MSQ(WPVMVM(1))
      llwidth(6) = coef2*MSQ(WPVMVT(1))
      llwidth(7) = coef2*MSQ(WPVTVE(1))
      llwidth(8) = coef2*MSQ(WPVTVM(1))
      llwidth(9) = coef2*MSQ(WPVTVT(1))
c==========================================================
c There is no right-handed neutrino in our scope
c==========================================================
      rqwidth(7) = 0d0
      rqwidth(8) = 0d0
      rqwidth(9) = 0d0
      lqwidth(7) = 0d0
      lqwidth(8) = 0d0
      lqwidth(9) = 0d0

      if(WPMASS.GT.tmass)then
c if WPMASS >= tmass, W-Prime can decay into top quark
      topbetasq = (1d0-(tmass**2)/(WPMASS**2))**2
      coef3 = (WPMASS**2+tmass**2/2d0)*topbetasq/WPMASS/8d0/PI

      rqwidth(7) = coef3*MSQ(WPVTD(2))
      rqwidth(8) = coef3*MSQ(WPVTS(2))
      rqwidth(9) = coef3*MSQ(WPVTB(2))
      lqwidth(7) = coef3*MSQ(WPVTD(1))
      lqwidth(8) = coef3*MSQ(WPVTS(1))
      lqwidth(9) = coef3*MSQ(WPVTB(1))

      endif

      WPWIDTHF = 0d0

      do i = 1, 9
         WPWIDTHF = WPWIDTHF + rqwidth(i)
         WPWIDTHF = WPWIDTHF + lqwidth(i)
         WPWIDTHF = WPWIDTHF + llwidth(i)
      enddo

      print *,'W'' width calculation DONE'
c 'W' width calculation DONE


c Calculating branching ratios...
      if(sets(4).eqv..true.) then
      print *,'Calculating branching ratios...'
      do i = 1, 9
         qwidth(i) = rqwidth(i) + lqwidth(i)
      enddo

      do i = 1, 9
         rqbr(i) = rqwidth(i) / WPWIDTHF
         lqbr(i) = lqwidth(i) / WPWIDTHF
         llbr(i) = llwidth(i) / WPWIDTHF
      enddo

      do i = 1, 9
      qbr(i) = rqbr(i) + lqbr(i)
      enddo
      print *,'Branching ratios'' calculations DONE'
      endif
c Branching ratios' calculations DONE


c Calculating R_{t} and R_{u}...
      if((sets(5).eqv..true.).or.(sets(6).eqv..true.)) then
      print *,'Calculating R_{t}''s and R_{u}''s...'
      v2rq(1) = MSQ(WPVUD(2))
      v2rq(2) = MSQ(WPVUS(2))
      v2rq(3) = MSQ(WPVUB(2))
      v2rq(4) = MSQ(WPVCD(2))
      v2rq(5) = MSQ(WPVCS(2))
      v2rq(6) = MSQ(WPVCB(2))
      v2rq(7) = MSQ(WPVTD(2))
      v2rq(8) = MSQ(WPVTS(2))
      v2rq(9) = MSQ(WPVTB(2))
      v2lq(1) = MSQ(WPVUD(1))
      v2lq(2) = MSQ(WPVUS(1))
      v2lq(3) = MSQ(WPVUB(1))
      v2lq(4) = MSQ(WPVCD(1))
      v2lq(5) = MSQ(WPVCS(1))
      v2lq(6) = MSQ(WPVCB(1))
      v2lq(7) = MSQ(WPVTD(1))
      v2lq(8) = MSQ(WPVTS(1))
      v2lq(9) = MSQ(WPVTB(1))
      v2ll(1) = MSQ(WPVEVE(1))
      v2ll(2) = MSQ(WPVEVM(1))
      v2ll(3) = MSQ(WPVEVT(1))
      v2ll(4) = MSQ(WPVMVE(1))
      v2ll(5) = MSQ(WPVMVM(1))
      v2ll(6) = MSQ(WPVMVT(1))
      v2ll(7) = MSQ(WPVTVE(1))
      v2ll(8) = MSQ(WPVTVM(1))
      v2ll(9) = MSQ(WPVTVT(1))
cc                        print *,'  WPVTB(1)=',WPVTB(1)
cc                        print *,'   v2lq(9)=',v2lq(9)

      do i=1,9
      smqckm2(i)=MSQ(smqckm(i))
      smlckm2(i)=MSQ(smlckm(i))
      enddo
cc                        print *,'-------'
cc                        print *,'    gwf(1)=',gwf(1)
cc                        print *,smqckm(9)
cc                        print *,' smqckm(9)=',smqckm(9)

      gratio=cf(1)/dsqrt(2d0*MSQ(gsmsu2l))

      qqsm=.true.
      qlsm=.true.
      do i=1,9
        do j=1,9
        qqru(i,j)=((v2rq(i)+v2lq(i))
     &            *(v2rq(j)+v2lq(j))
     &            -(v2rq(i)-v2lq(i))
     &            *(v2rq(j)-v2lq(j)))/16d0
        qqrt(i,j)=((v2rq(i)+v2lq(i))
     &            *(v2rq(j)+v2lq(j))
     &            +(v2rq(i)-v2lq(i))
     &            *(v2rq(j)-v2lq(j)))/16d0
        qlru(i,j)=((v2rq(i)+v2lq(i))*v2ll(j)
     &            +(v2rq(i)-v2lq(i))*v2ll(j))/16d0
        qlrt(i,j)=((v2rq(i)+v2lq(i))*v2ll(j)
     &            -(v2rq(i)-v2lq(i))*v2ll(j))/16d0

        if(dabs(qqru(i,j)).gt.1d-10)then

        qqsm=.false.
        gqqscale(i,j)=-99999d0
        else
        gqqscale(i,j)=gratio*
     &  dsqrt(((v2rq(i)+v2lq(i))*(v2rq(j)+v2lq(j)))/
     &               (smqckm2(i)*smqckm2(j))        )
cc                        print *,gqqscale(i,j)
        endif

        if(dabs(qlru(i,j)).gt.1d-10)then
        qlsm=.false.
        gqlscale(i,j)=-99999d0
        else if((j.eq.1).or.(j.eq.5).or.(j.eq.9))then
        gqlscale(i,j)=gratio*
     &  dsqrt(v2rq(i)+(v2lq(i))*v2ll(j)/(smqckm2(i)*smlckm2(j)))
cc                        print *,v2ll(i)
cc                        print *,gqlscale(2,5)
        else
        endif

        enddo
      enddo
cc                        print *,gqlscale(2,5)
cc                        print *,dsqrt(2d0*MSQ(gwf(1)))
      print *,'R_{t}''s and R_{u}''s calculations DONE'
      endif
c R_{t} and R_{u} calculations DONE

      return
      end
c function WPWIDTHF


c MSQ() tekes a (double precision) complex number and
c returns the modulus squared with double precision
      double precision function MSQ(acomplexnum)
      double complex acomplexnum
c      double precision MSQ
      MSQ=dble(acomplexnum)*dble(acomplexnum)+
     &    dimag(acomplexnum)*dimag(acomplexnum)
      return
      end
c function MSQ

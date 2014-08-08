      subroutine CONVERTMODELF
      implicit none
      include 'CHECKMODEL.inc'
      include 'coupl.inc'
      include 'input.inc'

      double precision rqconst, lqconst, llconst,gwpf(2)
cc                        double precision tempgwf

      print *, 'Converting variables...'

      gwpf(1) = dcmplx(-cf(1)*cf(3), 0d0)/dsqrt(2d0)
      gwpf(2) = dcmplx(-cf(1)*cf(2), 0d0)/dsqrt(2d0)
     &        *cdexp(dcmplx(0d0,cf(4)))
c     /dsqrt(2d0) is to comply with gwf MadEvent convention
c is g correct in the model file?

      rqconst = grscale*gwpf(2)*cf(5)

      WPVUD(2)=rqconst*dcmplx(rqr(1),rqi(1))
      WPVUS(2)=rqconst*dcmplx(rqr(2),rqi(2))
      WPVUB(2)=rqconst*dcmplx(rqr(3),rqi(3))
      WPVCD(2)=rqconst*dcmplx(rqr(4),rqi(4))
      WPVCS(2)=rqconst*dcmplx(rqr(5),rqi(5))
      WPVCB(2)=rqconst*dcmplx(rqr(6),rqi(6))
      WPVTD(2)=rqconst*dcmplx(rqr(7),rqi(7))
      WPVTS(2)=rqconst*dcmplx(rqr(8),rqi(8))
      WPVTB(2)=rqconst*dcmplx(rqr(9),rqi(9))

      lqconst = glscale*gwpf(1)*cf(6)

      WPVUD(1)=lqconst*dcmplx(lqr(1),lqi(1))
      WPVUS(1)=lqconst*dcmplx(lqr(2),lqi(2))
      WPVUB(1)=lqconst*dcmplx(lqr(3),lqi(3))
      WPVCD(1)=lqconst*dcmplx(lqr(4),lqi(4))
      WPVCS(1)=lqconst*dcmplx(lqr(5),lqi(5))
      WPVCB(1)=lqconst*dcmplx(lqr(6),lqi(6))
      WPVTD(1)=lqconst*dcmplx(lqr(7),lqi(7))
      WPVTS(1)=lqconst*dcmplx(lqr(8),lqi(8))
      WPVTB(1)=lqconst*dcmplx(lqr(9),lqi(9))
cc                        tempgwf=-cf(1)*cf(3)!/dsqrt(2d0)
cc                        print *,' tempgwf=',tempgwf
cc                        print *,'  gwpf(1)=',gwpf(1)
cc                        print *,'WPVTB(1)=',WPVTB(1)
      llconst = glscale*gwpf(1)*cf(7)

      WPVEVE(1)=llconst*dcmplx(llr(1),lli(1))
      WPVEVM(1)=llconst*dcmplx(llr(2),lli(2))
      WPVEVT(1)=llconst*dcmplx(llr(3),lli(3))
      WPVMVE(1)=llconst*dcmplx(llr(4),lli(4))
      WPVMVM(1)=llconst*dcmplx(llr(5),lli(5))
      WPVMVT(1)=llconst*dcmplx(llr(6),lli(6))
      WPVTVE(1)=llconst*dcmplx(llr(7),lli(7))
      WPVTVM(1)=llconst*dcmplx(llr(8),lli(8))
      WPVTVT(1)=llconst*dcmplx(llr(9),lli(9))

      print *, 'Variables conversion DONE'

      end

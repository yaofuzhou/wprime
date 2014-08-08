      subroutine READMODELF
      implicit none
      include 'CHECKMODEL.inc'
      include 'coupl.inc'
      include 'input.inc'

      logical done, first
      integer iunit, i, imax, icf, irq, ilq, ill, isets,
     &        first1, first2, last1, last2
      character*132 buff, buffin
      character*24 buffin1, buffin2

      integer ioresult

      iunit = 77
      icf = 1
      irq = 1
      ilq = 1
      ill = 1
      isets = 1

      print *,'model not equals to zero, reading wpmodelfile...'

      open(unit=iunit, file=wpmodelfile, status='old',
     &iostat=ioresult)
      if (ioresult .ne. 0) then
      open(unit=iunit, file='../../Source/MODEL/'//wpmodelfile,
     &status='old',iostat=ioresult)
      endif
      if (ioresult .ne. 0) then
      open(unit=iunit, file='../../../Source/MODEL/'//wpmodelfile,
     &status='old',iostat=ioresult)
      endif
      if (ioresult .ne. 0) then
      open(unit=iunit, file='../Source/MODEL/'//wpmodelfile,
     &status='old',iostat=ioresult)
      endif
      
      done=.false.

      do while(.not.done)

      read(iunit,'(a132)',end=2301,err=2301) buffin
cc      print *,buffin

c get rid of everything beyound #
      if((len(buffin).eq.0) .or. (index(buffin(1:132),'#').eq.1)) then
cc                              print *, '7777777777'
      goto 2302
      else if(index(buffin(1:132),'#').eq.0) then
      imax = 132
      else
      imax = index(buffin(1:132),'#')-1
      endif
      buff(1:imax) = buffin(1:imax)
c get rid of everything beyound #

      if(buff(1:3).eq.'EOF') then !1
cc      print *,'yeah!!!'
      done=.true.
      goto 2302
      else !1

      first=.true.
      i=1
      first1=1
      last1=1
      first2=1
      last2=1
      do while(i.lt.imax)

        if(buff(i:i).ne.' ') then !2
cc                              print *, '1111111111'
          if(first.eqv..true.) then !3
cc                              print *, '6666666666'
          first1 = i
          last1 = first1
          do while(last1.lt.imax.and.buff(last1+1:last1+1).ne.' ')
          last1 = last1 + 1
cc                              print *, '2222222222'
          enddo
          i = last1
          first2 = last1
          last2 = first2
          first=.false.
cc                              print *, '4444444444'
          else !3
cc                              print *, '5555555555'
          first2 = i
          last2 = first2
cc                              print *,'i=',i
cc                              print *,'first2=',first2
cc                              print *,'last2=',last2
          do while(last2.lt.imax.and.buff(last2+1:last2+1).ne.' ')
          last2 = last2 + 1
cc                              print *, '3333333333'
cc                              print *,'last2=',last2
          enddo
          i = last2
          endif !3
cc                              print *, first1, last1
cc                              print *, buff(first1:last1)
cc                              print *, first2, last2
cc                              print *, buff(first2:last2)
        endif !2
        i=i+1
cc                              print *, first1, last1
      enddo
cc                              print *, first1, last1
      endif !1
cc                              print *, first1, last1

      if((buff(first1:last1).eq.'.true.').or.
     &   (buff(first1:last1).eq.'.false.')) then
      read(buff(first1:last1),*) sets(isets)
      isets = isets + 1
      else if((first2.ne.1).and.(first2.eq.last1)) then
      read(buff(first1:last1),*) cf(icf)
cc          print *, first1, last1, first2, last2, buff(first1:last1)
cc                              print *, cf(icf)
      icf = icf + 1

      else if(irq.ne.10) then
      read(buff(first1:last1),*) rqr(irq)
      read(buff(first2:last2),*) rqi(irq)
cc                              print *,rqr(irq)
      irq = irq + 1
      else if(ilq.ne.10) then
      read(buff(first1:last1),*) lqr(ilq)
      read(buff(first2:last2),*) lqi(ilq)
cc                              print *,lqr(ilq)
      ilq = ilq + 1
      else if(ill.ne.10) then
      read(buff(first1:last1),*) llr(ill)
      read(buff(first2:last2),*) lli(ill)
cc                              print *,llr(ill)
      ill = ill + 1
      endif

cc 2302 continue
 2302 enddo
c     do while loop for the entire file

cc 2301 continue
 2301 if(done.eqv..false.) then
      print *, 'ERROR reading wpmodelfile'
      else
      print *, 'wpmodelfile reading DONE'
      endif
      close(iunit)

cc      do icf=1,7
cc      print *,cf(icf)
cc      enddo
cc      do isets=1,6
cc      print *,sets(isets)
cc      enddo

      end

           subroutine lh_readin(param_name)
c----------------------------------------------------------------------
c Read the parameters from the lh file
c
c 1. Input values for the EW sector
c 2. Higgs mass and width
c 3. Fermion masses (pole and MSbar) and widths
c----------------------------------------------------------------------
      implicit none
c
c     parameters
c
      integer maxpara
      parameter (maxpara=100)
c
c     local
c
	  character*(*) param_name	
          integer npara,l1,l2,id
cz buffin
          character*132 buff, buffin
          real*8 real_value
          real*8 value(maxpara)
          integer ivalue(maxpara),n
          character*20 name(maxpara),bn
          logical block_found,done,fopened
      integer iunit,i,name_length,idum
cz
      integer iparam,ioffset,ilast
      logical gotname
      character*1 null
      parameter  (null=' ')
cz //
      logical found
c
c       block info
c
      character*20 block_name
c
c   Common
c
          include 'coupl.inc'
	  include 'input.inc'
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
c     !CKM matrix elements
      common/values/    alpha,gfermi,alfas,
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb

c
c----------
c     start
c----------
c
c     open file
c
      iunit=14
      call open_file_mdl(iunit,param_name,fopened)
           done=.false.

cz Use parameter id
      iparam = 0

       n=0
           do while(.not.done)
           block_found=.false.
c
c looks for the blocks or for decay
c
      do while(.not.block_found)
cz buffin
       read(iunit,'(a132)',end=99,err=99) buffin
       buff(1:132) = buffin(1:132)
c--     change to lower case
           call case_trap(buff,20)
       if(buff(1:5).eq.'block') then
         if(index(buff,"#").ne.0) l1=index(buff,"#")-1
         block_name=buff(6:min(l1,26))
         call no_spaces(block_name,name_length)
c        write(*,*) block_name(1:name_length)
         block_found=.true.
           elseif(buff(1:5).eq.'decay') then
               n=n+1
               l1=30
               if(index(buff,"#").ne.0) l1=index(buff,"#")-1 ! ignore comments
               read(buff(6:l1),*) ivalue(n),value(n)
               name(n)="decay"
       endif
      end do
c
c

      if(block_found) then
 	  do while(.true.)
cz buffin
       read(iunit,'(a132)',end=99,err=99) buffin
       buff(1:132) = buffin(1:132)
     	  call case_trap(buff,20)
          if(buff(1:1).eq.'b'.or.buff(1:1).eq.'d') then
          	backspace iunit
          	exit
          endif	         	
            if(buff(1:1).ne.'#'.and.buff.ne.'') then  !if it not a comment
              n=n+1	       
              l1=30
              if(index(buff,"#").ne.0) l1=index(buff,"#")-1 ! ignore comments       
c
c  WARNING:... not all blocks have the same syntax!! You need to change it
c  depending on the block you are reading
c
             if(block_name(1:5).eq."mgckm") then
                read(buff(1:l1),*) ivalue(n),idum,value(n)
cz
              else if(block_name(1:6).eq."mguser") then
                 iparam=iparam+1
                 if (iparam.eq.4) then
                    gotname = .false.
                    ioffset = 0
                    ilast = 0
                    do i=1,132
                       if(buffin(i:i).ne.null) then
                          if (gotname) then
                             if (ioffset.eq.0) ioffset = i
                             ilast = i
                          endif
                          gotname = .true.
                       else if(ilast .gt. 0) then
                          goto 98
                       endif
                    enddo

 98                 wpmodelfile(1:ilast-ioffset+1)=buffin(ioffset:ilast)
                 else
                    read(buff(1:l1),*) ivalue(n),value(n)
                 endif
cz //
              else
                read(buff(1:l1),*) ivalue(n),value(n)
              endif
              name(n)=block_name(1:name_length)
c              write(*,"(1x,i2,2x,e16.8,1x,a)")
c     &        ivalue(n),value(n),name(n)
                  endif
      end do ! do while in the block
      else
        done=.true.
      endif
      end do ! do while the entire file

 99    continue      
	
       bn="sminputs"
       call set_it(n,ivalue,value,name,1,bn,alpha,128.9d0)
       alpha=1d0/alpha
       call set_it(n,ivalue,value,name,2,bn,gfermi,0.1166d-4)
       call set_it(n,ivalue,value,name,3,bn,alfas,0.119d0)
       call set_it(n,ivalue,value,name,4,bn,zmass,91.188d0)
       call set_it(n,ivalue,value,name,6,bn,tmass,174.3d0)
       call set_it(n,ivalue,value,name,7,bn,lmass,1.777d0)
       bn="mgyukawa"
       call set_it(n,ivalue,value,name,4,bn,mcMS,1.25d0)
       call set_it(n,ivalue,value,name,5,bn,mbMS,4.2d0)
       call set_it(n,ivalue,value,name,6,bn,mtMS,174d0)
       call set_it(n,ivalue,value,name,15,bn,mtaMS,1.777d0)
       bn="mgckm"
       call set_it(n,ivalue,value,name,11,bn,vud,1d0)
       call set_it(n,ivalue,value,name,12,bn,vus,0d0)
       call set_it(n,ivalue,value,name,13,bn,vub,0d0)
       call set_it(n,ivalue,value,name,21,bn,vcd,0d0)
       call set_it(n,ivalue,value,name,22,bn,vcs,1d0)
       call set_it(n,ivalue,value,name,23,bn,vcb,0d0)
       call set_it(n,ivalue,value,name,31,bn,vtd,0d0)
       call set_it(n,ivalue,value,name,32,bn,vts,0d0)
       call set_it(n,ivalue,value,name,33,bn,vtb,1d0)
       bn="mass"
       call set_it(n,ivalue,value,name,4,bn,cmass,1.4d0)
       call set_it(n,ivalue,value,name,5,bn,bmass,4.7d0)
       call set_it(n,ivalue,value,name,6,bn,tmass,tmass*1d0)
       call set_it(n,ivalue,value,name,15,bn,lmass,lmass*1d0)
       call set_it(n,ivalue,value,name,25,bn,hmass,120d0)
       call set_it(n,ivalue,value,name,23,bn,zmass,zmass*1d0)
       call set_it(n,ivalue,value,name,24,bn,wmass,80.419d0)
       call set_it(n,ivalue,value,name,-34,bn,wpmass,500d0)
       bn="decay"
       call set_it(n,ivalue,value,name,6,bn,twidth,1.5083d0)
       call set_it(n,ivalue,value,name,25,bn,hwidth,0.0037d0)
       call set_it(n,ivalue,value,name,23,bn,zwidth,2.441d0)
       call set_it(n,ivalue,value,name,24,bn,wwidth,2.0476d0)

       call set_it(n,ivalue,value,name,-34,bn,wpwidth,16.195d0)
c wpwidth will be reevaluated by WPWIDTH.f
       bn='mguser'
       call set_it(n,ivalue,value,name,1,bn,model         ,0d0)
       call set_it(n,ivalue,value,name,2,bn,glscale       ,1d0)
       call set_it(n,ivalue,value,name,3,bn,grscale       ,0d0)
c       call set_it(n,ivalue,value,name,4,bn,wpmodelfile   ,0d0)
       if (wpmodelfile .eq. '') wpmodelfile='SMLeftRight.wpmod'      

      return
      end

      
      subroutine set_it(npara,ivalue,value,name,id,
     &                  block_name,var,def_value)
c----------------------------------------------------------------------------------
c     finds the parameter value  in block_name and associate var to it.
c     If it is not found a default is given.
c----------------------------------------------------------------------------------
      implicit none

c
c     parameters
c
      integer maxpara
      parameter (maxpara=100)
c
c     arguments
c
      integer npara,ivalue(maxpara),id
      character*20  block_name,name(maxpara)
      real*8 var,def_value,value(maxpara)
c
c     local
c
      logical found
      integer i
c
c     start
c
	  found=.false.
      do i=1,npara
         found = (id.eq.ivalue(i)).and.(name(i).eq.block_name)
 	               if(found) then
         	var=value(i)
            exit
          endif	
      enddo
      
      if (.not.found) then
c         write (*,*) "Warning: parameter not found"
c         write (*,*) "         setting it to default value ",def_value
         var=def_value
      endif
      return

      end
      
      
      subroutine case_trap(string,length)
c**********************************************************    
c change string to lowercase if the input is not
c**********************************************************
      implicit none
c
c     ARGUMENT
c      
      character*(*) string
      integer length
c
c     LOCAL
c
      integer i,k

      do i=1,length
         k=ichar(string(i:i))
         if(k.ge.65.and.k.le.90) then  !upper case A-Z
            k=ichar(string(i:i))+32   
            string(i:i)=char(k)        
         endif
      enddo

      return
      end

c********************************************************************
           subroutine open_file_mdl(lun,filename,fopened)
c***********************************************************************
c     opens file input-card.dat in current directory or above
c***********************************************************************
      implicit none
c
c     Arguments
c
      integer lun
      logical fopened
      character*(*) filename
      character*90  tempname
      integer fine
      integer dirup,i

c-----
c  Begin Code
c-----
c
c     first check that we will end in the main directory
c
      tempname=filename
      fine=index(tempname,' ')
      if(fine.eq.0) fine=len(tempname)
      tempname=tempname(1:fine)
      open(unit=lun,file=tempname,status='old',ERR=10)
      return
c
c         if I have to read a card
c
 10   if(index(filename,"_card").gt.0) then
         tempname='Cards/'//tempname
      endif

      fopened=.false.
      do i=0,5
         open(unit=lun,file=tempname,status='old',ERR=30)
         fopened=.true.
         write(*,*) 'read model file',tempname
         exit
 30      tempname='../'//tempname
         if (i.eq.5)then
            write(*,*) 'Warning: file ',tempname,' is not correct'
            stop
         endif
      enddo


      return
      end
 



c********************************************************************
      subroutine no_spaces(buff,nchars)
c**********************************************************************
c     Given buff a buffer of words separated by spaces
c     returns it where all space are moved to the right
c     returns also the length of the single word.
c     maxlength is the length of the buffer
c**********************************************************************
      implicit none
c
c     Constants
c
      integer    maxline
      parameter (maxline=20)
      character*1 null
      parameter  (null=' ')
c
c     Arguments
c
      character*(maxline) buff
      integer nchars,maxlength
c
c     Local
c
      integer i,j
      character*(maxline) temp
c-----
c  Begin Code
c-----
      nchars=0
c      write (*,*) "buff=",buff(1:maxlength)
      do i=1,maxline
         if(buff(i:i).ne.null) then
            nchars=nchars+1
            temp(nchars:nchars)=buff(i:i)
         endif
c         write(*,*) i,":",buff(1:maxlength),":",temp(1:nchars),":"
      enddo
      buff=temp      
      end

      

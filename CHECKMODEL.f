      PROGRAM CHECKMODELF
      implicit none
      include 'CHECKMODEL.inc'

      CALL SETPARA('param_card.dat',.true.)
      if(sets(1).eqv..true.) then
      CALL LOGMODELF
      endif

      END

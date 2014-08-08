      subroutine LOGMODELF
      implicit none
      include 'CHECKMODEL.inc'
      include 'coupl.inc'
      include 'input.inc'

      integer i, j
      double precision srq, slq, sll
 63   format(1x,a4,' = ( ',d15.9,' , ',d15.9, ' )')
 64   format(1x,a8,' = ',d15.9,' GeV')
 65   format(1x,'g = ',d15.9,' GeV')
 66   format(1x,a46,' = ',d15.9)
 67   format(1x,a21,' = ',d15.9)
 68   format(1x,'i\\f',4x,a2,6x,a2,6x,a2,6x,a2,6x,a2,6x,a2,6x,a2,
     &                6x,a2,6x,a2)
 69   format(1x,a2,2x,d7.1,1x,d7.1,1x,d7.1,1x,d7.1,1x,d7.1,
     &             1x,d7.1,1x,d7.1,1x,d7.1,1x,d7.1)
 70   format(1x,'i\\f',3x,a4,4x,a4,4x,a4,4x,a4,4x,a4,4x,a4,4x,a4,
     &                4x,a4,4x,a4)
 71   format(1x,a18,11x,a5,9x,a5,5x,a14)
 72   format(1x,a3,' -> W'' -> ',a9,':',3x,d11.5,3x,d11.5,3x,d11.5)

      print *, 'Log enabled, printing log...'
      print *, '================ Beginning of Log ================'

      if(sets(2).eqv..true.) then
      write(*,65) cf(1)
      srq = cf(2)*cf(5)*grscale
      slq = cf(3)*cf(6)*glscale
      sll = cf(3)*cf(7)*glscale
      write(*,66) 'Overall muliplier of g for right-handed quarks', srq
      write(*,66) 'Overall muliplier of g for left-handed quarks ', slq
      write(*,66) 'Overall muliplier of g for left-handed leptons', sll
      write(*,66) 'CP-violating phase in Radians                 ', cf(4)
      print *, '--------------------------------------------------'
	print *, 'CKM matrix for right-handed quarks:'
      write(*,63) 'VubR', rqr(1), rqi(1)
      write(*,63) 'VusR', rqr(2), rqi(2)
      write(*,63) 'VudR', rqr(3), rqi(3)
      write(*,63) 'VcbR', rqr(4), rqi(4)
      write(*,63) 'VcsR', rqr(5), rqi(5)
      write(*,63) 'VcdR', rqr(6), rqi(6)
      write(*,63) 'VtbR', rqr(7), rqi(7)
      write(*,63) 'VtsR', rqr(8), rqi(8)
      write(*,63) 'VtdR', rqr(9), rqi(9)
	print *, 'CKM matrix for left-handed quarks:'
      write(*,63) 'VubL', lqr(1), lqi(1)
      write(*,63) 'VusL', lqr(2), lqi(2)
      write(*,63) 'VudL', lqr(3), lqi(3)
      write(*,63) 'VcbL', lqr(4), lqi(4)
      write(*,63) 'VcsL', lqr(5), lqi(5)
      write(*,63) 'VcdL', lqr(6), lqi(6)
      write(*,63) 'VtbL', lqr(7), lqi(7)
      write(*,63) 'VtsL', lqr(8), lqi(8)
      write(*,63) 'VtdL', lqr(9), lqi(9)
	print *, 'CKM matrix for left-handed leptons:'
      write(*,63) 'Veve', llr(1), lli(1)
      write(*,63) 'Vevm', llr(2), lli(2)
      write(*,63) 'Vevt', llr(3), lli(3)
      write(*,63) 'Vmve', llr(4), lli(4)
      write(*,63) 'Vmvm', llr(5), lli(5)
      write(*,63) 'Vmvt', llr(6), lli(6)
      write(*,63) 'Vtve', llr(7), lli(7)
      write(*,63) 'Vtvm', llr(8), lli(8)
      write(*,63) 'Vtvt', llr(9), lli(9)
      print *, '--------------------------------------------------'
      endif

      if(sets(3).eqv..true.) then
      write(*,64) 'W'' mass ', WPMASS
      write(*,64) 'W'' width', WPWIDTH
      print *, '--------------------------------------------------'
      endif

      if(sets(4).eqv..true.) then
      print *, 'Branching ratios:'
      write(*,67) 'BR( W'' ->    u+d    )', qbr(1)
      write(*,67) 'BR( W'' ->    u+s    )', qbr(2)
      write(*,67) 'BR( W'' ->    u+b    )', qbr(3)
      write(*,67) 'BR( W'' ->    c+d    )', qbr(4)
      write(*,67) 'BR( W'' ->    c+s    )', qbr(5)
      write(*,67) 'BR( W'' ->    c+b    )', qbr(6)
      write(*,67) 'BR( W'' ->    t+d    )', qbr(7)
      write(*,67) 'BR( W'' ->    t+s    )', qbr(8)
      write(*,67) 'BR( W'' ->    t+b    )', qbr(9)
      write(*,67) 'BR( W'' ->   e+v_e   )', llbr(1)
      write(*,67) 'BR( W'' ->  mu+v_mu  )', llbr(5)
      write(*,67) 'BR( W'' -> tau+v_tau )', llbr(9)
      print *, '--------------------------------------------------'
      endif

      if(sets(5).eqv..true.) then
      print *, 'R_{u}_{i,f} for qq'' -> W'' -> qq'':'
      write(*,68) 'ud','us','ub','cd','cs','cb','td','ts','tb'
      write(*,69) 'ud',qqru(1,1),qqru(1,2),qqru(1,3),
     &                 qqru(1,4),qqru(1,5),qqru(1,6),
     &                 qqru(1,7),qqru(1,8),qqru(1,9)
      write(*,69) 'us',qqru(2,1),qqru(2,2),qqru(2,3),
     &                 qqru(2,4),qqru(2,5),qqru(2,6),
     &                 qqru(2,7),qqru(2,8),qqru(2,9)
      write(*,69) 'ub',qqru(3,1),qqru(3,2),qqru(3,3),
     &                 qqru(3,4),qqru(3,5),qqru(3,6),
     &                 qqru(3,7),qqru(3,8),qqru(3,9)
      write(*,69) 'cd',qqru(4,1),qqru(4,2),qqru(4,3),
     &                 qqru(4,4),qqru(4,5),qqru(4,6),
     &                 qqru(4,7),qqru(4,8),qqru(4,9)
      write(*,69) 'cs',qqru(5,1),qqru(5,2),qqru(5,3),
     &                 qqru(5,4),qqru(5,5),qqru(5,6),
     &                 qqru(5,7),qqru(5,8),qqru(5,9)
      write(*,69) 'cb',qqru(6,1),qqru(6,2),qqru(6,3),
     &                 qqru(6,4),qqru(6,5),qqru(6,6),
     &                 qqru(6,7),qqru(6,8),qqru(6,9)
      write(*,69) 'td',qqru(7,1),qqru(7,2),qqru(7,3),
     &                 qqru(7,4),qqru(7,5),qqru(7,6),
     &                 qqru(7,7),qqru(7,8),qqru(7,9)
      write(*,69) 'ts',qqru(8,1),qqru(8,2),qqru(8,3),
     &                 qqru(8,4),qqru(8,5),qqru(8,6),
     &                 qqru(8,7),qqru(8,8),qqru(8,9)
      write(*,69) 'tb',qqru(9,1),qqru(9,2),qqru(9,3),
     &                 qqru(9,4),qqru(9,5),qqru(9,6),
     &                 qqru(9,7),qqru(9,8),qqru(9,9)
      print *, '--------------------------------------------------'
      print *, 'R_{t}_{i,f} for qq'' -> W'' -> qq'':'
      write(*,68) 'ud','us','ub','cd','cs','cb','td','ts','tb'
      write(*,69) 'ud',qqrt(1,1),qqrt(1,2),qqrt(1,3),
     &                 qqrt(1,4),qqrt(1,5),qqrt(1,6),
     &                 qqrt(1,7),qqrt(1,8),qqrt(1,9)
      write(*,69) 'us',qqrt(2,1),qqrt(2,2),qqrt(2,3),
     &                 qqrt(2,4),qqrt(2,5),qqrt(2,6),
     &                 qqrt(2,7),qqrt(2,8),qqrt(2,9)
      write(*,69) 'ub',qqrt(3,1),qqrt(3,2),qqrt(3,3),
     &                 qqrt(3,4),qqrt(3,5),qqrt(3,6),
     &                 qqrt(3,7),qqrt(3,8),qqrt(3,9)
      write(*,69) 'cd',qqrt(4,1),qqrt(4,2),qqrt(4,3),
     &                 qqrt(4,4),qqrt(4,5),qqrt(4,6),
     &                 qqrt(4,7),qqrt(4,8),qqrt(4,9)
      write(*,69) 'cs',qqrt(5,1),qqrt(5,2),qqrt(5,3),
     &                 qqrt(5,4),qqrt(5,5),qqrt(5,6),
     &                 qqrt(5,7),qqrt(5,8),qqrt(5,9)
      write(*,69) 'cb',qqrt(6,1),qqrt(6,2),qqrt(6,3),
     &                 qqrt(6,4),qqrt(6,5),qqrt(6,6),
     &                 qqrt(6,7),qqrt(6,8),qqrt(6,9)
      write(*,69) 'td',qqrt(7,1),qqrt(7,2),qqrt(7,3),
     &                 qqrt(7,4),qqrt(7,5),qqrt(7,6),
     &                 qqrt(7,7),qqrt(7,8),qqrt(7,9)
      write(*,69) 'ts',qqrt(8,1),qqrt(8,2),qqrt(8,3),
     &                 qqrt(8,4),qqrt(8,5),qqrt(8,6),
     &                 qqrt(8,7),qqrt(8,8),qqrt(8,9)
      write(*,69) 'tb',qqrt(9,1),qqrt(9,2),qqrt(9,3),
     &                 qqrt(9,4),qqrt(9,5),qqrt(9,6),
     &                 qqrt(9,7),qqrt(9,8),qqrt(9,9)
      print *, '--------------------------------------------------'
      print *, 'R_{u}_{i,f} for qq'' -> W'' -> ll'':'
      write(*,70) 'ev_e','ev_m','ev_t','mv_e','mv_m','mv_t',
     &            'tv_e','tv_m','tv_t'
      write(*,69) 'ud',qlru(1,1),qlru(1,2),qlru(1,3),
     &                 qlru(1,4),qlru(1,5),qlru(1,6),
     &                 qlru(1,7),qlru(1,8),qlru(1,9)
      write(*,69) 'us',qlru(2,1),qlru(2,2),qlru(2,3),
     &                 qlru(2,4),qlru(2,5),qlru(2,6),
     &                 qlru(2,7),qlru(2,8),qlru(2,9)
      write(*,69) 'ub',qlru(3,1),qlru(3,2),qlru(3,3),
     &                 qlru(3,4),qlru(3,5),qlru(3,6),
     &                 qlru(3,7),qlru(3,8),qlru(3,9)
      write(*,69) 'cd',qlru(4,1),qlru(4,2),qlru(4,3),
     &                 qlru(4,4),qlru(4,5),qlru(4,6),
     &                 qlru(4,7),qlru(4,8),qlru(4,9)
      write(*,69) 'cs',qlru(5,1),qlru(5,2),qlru(5,3),
     &                 qlru(5,4),qlru(5,5),qlru(5,6),
     &                 qlru(5,7),qlru(5,8),qlru(5,9)
      write(*,69) 'cb',qlru(6,1),qlru(6,2),qlru(6,3),
     &                 qlru(6,4),qlru(6,5),qlru(6,6),
     &                 qlru(6,7),qlru(6,8),qlru(6,9)
      write(*,69) 'td',qlru(7,1),qlru(7,2),qlru(7,3),
     &                 qlru(7,4),qlru(7,5),qlru(7,6),
     &                 qlru(7,7),qlru(7,8),qlru(7,9)
      write(*,69) 'ts',qlru(8,1),qlru(8,2),qlru(8,3),
     &                 qlru(8,4),qlru(8,5),qlru(8,6),
     &                 qlru(8,7),qlru(8,8),qlru(8,9)
      write(*,69) 'tb',qlru(9,1),qlru(9,2),qlru(9,3),
     &                 qlru(9,4),qlru(9,5),qlru(9,6),
     &                 qlru(9,7),qlru(9,8),qlru(9,9)
      print *, '--------------------------------------------------'
      print *, 'R_{t}_{i,f} for qq'' -> W'' -> ll'':'
      write(*,70) 'ev_e','ev_m','ev_t','mv_e','mv_m','mv_t',
     &            'tv_e','tv_m','tv_t'
      write(*,69) 'ud',qlrt(1,1),qlrt(1,2),qlrt(1,3),
     &                 qlrt(1,4),qlrt(1,5),qlrt(1,6),
     &                 qlrt(1,7),qlrt(1,8),qlrt(1,9)
      write(*,69) 'us',qlrt(2,1),qlrt(2,2),qlrt(2,3),
     &                 qlrt(2,4),qlrt(2,5),qlrt(2,6),
     &                 qlrt(2,7),qlrt(2,8),qlrt(2,9)
      write(*,69) 'ub',qlrt(3,1),qlrt(3,2),qlrt(3,3),
     &                 qlrt(3,4),qlrt(3,5),qlrt(3,6),
     &                 qlrt(3,7),qlrt(3,8),qlrt(3,9)
      write(*,69) 'cd',qlrt(4,1),qlrt(4,2),qlrt(4,3),
     &                 qlrt(4,4),qlrt(4,5),qlrt(4,6),
     &                 qlrt(4,7),qlrt(4,8),qlrt(4,9)
      write(*,69) 'cs',qlrt(5,1),qlrt(5,2),qlrt(5,3),
     &                 qlrt(5,4),qlrt(5,5),qlrt(5,6),
     &                 qlrt(5,7),qlrt(5,8),qlrt(5,9)
      write(*,69) 'cb',qlrt(6,1),qlrt(6,2),qlrt(6,3),
     &                 qlrt(6,4),qlrt(6,5),qlrt(6,6),
     &                 qlrt(6,7),qlrt(6,8),qlrt(6,9)
      write(*,69) 'td',qlrt(7,1),qlrt(7,2),qlrt(7,3),
     &                 qlrt(7,4),qlrt(7,5),qlrt(7,6),
     &                 qlrt(7,7),qlrt(7,8),qlrt(7,9)
      write(*,69) 'ts',qlrt(8,1),qlrt(8,2),qlrt(8,3),
     &                 qlrt(8,4),qlrt(8,5),qlrt(8,6),
     &                 qlrt(8,7),qlrt(8,8),qlrt(8,9)
      write(*,69) 'tb',qlrt(9,1),qlrt(9,2),qlrt(9,3),
     &                 qlrt(9,4),qlrt(9,5),qlrt(9,6),
     &                 qlrt(9,7),qlrt(9,8),qlrt(9,9)
      print *, '--------------------------------------------------'
      endif

      if(sets(6).eqv..true.) then
      write(*,71) 'q+q'' -> W'' -> q+q''','R_{u}','R_{t}',
     &            'g_{eff}/g_{SM}'
      write(*,72) 'u+d','   u+d   ',qqru(1,1),qqrt(1,1),gqqscale(1,1)
      write(*,72) 'u+d','   u+s   ',qqru(1,2),qqrt(1,2),gqqscale(1,2)
      write(*,72) 'u+d','   u+b   ',qqru(1,3),qqrt(1,3),gqqscale(1,3)
      write(*,72) 'u+d','   c+d   ',qqru(1,4),qqrt(1,4),gqqscale(1,4)
      write(*,72) 'u+d','   c+s   ',qqru(1,5),qqrt(1,5),gqqscale(1,5)
      write(*,72) 'u+d','   c+b   ',qqru(1,6),qqrt(1,6),gqqscale(1,6)
      write(*,72) 'u+d','   t+d   ',qqru(1,7),qqrt(1,7),gqqscale(1,7)
      write(*,72) 'u+d','   t+s   ',qqru(1,8),qqrt(1,8),gqqscale(1,8)
      write(*,72) 'u+d','   t+b   ',qqru(1,9),qqrt(1,9),gqqscale(1,9)
      write(*,72) 'u+s','   u+d   ',qqru(2,1),qqrt(2,1),gqqscale(2,1)
      write(*,72) 'u+s','   u+s   ',qqru(2,2),qqrt(2,2),gqqscale(2,2)
      write(*,72) 'u+s','   u+b   ',qqru(2,3),qqrt(2,3),gqqscale(2,3)
      write(*,72) 'u+s','   c+d   ',qqru(2,4),qqrt(2,4),gqqscale(2,4)
      write(*,72) 'u+s','   c+s   ',qqru(2,5),qqrt(2,5),gqqscale(2,5)
      write(*,72) 'u+s','   c+b   ',qqru(2,6),qqrt(2,6),gqqscale(2,6)
      write(*,72) 'u+s','   t+d   ',qqru(2,7),qqrt(2,7),gqqscale(2,7)
      write(*,72) 'u+s','   t+s   ',qqru(2,8),qqrt(2,8),gqqscale(2,8)
      write(*,72) 'u+s','   t+b   ',qqru(2,9),qqrt(2,9),gqqscale(2,9)
      write(*,72) 'u+b','   u+d   ',qqru(3,1),qqrt(3,1),gqqscale(3,1)
      write(*,72) 'u+b','   u+s   ',qqru(3,2),qqrt(3,2),gqqscale(3,2)
      write(*,72) 'u+b','   u+b   ',qqru(3,3),qqrt(3,3),gqqscale(3,3)
      write(*,72) 'u+b','   c+d   ',qqru(3,4),qqrt(3,4),gqqscale(3,4)
      write(*,72) 'u+b','   c+s   ',qqru(3,5),qqrt(3,5),gqqscale(3,5)
      write(*,72) 'u+b','   c+b   ',qqru(3,6),qqrt(3,6),gqqscale(3,6)
      write(*,72) 'u+b','   t+d   ',qqru(3,7),qqrt(3,7),gqqscale(3,7)
      write(*,72) 'u+b','   t+s   ',qqru(3,8),qqrt(3,8),gqqscale(3,8)
      write(*,72) 'u+b','   t+b   ',qqru(3,9),qqrt(3,9),gqqscale(3,9)
      write(*,72) 'c+d','   u+d   ',qqru(4,1),qqrt(4,1),gqqscale(4,1)
      write(*,72) 'c+d','   u+s   ',qqru(4,2),qqrt(4,2),gqqscale(4,2)
      write(*,72) 'c+d','   u+b   ',qqru(4,3),qqrt(4,3),gqqscale(4,3)
      write(*,72) 'c+d','   c+d   ',qqru(4,4),qqrt(4,4),gqqscale(4,4)
      write(*,72) 'c+d','   c+s   ',qqru(4,5),qqrt(4,5),gqqscale(4,5)
      write(*,72) 'c+d','   c+b   ',qqru(4,6),qqrt(4,6),gqqscale(4,6)
      write(*,72) 'c+d','   t+d   ',qqru(4,7),qqrt(4,7),gqqscale(4,7)
      write(*,72) 'c+d','   t+s   ',qqru(4,8),qqrt(4,8),gqqscale(4,8)
      write(*,72) 'c+d','   t+b   ',qqru(4,9),qqrt(4,9),gqqscale(4,9)
      write(*,72) 'c+s','   u+d   ',qqru(5,1),qqrt(5,1),gqqscale(5,1)
      write(*,72) 'c+s','   u+s   ',qqru(5,2),qqrt(5,2),gqqscale(5,2)
      write(*,72) 'c+s','   u+b   ',qqru(5,3),qqrt(5,3),gqqscale(5,3)
      write(*,72) 'c+s','   c+d   ',qqru(5,4),qqrt(5,4),gqqscale(5,4)
      write(*,72) 'c+s','   c+s   ',qqru(5,5),qqrt(5,5),gqqscale(5,5)
      write(*,72) 'c+s','   c+b   ',qqru(5,6),qqrt(5,6),gqqscale(5,6)
      write(*,72) 'c+s','   t+d   ',qqru(5,7),qqrt(5,7),gqqscale(5,7)
      write(*,72) 'c+s','   t+s   ',qqru(5,8),qqrt(5,8),gqqscale(5,8)
      write(*,72) 'c+s','   t+b   ',qqru(5,9),qqrt(5,9),gqqscale(5,9)
      write(*,72) 'c+b','   u+d   ',qqru(6,1),qqrt(6,1),gqqscale(6,1)
      write(*,72) 'c+b','   u+s   ',qqru(6,2),qqrt(6,2),gqqscale(6,2)
      write(*,72) 'c+b','   u+b   ',qqru(6,3),qqrt(6,3),gqqscale(6,3)
      write(*,72) 'c+b','   c+d   ',qqru(6,4),qqrt(6,4),gqqscale(6,4)
      write(*,72) 'c+b','   c+s   ',qqru(6,5),qqrt(6,5),gqqscale(6,5)
      write(*,72) 'c+b','   c+b   ',qqru(6,6),qqrt(6,6),gqqscale(6,6)
      write(*,72) 'c+b','   t+d   ',qqru(6,7),qqrt(6,7),gqqscale(6,7)
      write(*,72) 'c+b','   t+s   ',qqru(6,8),qqrt(6,8),gqqscale(6,8)
      write(*,72) 'c+b','   t+b   ',qqru(6,9),qqrt(6,9),gqqscale(6,9)
      write(*,72) 't+d','   u+d   ',qqru(7,1),qqrt(7,1),gqqscale(7,1)
      write(*,72) 't+d','   u+s   ',qqru(7,2),qqrt(7,2),gqqscale(7,2)
      write(*,72) 't+d','   u+b   ',qqru(7,3),qqrt(7,3),gqqscale(7,3)
      write(*,72) 't+d','   c+d   ',qqru(7,4),qqrt(7,4),gqqscale(7,4)
      write(*,72) 't+d','   c+s   ',qqru(7,5),qqrt(7,5),gqqscale(7,5)
      write(*,72) 't+d','   c+b   ',qqru(7,6),qqrt(7,6),gqqscale(7,6)
      write(*,72) 't+d','   t+d   ',qqru(7,7),qqrt(7,7),gqqscale(7,7)
      write(*,72) 't+d','   t+s   ',qqru(7,8),qqrt(7,8),gqqscale(7,8)
      write(*,72) 't+d','   t+b   ',qqru(7,9),qqrt(7,9),gqqscale(7,9)
      write(*,72) 't+s','   u+d   ',qqru(8,1),qqrt(8,1),gqqscale(8,1)
      write(*,72) 't+s','   u+s   ',qqru(8,2),qqrt(8,2),gqqscale(8,2)
      write(*,72) 't+s','   u+b   ',qqru(8,3),qqrt(8,3),gqqscale(8,3)
      write(*,72) 't+s','   c+d   ',qqru(8,4),qqrt(8,4),gqqscale(8,4)
      write(*,72) 't+s','   c+s   ',qqru(8,5),qqrt(8,5),gqqscale(8,5)
      write(*,72) 't+s','   c+b   ',qqru(8,6),qqrt(8,6),gqqscale(8,6)
      write(*,72) 't+s','   t+d   ',qqru(8,7),qqrt(8,7),gqqscale(8,7)
      write(*,72) 't+s','   t+s   ',qqru(8,8),qqrt(8,8),gqqscale(8,8)
      write(*,72) 't+s','   t+b   ',qqru(8,9),qqrt(8,9),gqqscale(8,9)
      write(*,72) 't+b','   u+d   ',qqru(9,1),qqrt(9,1),gqqscale(9,1)
      write(*,72) 't+b','   u+s   ',qqru(9,2),qqrt(9,2),gqqscale(9,2)
      write(*,72) 't+b','   u+b   ',qqru(9,3),qqrt(9,3),gqqscale(9,3)
      write(*,72) 't+b','   c+d   ',qqru(9,4),qqrt(9,4),gqqscale(9,4)
      write(*,72) 't+b','   c+s   ',qqru(9,5),qqrt(9,5),gqqscale(9,5)
      write(*,72) 't+b','   c+b   ',qqru(9,6),qqrt(9,6),gqqscale(9,6)
      write(*,72) 't+b','   t+d   ',qqru(9,7),qqrt(9,7),gqqscale(9,7)
      write(*,72) 't+b','   t+s   ',qqru(9,8),qqrt(9,8),gqqscale(9,8)
      write(*,72) 't+b','   t+b   ',qqru(9,9),qqrt(9,9),gqqscale(9,9)
      print *, '--------------------------------------------------'
      write(*,71) 'q+q'' -> W'' -> l+l''','R_{u}','R_{t}',
     &            'g_{eff}/g_{SM}'
      write(*,72) 'u+d','  e+v_e  ',qlru(1,1),qlrt(1,1),gqlscale(1,1)
      write(*,72) 'u+d','  e+v_mu ',qlru(1,2),qlrt(1,2)
      write(*,72) 'u+d','  e+v_tau',qlru(1,3),qlrt(1,3)
      write(*,72) 'u+d',' mu+v_e  ',qlru(1,4),qlrt(1,4)
      write(*,72) 'u+d',' mu+v_mu ',qlru(1,5),qlrt(1,5),gqlscale(1,5)
      write(*,72) 'u+d',' mu+v_tau',qlru(1,6),qlrt(1,6)
      write(*,72) 'u+d','tau+v_e  ',qlru(1,7),qlrt(1,7)
      write(*,72) 'u+d','tau+v_mu ',qlru(1,8),qlrt(1,8)
      write(*,72) 'u+d','tau+v_tau',qlru(1,9),qlrt(1,9),gqlscale(1,9)
      write(*,72) 'u+s','  e+v_e  ',qlru(2,1),qlrt(2,1),gqlscale(2,1)
      write(*,72) 'u+s','  e+v_mu ',qlru(2,2),qlrt(2,2)
      write(*,72) 'u+s','  e+v_tau',qlru(2,3),qlrt(2,3)
      write(*,72) 'u+s',' mu+v_e  ',qlru(2,4),qlrt(2,4)
      write(*,72) 'u+s',' mu+v_mu ',qlru(2,5),qlrt(2,5),gqlscale(2,5)
      write(*,72) 'u+s',' mu+v_tau',qlru(2,6),qlrt(2,6)
      write(*,72) 'u+s','tau+v_e  ',qlru(2,7),qlrt(2,7)
      write(*,72) 'u+s','tau+v_mu ',qlru(2,8),qlrt(2,8)
      write(*,72) 'u+s','tau+v_tau',qlru(2,9),qlrt(2,9),gqlscale(2,9)
      write(*,72) 'u+b','  e+v_e  ',qlru(3,1),qlrt(3,1),gqlscale(3,1)
      write(*,72) 'u+b','  e+v_mu ',qlru(3,2),qlrt(3,2)
      write(*,72) 'u+b','  e+v_tau',qlru(3,3),qlrt(3,3)
      write(*,72) 'u+b',' mu+v_e  ',qlru(3,4),qlrt(3,4)
      write(*,72) 'u+b',' mu+v_mu ',qlru(3,5),qlrt(3,5),gqlscale(3,5)
      write(*,72) 'u+b',' mu+v_tau',qlru(3,6),qlrt(3,6)
      write(*,72) 'u+b','tau+v_e  ',qlru(3,7),qlrt(3,7)
      write(*,72) 'u+b','tau+v_mu ',qlru(3,8),qlrt(3,8)
      write(*,72) 'u+b','tau+v_tau',qlru(3,9),qlrt(3,9),gqlscale(3,9)
      write(*,72) 'c+d','  e+v_e  ',qlru(4,1),qlrt(4,1),gqlscale(4,1)
      write(*,72) 'c+d','  e+v_mu ',qlru(4,2),qlrt(4,2)
      write(*,72) 'c+d','  e+v_tau',qlru(4,3),qlrt(4,3)
      write(*,72) 'c+d',' mu+v_e  ',qlru(4,4),qlrt(4,4)
      write(*,72) 'c+d',' mu+v_mu ',qlru(4,5),qlrt(4,5),gqlscale(4,5)
      write(*,72) 'c+d',' mu+v_tau',qlru(4,6),qlrt(4,6)
      write(*,72) 'c+d','tau+v_e  ',qlru(4,7),qlrt(4,7)
      write(*,72) 'c+d','tau+v_mu ',qlru(4,8),qlrt(4,8)
      write(*,72) 'c+d','tau+v_tau',qlru(4,9),qlrt(4,9),gqlscale(4,9)
      write(*,72) 'c+s','  e+v_e  ',qlru(5,1),qlrt(5,1),gqlscale(5,1)
      write(*,72) 'c+s','  e+v_mu ',qlru(5,2),qlrt(5,2)
      write(*,72) 'c+s','  e+v_tau',qlru(5,3),qlrt(5,3)
      write(*,72) 'c+s',' mu+v_e  ',qlru(5,4),qlrt(5,4)
      write(*,72) 'c+s',' mu+v_mu ',qlru(5,5),qlrt(5,5),gqlscale(5,5)
      write(*,72) 'c+s',' mu+v_tau',qlru(5,6),qlrt(5,6)
      write(*,72) 'c+s','tau+v_e  ',qlru(5,7),qlrt(5,7)
      write(*,72) 'c+s','tau+v_mu ',qlru(5,8),qlrt(5,8)
      write(*,72) 'c+s','tau+v_tau',qlru(5,9),qlrt(5,9),gqlscale(5,9)
      write(*,72) 'c+b','  e+v_e  ',qlru(6,1),qlrt(6,1),gqlscale(6,1)
      write(*,72) 'c+b','  e+v_mu ',qlru(6,2),qlrt(6,2)
      write(*,72) 'c+b','  e+v_tau',qlru(6,3),qlrt(6,3)
      write(*,72) 'c+b',' mu+v_e  ',qlru(6,4),qlrt(6,4)
      write(*,72) 'c+b',' mu+v_mu ',qlru(6,5),qlrt(6,5),gqlscale(6,5)
      write(*,72) 'c+b',' mu+v_tau',qlru(6,6),qlrt(6,6)
      write(*,72) 'c+b','tau+v_e  ',qlru(6,7),qlrt(6,7)
      write(*,72) 'c+b','tau+v_mu ',qlru(6,8),qlrt(6,8)
      write(*,72) 'c+b','tau+v_tau',qlru(6,9),qlrt(6,9),gqlscale(6,9)
      write(*,72) 't+d','  e+v_e  ',qlru(7,1),qlrt(7,1),gqlscale(7,1)
      write(*,72) 't+d','  e+v_mu ',qlru(7,2),qlrt(7,2)
      write(*,72) 't+d','  e+v_tau',qlru(7,3),qlrt(7,3)
      write(*,72) 't+d',' mu+v_e  ',qlru(7,4),qlrt(7,4)
      write(*,72) 't+d',' mu+v_mu ',qlru(7,5),qlrt(7,5),gqlscale(7,5)
      write(*,72) 't+d',' mu+v_tau',qlru(7,6),qlrt(7,6)
      write(*,72) 't+d','tau+v_e  ',qlru(7,7),qlrt(7,7)
      write(*,72) 't+d','tau+v_mu ',qlru(7,8),qlrt(7,8)
      write(*,72) 't+d','tau+v_tau',qlru(7,9),qlrt(7,9),gqlscale(7,9)
      write(*,72) 't+s','  e+v_e  ',qlru(8,1),qlrt(8,1),gqlscale(8,1)
      write(*,72) 't+s','  e+v_mu ',qlru(8,2),qlrt(8,2)
      write(*,72) 't+s','  e+v_tau',qlru(8,3),qlrt(8,3)
      write(*,72) 't+s',' mu+v_e  ',qlru(8,4),qlrt(8,4)
      write(*,72) 't+s',' mu+v_mu ',qlru(8,5),qlrt(8,5),gqlscale(8,5)
      write(*,72) 't+s',' mu+v_tau',qlru(8,6),qlrt(8,6)
      write(*,72) 't+s','tau+v_e  ',qlru(8,7),qlrt(8,7)
      write(*,72) 't+s','tau+v_mu ',qlru(8,8),qlrt(8,8)
      write(*,72) 't+s','tau+v_tau',qlru(8,9),qlrt(8,9),gqlscale(8,9)
      write(*,72) 't+b','  e+v_e  ',qlru(9,1),qlrt(9,1),gqlscale(9,1)
      write(*,72) 't+b','  e+v_mu ',qlru(9,2),qlrt(9,2)
      write(*,72) 't+b','  e+v_tau',qlru(9,3),qlrt(9,3)
      write(*,72) 't+b',' mu+v_e  ',qlru(9,4),qlrt(9,4)
      write(*,72) 't+b',' mu+v_mu ',qlru(9,5),qlrt(9,5),gqlscale(9,5)
      write(*,72) 't+b',' mu+v_tau',qlru(9,6),qlrt(9,6)
      write(*,72) 't+b','tau+v_e  ',qlru(9,7),qlrt(9,7)
      write(*,72) 't+b','tau+v_mu ',qlru(9,8),qlrt(9,8)
      write(*,72) 't+b','tau+v_tau',qlru(9,9),qlrt(9,9),gqlscale(9,9)
      print *, '--------------------------------------------------'
      endif


      if((sets(5).eqv..true.).or.(sets(6).eqv..true.)) then

      if((qqsm.eqv..false.).or.(qlsm.eqv..false.)) then
      print *, 'There is non-zero R_{u}, hence there are'
      print *, 'non-Standard Model-like kinematics.'
      else
      print *, 'All R_{u}''s are 0, hence this model is purely'
      print *, 'right- or left-handed, and scales like the'
      print *, 'Standard Model.'
      print *, '--------------------------------------------------'
      endif

      endif

      print *, '=================== End of Log ==================='

      end

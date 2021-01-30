      SUBROUTINE GETBPO(N, IX, BPOS)
        INTEGER*4 N, IX, BPOS
        COMMON /INV/ P
        INTEGER*4 P(8)

        K = N / 30
        IX = K / 4 + 1

C       use computed goto here instead of a look-up array, faster
        GOTO (200,100,100,100,100,100,300,100,100,100,
     ;        400,100,500,100,100,100,600,100,700,100,
     ;        100,100,800,100,100,100,100,100,900) MOD(N,30)

  100   BPOS = -1
        RETURN
  200   BPOS = 1 + MOD(K,4)*8
        RETURN
  300   BPOS = 2 + MOD(K,4)*8
        RETURN
  400   BPOS = 3 + MOD(K,4)*8
        RETURN
  500   BPOS = 4 + MOD(K,4)*8
        RETURN
  600   BPOS = 5 + MOD(K,4)*8
        RETURN
  700   BPOS = 6 + MOD(K,4)*8
        RETURN
  800   BPOS = 7 + MOD(K,4)*8
        RETURN
  900   BPOS = 8 + MOD(K,4)*8
        RETURN

        RETURN
      END

      SUBROUTINE GETNUM(IX, BPOS, N)
        INTEGER*4 N, IX, BPOS
        COMMON /INV/ P
        INTEGER*4 P(8)

        IF (BPOS.EQ.-1) THEN
          N = -1
          RETURN
        ENDIF

        IF (MOD(BPOS,8).NE.0) THEN
          K = 4*(IX-1) + BPOS/8
          N = 30*K + P(MOD(BPOS,8))
        ELSE
          K = 4*(IX-1) + BPOS/8 - 1
          N = 30*K + P(8)
        ENDIF

        RETURN
      END

      SUBROUTINE GETKMX(N,KFLAG,KMAX,BLAST,LMT)
        INTEGER*4 N, KFLAG, KMAX, BLAST, LMT
        INTEGER*4 ROOT, IX, BPOS
        COMMON /INV/ P
        INTEGER*4 P(8)

        IF (KFLAG.EQ.1) THEN
          ROOT = IFIX(SQRT(N*1.)+0.5)
        ELSE
          ROOT = N
        ENDIF

        IF (MOD(ROOT,30).NE.0) THEN
          KMAX = ROOT / 30
        ELSE
          KMAX = ROOT / 30 - 1
        ENDIF

        LMT = ROOT
        BPOS = -1
        DO 10 I=1,7
          CALL GETBPO(LMT, IX, BPOS)
          IF (BPOS.NE.-1) GOTO 100
          IF (BPOS.EQ.-1) LMT = LMT - 1
   10   CONTINUE

  100   DO 20 I=1,8
          IF (P(I).EQ.MOD(LMT,30)) BLAST = I
   20   CONTINUE

        RETURN
      END

      SUBROUTINE SSIEVE(N, NSSVE)
        INTEGER*4 N, NSSVE

        NSSVE = (N/30)/4 + 1

        RETURN
      END

      BLOCK DATA
        COMMON /INV/ P
        INTEGER*4 P(8)/1,7,11,13,17,19,23,29/
      END

      SUBROUTINE ESIEVE(SIEVE, N, NPRIME)
        INTEGER*4 N,  NPRIME
        INTEGER*4 IX, BPOS, KMAX, BLAST, LMT, BMAX, B
        INTEGER*4 SIEVE(*)
        INTEGER*4 CYCL(8)/6,4,2,4,2,4,6,2/
        COMMON /INV/ P
        INTEGER*4 P(8)

        NPRIME = 3
        SIEVE(1) = IBSET(SIEVE(1), 31)
        CALL GETKMX(N,1,KMAX,BLAST,LMT)

        BMAX = 8
        DO 10 K=0,KMAX
          IF (K.EQ.KMAX) BMAX = BLAST
          DO 20 B=1,BMAX
            NUM = 30*K + P(B)
            CALL GETBPO(NUM, IX, BPOS)

            IF (.NOT.BTEST(SIEVE(IX), 32-BPOS)) THEN
              CALL GETBPO(NUM**2, IX, BPOS)
              SIEVE(IX) = IBSET(SIEVE(IX), 32-BPOS)

              M = 0
              IC = 0

  200         IC = IC + 1
              ICX = MOD(IC-1, 8) + 1
              M = M + CYCL(ICX)
              NNUM = NUM*(NUM + M)

              IF (NNUM.GT.N) GOTO 100
              CALL GETBPO(NNUM, IX, BPOS)
              SIEVE(IX) = IBSET(SIEVE(IX), 32-BPOS)
              GOTO 200

  100         CYCL = CSHIFT(CYCL, 1)
            ELSE
              CYCL = CSHIFT(CYCL, 1)
            ENDIF
   20     CONTINUE
   10   CONTINUE

        KMX = (N/30)/4
        NPRIME = NPRIME + 32*KMX - SUM(POPCNT(SIEVE(:KMX)))

        CALL GETKMX(N,0,KMAX,BLAST,LMT)

        BMAX = 8
        KS = KMX*4
        DO 30 K=KS,KMAX
          IF (K.EQ.KMAX) BMAX = BLAST
          DO 40 B=1,BMAX
            NUM = 30*K + P(B)
            CALL GETBPO(NUM, IX, BPOS)
            IF (.NOT.BTEST(SIEVE(IX), 32-BPOS)) NPRIME = NPRIME + 1
   40     CONTINUE
   30   CONTINUE


        RETURN
      END

      SUBROUTINE ISPRIM(SIEVE, NUM, FLAG)
        INTEGER*4 SIEVE(*), NUM, FLAG
        INTEGER*4 IX, BPOS, PFLAG

        CALL GETBPO(NUM, IX, BPOS)

        IF (BPOS.EQ.-1) THEN
          FLAG = 0
          RETURN
        ENDIF

        PFLAG = IBITS(SIEVE(IX), 32-BPOS, 1)
        IF (PFLAG.EQ.0) THEN
          FLAG = 1
          RETURN
        ELSE
          FLAG = 0
          RETURN
        ENDIF
      END

      SUBROUTINE PNUMS(SIEVE,N,NPRIME,PARR)
        INTEGER*4 SIEVE(*), N, NPRIME
        INTEGER*4 PARR(NPRIME)
        INTEGER*4 K, B, KMAX, BMAX, BLAST, LMT, NUM, FLAG, I/4/
        COMMON /INV/ P
        INTEGER*4 P(8)

        PARR(1) = 2
        PARR(2) = 3
        PARR(3) = 5

        CALL GETKMX(N,0,KMAX,BLAST,LMT)

        BMAX = 8
        DO 10 K=0,KMAX
          IF (K.EQ.KMAX) BMAX = BLAST
          DO 20 B=1,BMAX
            NUM = 30*K + P(B)
            CALL ISPRIM(SIEVE, NUM, FLAG)
            IF (FLAG.EQ.1) THEN
              PARR(I) = NUM
              I = I + 1
            ENDIF
   20     CONTINUE
   10   CONTINUE
        RETURN
      END

C     INTEGER*4 SIEVE(16983957)/16983957*0/, N, SSVE, NPRIME
C     INTEGER*4 PARR(100000000)
      INTEGER*4 SIEVE(17895698)/17895698*0/, N, SSVE, NPRIME
      INTEGER*4 PARR(105097565)

      N = 2038074744
C     N = 1000
C     N = 2147483647

C     CALL SSIEVE(N, SSVE)
C     PRINT *, SSVE
C     STOP

      CALL ESIEVE(SIEVE, N, NPRIME)

      PRINT *, NPRIME
C     STOP

      CALL PNUMS(SIEVE, N, NPRIME, PARR)
      PRINT *, PARR(100000000)



      STOP
      END

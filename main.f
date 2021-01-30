      USE eratos

C     INTEGER*4 SIEVE(16983957)/16983957*0/, N, SSVE, NPRIME
      INTEGER*4 SIEVE(17895698)/17895698*0/, N, SSVE, NPRIME, FLAG
      INTEGER*4 PARR(105097565)
      N = 2038074744
C     N = 2147483647

C     use this subroutine to set the size of SIEVE
      CALL SSIEVE(N, SSVE)
      PRINT *, "The size of sieve for number under", N, "is", SSVE

C     start the sieving
      CALL ESIEVE(SIEVE, N, NPRIME)
      PRINT *, "Sieved numbers of prime number:", NPRIME

C     generate the actual prime numbers array into PARR
      CALL PNUMS(SIEVE, N, NPRIME, PARR)

      PRINT *, "The 1 000th prime number is", PARR(1000)
      PRINT *, "The 1 000 000th prime number is", PARR(1000000)
      PRINT *, "The 100 000 000th prime number is", PARR(100000000)

C     check a number if it is prime, flag=1 indicates prime number
      CALL ISPRIM(SIEVE, 15485863, FLAG)
      IF (FLAG.EQ.1) THEN
        PRINT *, "15485863 is a prime number."
      ENDIF

      CALL ISPRIM(SIEVE, 1548586, FLAG)
      IF (FLAG.EQ.0) THEN
        PRINT *, "1548586 is NOT a prime number."
      ENDIF


      STOP
      END

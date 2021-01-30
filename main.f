      USE eratos

      INTEGER*4 SIEVE(16983957)/16983957*0/, N, SSVE, NPRIME
C     INTEGER*4 SIEVE(17895698)/17895698*0/, N, SSVE, NPRIME
      INTEGER*4 PARR(100000000)
      N = 2038074744
C     N = 2147483600

C     use this subroutine to set the size of SIEVE
      CALL SSIEVE(N, SSVE)
      PRINT *, "The size of sieve for number under", N, "is", SSVE

C     start the sieving
      CALL ESIEVE(SIEVE, N, NPRIME)
      PRINT *, "Sieved numbers of prime number:", NPRIME

C     generate the actual prime numbers array into PARR
      CALL PNUMS(SIEVE, N, NPRIME, PARR)

      PRINT *, "The 1000th prime number is", PARR(1000)
      PRINT *, "The 1000000th prime number is", PARR(1000000)
      PRINT *, "The 100000000th prime number is", PARR(100000000)

      STOP
      END

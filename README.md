# Sieve of Eratosthenes
When I was tidying up my computer I found an implementation of the sieve of
Eratosthenes for finding prime number, when I was learning Fortran 77 through
Project Euler. I completely forget the detail of implementation (lack of
comment!!!) but I hope at some day I can tidy the code up. I think it is using
bit array to store the sieve.

## Usage
Compile the module:
```
f77 -c eratosthenes.f
```
Optionally you can supply the `-O5` for 'ultimate' optimisation.

Link with the main program:
```
f77 -o main main.f eratosthenes.o
```

Execute:
```
./main
```

The output:
```
 The size of sieve for number under  2038074744 is    16983957
 Sieved numbers of prime number:   100000000
 The 1000th prime number is        7919
 The 1000000th prime number is    15485863
 The 100000000th prime number is  2038074743
```

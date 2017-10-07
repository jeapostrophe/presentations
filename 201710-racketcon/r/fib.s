      STOL 0     ; AC = 0 => M[SP] = Fib(0)
      LOCO 1     ; True => AC = 1
      STOL 1     ; AC = 1 => M[SP+1] = Fib(1)
loop: ADDL 0     ; AC = Fib(n+1), M[SP] = Fib(n) => AC = Fib(n+2)
      STOL 2     ; AC = Fib(n+2) => M[SP+2] = Fib(n+2)
      INSP 1     ; increment n & SP
      JPOS loop: ; If Fib(n+2) was negative, then n = 24

PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: P = 1
    PRINT *, "Input required quantity of random numbers(10^N):"
    READ (*, *) P
    CALL Schrage(P) !使用以前编写的16807生成器子程序生成97个随机数作为减法生成器的种子
END PROGRAM MAIN

SUBROUTINE Minus(N) !用Marsaglia生成器中的“减法生成器”生成随机数序列
    INTEGER(KIND=4) :: p = 97, q = 33, N, i
    REAL(KIND=8) :: x(10**N + 97) 
    OPEN (2, file='lcg.out')
    READ (2, *) x
    CLOSE (2)
    DO i = 98, 10**N + 97 !从第98个数组元素开始生成总数目为10^N的随机数序列
        x(i) = x(i - p) - x(i - q)
        IF(x(i) < 0) THEN
            x(i) = x(i) + 1
        END IF
    END DO
    OPEN (99, file='minusrand.out')
    DO i = 98, 10**N + 97
        WRITE (1, *) x(i)
    END DO
    CLOSE (99)

END SUBROUTINE Minus

SUBROUTINE Test()
    
END SUBROUTINE Test

SUBROUTINE Schrage(P) !Schrage随机数生成器子程序
   IMPLICIT NONE
   INTEGER :: N = 1, P
   INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(10**P)
   REAL(KIND=8) z(10**P)
   In(1) = m - 1
   z(1) = REAL(In(1))/m
   DO N = 1, 10**P - 1
      In(N + 1) = a*MOD(In(N), q) - r*INT(In(N)/q)
      IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
         In(N + 1) = In(N + 1) + m
      END IF
      z(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
   END DO
   OPEN (1, file='lcgrand.out') !每次运行子程序将覆盖随机数
   DO N = 1, 10**P !将随机数按行存入文件
      WRITE (1, *) z(N)
   END DO
   CLOSE (1)
END SUBROUTINE Schrage


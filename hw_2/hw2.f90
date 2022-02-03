PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: N = 1 !用16807生成器生成100个随机数作为“减法生成器”的种子
    !PRINT *, "Input required quantity of random numbers(10^N)"
    !READ (*,*) N
    DO N = 2 , 7
        CALL Minus(N)
        CALL Schrage(N)
        CALL Test(N, 'minrand.out') !分别传入两个文件名作为实参来对其进行测试
        CALL Test(N, 'lcgrand.out')
    END DO
END PROGRAM MAIN

SUBROUTINE Minus(N) !用Marsaglia生成器中的“减法生成器”生成随机数序列
    INTEGER(KIND=4) :: p = 97, q = 33, N, i
    REAL(KIND=8) :: x(10**N), z(100) 
    CALL Schrage(2) !用16807生成器生成100个随机数作为“减法生成器”的种子
    OPEN (99, file='lcgrand.out')
    READ (99, *) z !首先将种子读取到数组z中
    CLOSE (99)
    DO i = 1, 97 !由种子生成前97个随机数
        x(i) = z(97 + i - p) - z(97 + i - q) !x的第一个数据由z(1)和z(65)生成
        IF(x(i) < 0) THEN
            x(i) = x(i) + real(1)
        END IF
    END DO
    DO i = 98, 10**N  !由生成的前97个随机数生成总数目为10^N的随机数序列
        x(i) = x(i - p) - x(i - q)
        IF(x(i) < 0) THEN
            x(i) = x(i) + real(1)
        END IF
    END DO
    OPEN (99, file='minrand.out') !将随机数按行存入文件
    DO i = 1, 10**N
        WRITE (99, *) x(i)
    END DO
    CLOSE (99)

END SUBROUTINE Minus 

SUBROUTINE Test(N, Filename) !检验随机数序列中出现关联比重的子程序
    INTEGER(KIND=4) :: Number = 0, i = 1
    CHARACTER(LEN=11) :: Filename
    REAL(KIND=8) :: x(10**N), r !定义要检验的随机数序列
    OPEN (11, file=Filename ) !打开相应文件读取随机数
    READ (11,*) x 
    CLOSE (11)
    IF (x(10**N) > x(2) .AND. x(2) > x(1)) THEN !用x(10**N)代替x(0)
        Number = Number + 1 !第1个随机数和最后一个随机数中最多只能有一个满足我们讨论的关系
    ELSE IF (x(10**N - 1) > x(1) .AND. x(1) > x(10**N)) THEN !用x(1)代替x(10**N + 1)
        Number = Number + 1
    END IF
    DO i = 2, 10**N - 1 
        IF(x(i - 1) > x(i + 1) .AND. x(i + 1) > x(i)) THEN
            Number = Number + 1 !每当检验满足关系，累加Number
        END IF
    END DO
    r = real(Number) / 10**N !计算出现关联性的比重
    OPEN (1, ACCESS='append', file='ratio.out')
    WRITE (1, *) r 
    CLOSE(1)
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


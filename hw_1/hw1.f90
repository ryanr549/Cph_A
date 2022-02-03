PROGRAM MAIN
   IMPLICIT NONE
   INTEGER :: P=1 
   !PRINT *, 'How many random numbers are required?(10^P)'
   !READ *, P !从键盘读取随机数数量大小
   DO P = 2, 7
    CALL Schrage(P)
    CALL Moment(P)
    CALL Independence(P)
   END DO
END PROGRAM MAIN

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
   OPEN (1, file='rand.out') !每次运行子程序将覆盖随机数
   DO N = 1, 10**P !将随机数按行存入文件
      WRITE (1, *) z(N)
   END DO
   CLOSE (1)
END SUBROUTINE Schrage

SUBROUTINE Moment(P) !k阶矩均匀性检验子程序(N一定)
    INTEGER(KIND=4) :: i = 1, k = 1, P
    REAL(KIND=8), DIMENSION(10) :: S = 0, D = 0
    REAL(KIND=8) :: z(10**P)
    OPEN (1,file='rand.out') !从前面产生的'rand.out'文件中读取随机数表
    READ (1, *) z
    CLOSE (1)
    DO k = 1, 10 !求随机数的1阶矩至10阶矩
        S(k) = SUM(z**k) / SIZE(z)
        D(k) = ABS(S(k) - real(1) / (1 + k)) !求出各k值对应的偏差
    END DO
    OPEN (99, ACCESS='append', file='moments.out') !将k阶矩均值存入文件
    WRITE (99, *) S
    CLOSE(99)
    OPEN (3, ACCESS='append', file='difference.out') !将偏差存入文件
    WRITE (3, *) D
    CLOSE(3)
END SUBROUTINE Moment

SUBROUTINE Independence(P) !2维独立性检验子程序
    INTEGER(KIND=4) :: P, l = 1, i = 1
    REAL(KIND=8) :: ave = 0, coave = 0, sqrave = 0 !分别表示<x>,<x_i*x_{i+l}>,<x^2>
    REAL(KIND=8), DIMENSION(1:4) :: C
    REAL(KIND=8), DIMENSION(1:10**P) :: z
    OPEN (1, file='rand.out')
    READ (1, *) z
    CLOSE(1)
    ave = SUM(z) / SIZE(z) !求均值
    sqrave = SUM(z**2) / SIZE(z)
    DO l = 1, 4
        DO i = 1, 10**P - l
            coave = coave + z(i) * z(i + l)
        END DO
        DO i = 1, l !在这里将越界的z(i+l)替换为z(N+i-l)
            coave = coave + z(i) * z(10**P + i - l)
        END DO
        coave = coave / SIZE(z)
        C(l) = ABS((coave - ave**2)/(sqrave - ave**2)) !相关系数公式
    END DO
    OPEN (99, ACCESS='append', file='indep.out')
    WRITE (99, *) C !将相关系数值存入文件
    CLOSE (99)
END SUBROUTINE Independence

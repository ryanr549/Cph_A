PROGRAM MAIN
    INTEGER(KIND=4) :: intI
    CHARACTER(LEN=1) :: charI
    DO intI = 3, 5
        WRITE (charI,"(I1)") intI !此语句将10以内的整型数据intI变为字符类型charI
        CALL Schrage(intI, 1651661, 'x'//charI//'.dat')
        CALL Schrage(intI, 7459556, 'y'//charI//'.dat')
        CALL Sphererd(intI) !使用前面生成的随机数进行抽样
    END DO   
END PROGRAM MAIN
   


SUBROUTINE Sphererd(intP)
   INTEGER(KIND=4) intP, i
   CHARACTER(LEN=1) charP
   REAL(KIND=8), DIMENSION(10**intP) :: theta, phi, xi, eta
   REAL(KIND=8), PARAMETER :: PI = 3.1415926
   WRITE (charP,"(I1)") intP !将整型数据intP转为字符型数据charP
   OPEN (1, file='x'//charP//'.dat') !打开相应文件传入两组随机数序列
   READ (1, *) xi
   CLOSE (1)
   OPEN (1, file='y'//charP//'.dat')
   READ (1, *) eta
   CLOSE (1)
   DO i = 1, 10**intP
       theta(i) = ACOS(xi(i)) !按照公式进行直接抽样
       phi(i) = 2*PI*eta(i)
   END DO
   OPEN (1, file='theta'//charP//'.dat') !将不同大小的球上随机数存入相应文件
   WRITE (1, *) theta
   CLOSE (1)
   OPEN (1, file='phi'//charP//'.dat')
   WRITE (1, *) phi
   CLOSE (1)
END SUBROUTINE Sphererd

SUBROUTINE Schrage(P, z0, filename) !Schrage随机数生成器子程序
   IMPLICIT NONE
   INTEGER :: N = 1, P
   INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(10**P), z0
   REAL(KIND=8) :: z(10**P)
   CHARACTER(LEN=40) :: filename
   In(1) = z0 !将传入值z0作为种子
   z(1) = REAL(In(1))/m
   DO N = 1, 10**P - 1
      In(N + 1) = a*MOD(In(N), q) - r*INT(In(N)/q)
      IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
         In(N + 1) = In(N + 1) + m
      END IF
      z(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
   END DO
   OPEN (1, file=trim(filename)) !每次运行子程序按照传入参数filename生成数据文件
   DO N = 1, 10**P !将随机数按行存入文件
      WRITE (1, *) z(N)
   END DO
   CLOSE (1)
END SUBROUTINE Schrage

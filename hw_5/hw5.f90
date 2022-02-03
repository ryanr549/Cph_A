PROGRAM MAIN
    INTEGER(KIND=4) :: intI
    CHARACTER(LEN=1) :: charI
    DO intI = 3, 5
        WRITE (charI,"(I1)") intI !此语句将10以内的整型数据intI变为字符类型charI
        CALL Schrage(intI, 98456523,'u'//charI//'.dat')
        CALL Schrage(intI, 1654125, 'v'//charI//'.dat')
        CALL Marsaglia(intI) !由Marsaglia抽样方法从u,v抽出随机数点
    END DO
END PROGRAM MAIN

SUBROUTINE Marsaglia(intP)
    INTEGER(KIND=4) :: intP, i = 1, num
    CHARACTER(LEN=1) charP
    REAL(KIND=8), DIMENSION(10**intP) :: u, v
    REAL(KIND=8), DIMENSION(10**intP, 3) :: r !存放随机数点坐标的二维数组
    WRITE (charP,"(I1)") intP !将整型数据intP转为字符型数据charP
    OPEN (1, file='u'//charP//'.dat') !打开相应文件传入两组随机数序列
    READ (1, *) u
    CLOSE (1)
    OPEN (1, file='v'//charP//'.dat')
    READ (1, *) v
    CLOSE (1)
    u = 2 * u - 1
    v = 2 * v - 1
    num = 0 !初始化抽出点的个数
    DO i = 1, 10**intP !抽样的总点数
        IF (u(i)**2 + v(i)**2 <= 1) THEN !当u^2+v^2<1时抽样
            num = num + 1 !抽出点后总抽出点数num递增
            r(num, 1) = 2 * u(i) * SQRT(1 - u(i)**2 - v(i)**2)
            r(num, 2) = 2 * v(i) * SQRT(1 - u(i)**2 - v(i)**2)
            r(num, 3) = 1 - 2 * (u(i)**2 + v(i)**2)
        ENDIF
    END DO
    OPEN (1, file='x'//charP//'.dat') !将x,y,z坐标分别存入相应文件
    DO i = 1, num
        WRITE (1, *) r(i, 1)
    END DO
    CLOSE (1)
    OPEN (1, file='y'//charP//'.dat') 
    DO i = 1, num
        WRITE (1, *) r(i, 2)
    END DO
    OPEN (1, file='z'//charP//'.dat') 
    DO i = 1, num
        WRITE (1, *) r(i, 3)
    END DO
END SUBROUTINE Marsaglia

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

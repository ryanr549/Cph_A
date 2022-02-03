MODULE DLA
IMPLICIT NONE
REAL(KIND=8) :: pi = 3.14159265358979
CONTAINS
SUBROUTINE Generator(particles, steps, r0, rmax, resc, filename) ! 生成DLA图形的子程序
    INTEGER(KIND=4), INTENT(IN) :: particles, steps, r0, rmax, resc
    INTEGER(KIND=4) :: i, j, dir, r(2), pos(0:particles, 2), recorded
    INTEGER(KIND=4), DIMENSION(-resc:resc, -resc:resc) :: lattice
    REAL(KIND=8) :: seed, rand0(particles), rand(steps)
    CHARACTER(LEN=*) :: filename

    CALL RANDOM_NUMBER(seed)
    CALL Schrage(particles, INT(2147483647 * seed), rand0)
    lattice = 0 ! 用来记录坐标对应网格的占有情况
    lattice(-1:1, -1:1) = 1 ! 设置原点初始占据
    pos(0, :) = [0, 0] ! 记录DLA图形上像素点的坐标
    recorded = 1
    DO i = 1, particles
        r(1) = INT(r0 * COS(2 * pi * rand0(i)))
        r(2) = INT(r0 * SIN(2 * pi * rand0(i)))
        ! 用一个随机数产生粒子开始随机行走的初始点

        CALL RANDOM_NUMBER(seed)
        CALL Schrage(steps, INT(2147483647 * seed), rand)
        ! 产生用于判断粒子运动方向的随机数序列
        DO j = 1, steps
            dir = INT(4 * rand(j)) ! 设dir=0,1,2,3分别对应左,下,右,上
            r = r + walk(dir) ! 粒子随机行走一步
            IF (ABS(r(1)) > resc .OR. ABS(r(2)) > resc) THEN
                EXIT ! 若大于逃逸距离则停止计算
            ENDIF
            IF (SUM(lattice((r(1) - 1):(r(1) + 1), (r(2) - 1):(r(2) + 1))) .NE. 0) THEN
                IF(ABS(r(1)) <= rmax .AND. ABS(r(2)) <= rmax) THEN
                    ! 如果点在正方形范围(-rmax:rmax, -rmax:rmax)内则记录
                    lattice(r(1), r(2)) = 1 
                    ! 若粒子附近的八个点不是完全空的，则停止随机扩散使其成为核心的一部分
                    pos(recorded, 1) = r(1)
                    pos(recorded, 2) = r(2)
                    print *, r(1), r(2), real(i) / particles * 100, '%'
                    ! 记录新添加到DLA图形上像素点的坐标
                    recorded = recorded + 1 ! 添加到DLA图形上的像素数+1
                END IF
                EXIT
            END IF
        END DO
    END DO
    OPEN (1, file=filename)
    DO i = 1, recorded
        WRITE (1, *) pos(i, :)
    END DO
    CLOSE (1)
    OPEN (1, file='lattice.dat')
    WRITE (1, *) ((lattice(i, j), j = -resc, resc), i = -resc, resc)
    CLOSE (1)
END SUBROUTINE Generator  

SUBROUTINE Sandbox(resc, rmax, filename)
    CHARACTER(LEN=*) :: filename
    INTEGER(KIND=4),INTENT(IN) :: resc, rmax
    INTEGER(KIND=4) :: i, j, k, N(2:8), r(2:8)
    INTEGER(KIND=4) :: lattice(-resc:resc, -resc:resc), mesh(-rmax:rmax, -rmax:rmax)
    ! 读取Generator子程序中生成的格点占据矩阵
    OPEN (1, file='lattice.dat')
    READ (1, *) ((lattice(i, j), j = -resc, resc), i = -resc, resc)
    CLOSE (1)
    mesh = lattice(-rmax:rmax, -rmax:rmax) ! 取画出图像部分的像素点占据情况
    N = 0
    DO k = 2, 8
        r(k) = 2**k
        DO i = -r(k)+1, r(k)
            DO j = -r(k)+1, r(k)
                IF(lattice(i, j) .EQ. 1) THEN
                    N(k) = N(k) + 1 ! 当lattice值为1表示存在一个像素，N值累加
                END IF
            END DO
        END DO
    END DO
    OPEN (1, file=filename)
    DO k = 2, 8
        WRITE (1, *) r(k), N(k)
    END DO
    CLOSE (1)
END SUBROUTINE Sandbox

SUBROUTINE Box(resc, rmax, filename) ! 盒子计数法求分维子程序
    CHARACTER(LEN=*) :: filename
    INTEGER(KIND=4), INTENT(IN) :: resc, rmax
    INTEGER(KIND=4) :: i, j, k, N(0:7), eps(0:7)
    INTEGER(KIND=4) :: lattice(-resc:resc, -resc:resc), mesh(-rmax+1:rmax, -rmax+1:rmax)
    ! 读取Generator子程序中生成的格点占据矩阵
    OPEN (1, file='lattice.dat')
    READ (1, *) ((lattice(i, j), j = -resc, resc), i = -resc, resc)
    CLOSE (1)
    mesh = lattice(-rmax+1:rmax, -rmax+1:rmax) ! 取画出图像部分的像素点占据情况
    N = 0
    DO k = 0, 7
        eps(k) = 2**k
        DO i = -256 / eps(k), 256 / eps(k) 
            DO j = -256 / eps(k), 256 / eps(k)
                IF(SUM(lattice((i * eps(k) + 1):((i+1) * eps(k)), (j * eps(k) + 1):((j+1) * eps(k)))) .NE. 0) THEN
                    N(k) = N(k) + 1 ! 若网格中非空则盒子计数+1
                END IF
            END DO  
        END DO
    END DO
    OPEN (1, file=filename)
    DO k = 0, 7
        WRITE (1, *) eps(k), N(k)
    ENDDO
    CLOSE (1)
END SUBROUTINE Box

FUNCTION walk(direction) ! 将数字代号转换成位移坐标的函数
    INTEGER(KIND=4) :: direction
    INTEGER(KIND=4), DIMENSION(2) :: walk
    SELECTCASE(direction)
        CASE(0)
          walk = [-1, 0]
        CASE(1)
          walk = [0, -1]
        CASE(2)
          walk = [1, 0]
        CASE(3)
          walk = [0, 1]
    END SELECT
END FUNCTION walk
END MODULE DLA  


SUBROUTINE Schrage(num, z0, rand) 
    !Schrage随机数生成器子程序,将均匀随机数序列存放在数组rand中
    IMPLICIT NONE
    INTEGER(KIND=4) :: N = 1, num
    INTEGER :: m = 2147483647, a = 16807, q = 127773, r = 2836, In(num), z0
    REAL(KIND=8), INTENT(INOUT) :: rand(num)
    In(1) = z0 !将传入值z0作为种子
    rand(1) = REAL(In(1))/m
    DO N = 1, num - 1
        In(N + 1) = a * MOD(In(N), q) - r * INT(In(N) / q)
        IF (In(N + 1) < 0) THEN !若值小于零，按Schrage方法加m
            In(N + 1) = In(N + 1) + m
        END IF
        rand(N + 1) = REAL(In(N + 1))/m !得到第N+1个随机数
    END DO
END SUBROUTINE Schrage

PROGRAM MAIN
    USE DLA
    IMPLICIT NONE
    ! CALL Generator(100000, 500000, 400, 256, 450 ,'test.dat')
    CALL Box(450, 256, 'box.dat')
    ! CALL Sandbox(450, 256, 'sandbox.dat')
END PROGRAM MAIN

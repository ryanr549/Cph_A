MODULE RW
IMPLICIT NONE

CONTAINS
FUNCTION sqdr(steps) ! RW生成器函数，返回行走距离的平方sqdr
    INTEGER(KIND=4), INTENT(IN) :: steps
    INTEGER(KIND=4) :: i, j
    INTEGER(KIND=4) :: dir, lastdir, tmpr(2)
    INTEGER(KIND=4), DIMENSION(0:steps, 2) :: r
    INTEGER(KIND=4), DIMENSION(-steps:steps, -steps:steps) :: lattice
    REAL(KIND=8) :: sqdr
    LOGICAL(KIND=4) :: revive, reach
    REAL(KIND=8) :: seed, rand(steps), tmprand(50)

    reach = .FALSE.
    DO WHILE(reach .EQV. .FALSE.)
        CALL RANDOM_NUMBER(seed) !用FORTRAN自带的随机数生成器生成16807生成器的种子
        CALL Schrage(steps, int(2147483647 * seed), rand)
        r(0, :) = [0, 0]
        lattice = 0
        lattice(0, 0) = 1
        DO i = 1, steps
            IF(i .EQ. 1) THEN ! 第一步有4个可能方向
                dir = int(4 * rand(i)) ! 设dir=0,1,2,3分别对应向左,下,右,上
                r(i, :) = r(i-1, :) + walk(dir)
                lattice(r(i, 1), r(i, 2)) = 1 ! 将走过点对应的lattice值标记为1
                lastdir = dir
            ELSE ! 后面的步最多只能有3个可能方向
                dir = noturn(lastdir, int(3 * rand(i)))
                ! 从除了上一步行走方向反方向的3个选择中随机选择一个
                tmpr(:) = r(i-1, :) + walk(dir) ! tmpr表示粒子下一步可能前往的位置
                IF(lattice(tmpr(1), tmpr(2)) .EQ. 1) THEN
                    revive = .FALSE.
                    CALL RANDOM_NUMBER(seed) 
                    CALL Schrage(20, int(2147483647 * seed), tmprand)
                    ! 若抽中的方向将导致粒子死亡，再次抽样尽量避免这次死亡
                    DO j = 1, 50
                        dir = noturn(lastdir, int(3 * tmprand(j)))
                        tmpr(:) = r(i - 1, :) + walk(dir)
                        IF(lattice(tmpr(1), tmpr(2)) .NE. 1) THEN
                            r(i, :) = tmpr(:)
                            revive = .TRUE.
                            EXIT
                        END IF
                    END DO
                    IF(revive .EQV. .FALSE.) THEN
                        EXIT ! 如果粒子复活失败，则死亡
                    END IF
                ELSE
                    r(i, :) = tmpr(:)
                    lattice(r(i, 1), r(i, 2)) = 1
                END IF
                lastdir = dir
            END IF
            IF(i .EQ. steps) THEN
                reach = .TRUE.
            END IF
        END DO
    END DO
    sqdr = DOT_PRODUCT(r(steps, :), r(steps, :)) ! 求走过了指定步数RW距离的平方
END FUNCTION sqdr

FUNCTION Z(total, steps) ! 同为RW生成器函数，返回达到某指定步数粒子的比率
    INTEGER(KIND=4), INTENT(IN) :: total, steps
    INTEGER(KIND=4) :: successes, l, i, j
    INTEGER(KIND=4) :: dir, lastdir, tmpr(2)
    INTEGER(KIND=4), DIMENSION(0:steps, 2) :: r
    INTEGER(KIND=4), DIMENSION(-steps:steps, -steps:steps) :: lattice
    LOGICAL(KIND=4) :: revive
    REAL(KIND=8) :: seed, rand(steps), tmprand(50), Z

    successes = 0
    DO l = 1, total
        CALL RANDOM_NUMBER(seed) !用FORTRAN自带的随机数生成器生成16807生成器的种子
        CALL Schrage(steps, int(2147483647 * seed), rand)
        r(0, :) = [0, 0]
        lattice = 0
        lattice(0, 0) = 1
        DO i = 1, steps
            IF(i .EQ. 1) THEN ! 第一步有4个可能方向
                dir = int(4 * rand(i)) ! 设dir=0,1,2,3分别对应向左,下,右,上
                r(i, :) = r(i-1, :) + walk(dir)
                lattice(r(i, 1), r(i, 2)) = 1 ! 将走过点对应的lattice值标记为1
                lastdir = dir
            ELSE ! 后面的步最多只能有3个可能方向
                dir = noturn(lastdir, int(3 * rand(i)))
                ! 从除了上一步行走方向反方向的3个选择中随机选择一个
                tmpr(:) = r(i-1, :) + walk(dir) ! tmpr表示粒子下一步可能前往的位置
                IF(lattice(tmpr(1), tmpr(2)) .EQ. 1) THEN
                    revive = .FALSE.
                    CALL RANDOM_NUMBER(seed) 
                    CALL Schrage(20, int(2147483647 * seed), tmprand)
                    ! 若抽中的方向将导致粒子死亡，再次抽样尽量避免这次死亡
                    DO j = 1, 50
                        dir = noturn(lastdir, int(3 * tmprand(j)))
                        tmpr(:) = r(i - 1, :) + walk(dir)
                        IF(lattice(tmpr(1), tmpr(2)) .NE. 1) THEN
                            r(i, :) = tmpr(:)
                            revive = .TRUE.
                            EXIT
                        END IF
                    END DO
                    IF(revive .EQV. .FALSE.) THEN
                        EXIT ! 如果粒子复活失败，则死亡，进入下次循环
                    END IF
                ELSE
                    r(i, :) = tmpr(:)
                    lattice(r(i, 1), r(i, 2)) = 1
                END IF
                lastdir = dir
            END IF
            IF(i .EQ. steps) THEN
                successes = successes + 1 ! 成功走到最后则累计成功数successes
            END IF
        END DO
    END DO
    Z = real(successes) / total ! 计算成功比率
END FUNCTION Z

SUBROUTINE Getnu() ! 计算nu的子程序
    INTEGER(KIND=4) :: j, N, minim = 50, maxim = 200
    REAL(KIND=8) ,DIMENSION(50:200) :: nu
    REAL(KIND=8) :: big_ave = 0, small_ave = 0
    DO N = minim, maxim 
        print *, 'computing nu, ',N, 'steps / 200steps'
        DO j = 1, 300
            small_ave = small_ave - small_ave / j + sqdr(N-10) / j 
            big_ave = big_ave - big_ave / j + sqdr(N+10) / j 
            ! 分别累计r(N-i),r(N+i)的平均值，且防止溢出
        END DO
        nu(N) = 0.5 * LOG(big_ave / small_ave) / LOG(real(N+10) / real(N-10))
        ! 按照公式由双对数曲线斜率求指数nu(N)
    END DO
    OPEN (1, file='nu.dat') !将nu(N)数值写入文件
    WRITE (1, *) nu
    CLOSE (1)
    print *, 'Done with nu!'
    print *, '---------------------------------------------------------------'
END SUBROUTINE Getnu

SUBROUTINE Getgam() ! 计算指数gamma的子程序
    REAL(KIND=8), DIMENSION(50:200) :: gam
    INTEGER(KIND=4) :: N, minim = 50, maxim = 200

    DO N = minim, maxim
        print *, 'computing gamma, ', N, 'steps / 200steps'
        gam(N) = 1 + (real(N) / 10)**2 * LOG(Z(10000, N)**2 / &
                (Z(10000, N-10) * Z(10000, N+10)))
    END DO
    OPEN (1, file='gamma.dat')
    WRITE (1, *) gam
    CLOSE (1)
    print *, 'done with gamma!'
END SUBROUTINE Getgam

FUNCTION noturn(lastd, r) !不走回上一步的方向选择器
    INTEGER(KIND=4) :: r, lastd
    INTEGER(KIND=4) :: noturn
    SELECTCASE(r)
        CASE(0)
            noturn = MOD(MOD(lastd + 2, 4) + 1, 4)
        CASE(1)
            noturn = MOD(MOD(lastd + 2, 4) + 2, 4)
        CASE(2)
            noturn = MOD(MOD(lastd + 2, 4) + 3, 4)
    END SELECT
    ! 一个两个相反方向对应数字的关系为: A=MOD(B+2, 4)
END FUNCTION noturn
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

END MODULE RW

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
    USE RW
    IMPLICIT NONE
    CALL Getnu() ! 调用求指数nu的子程序
    CALL Getgam() ! 调用求指数gamma的子程序
END PROGRAM MAIN

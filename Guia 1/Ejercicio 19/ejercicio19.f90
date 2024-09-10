program lapack_example
    implicit none
    ! Declaraciones
    integer :: n, nrhs, lda, ldb, info, ipiv(2)
    real(8) :: A(2,2), B(2)

    ! Definir el sistema A * X = B
    A = reshape([2.0, 5.0, 1.0, 7.0], [2, 2])  ! Matriz A
    B = [11.0, 13.0]                            ! Vector B

    ! Parámetros de LAPACK
    n = 2       ! Orden de la matriz A
    nrhs = 1    ! Número de columnas en la matriz B
    lda = 2     ! Leading dimension de A
    ldb = 2     ! Leading dimension de B

    ! Llamada a la subrutina SGESV para resolver el sistema
    call dgesv(n, nrhs, A, lda, ipiv, B, ldb, info)

    ! Comprobar si el sistema fue resuelto con éxito
    if (info == 0) then
        print *, "El sistema fue resuelto. La solución es: "
        print *, "x1 =", B(1)
        print *, "x2 =", B(2)
    else
        print *, "Error en la resolución del sistema. Código de error:", info
    end if
end program lapack_example
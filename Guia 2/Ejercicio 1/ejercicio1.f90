program ejercicio1
    implicit none

    INTEGER :: cant_ec, max_iter
    REAL(8), DIMENSION(0:cant_ec) :: vi
    REAL(8) :: h
    INTEGER iter
    REAL(8), DIMENSION(0:cant_ec) :: v
    
    WRITE (*, '(I5, 2F10.6)') 0, vi

    v = vi
    DO iter = 1, max_iter
    v = v + h*v_prima(v)
    WRITE (*, '(I5, 2F10.6)') iter, v
    END DO

    
    contains
    function v_prima(v)
        ! Definición de la Ecuacion Diferencial
        REAL(8), DIMENSION(0:1) :: v, v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(0)*v(1)**(1/3.)

    END FUNCTION v_prima
end program ejercicio1
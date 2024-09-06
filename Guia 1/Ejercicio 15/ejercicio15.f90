program ejercicio15
    use ejercicio14
    implicit none
    integer N,opcion
    real(8), allocatable :: MATRIZ(:,:), VECTOR(:)
    call menu(opcion)
    do while (opcion/=7)
        select case(opcion)
        case (1)
            call cargarMatrizPorTeclado(MATRIZ,N)
        case (2)
            call imprimirMatriz(MATRIZ,N)
        case (3)
            call grabarMatriz(MATRIZ,N,"matriz.txt")
        case (4)
            call ingresarVectorPorTeclado(VECTOR,N)
        case (5)
            call imprimirVector(VECTOR,N)
        case (6)
            call grabarVector(VECTOR,N,"vector.txt")
        end select
        call menu(opcion)
    end do
    deallocate(VECTOR)
    deallocate(MATRIZ)
    
    contains
    subroutine menu(opcion)
        integer opcion
        print*,"Ingrese una opcion del menu"
        print*,"1 - Ingresar una matriz de nxn"
        print*,"2 - Imprimir por pantalla la matriz definida"
        print*,"3 - Grabar en archivo la matriz definida"
        print*,"4 - Ingresar un vector de n componentes"
        print*,"5 - Imprimir por pantalla el vector definido"
        print*,"6 - Grabar en archivo el vector definido"
        print*,"7 - Salir"
        read(*,'(I1)') opcion
        do while (opcion<1 .or. opcion>7)
            print*,"Ingrese una opcion valida"
            read(*,'(I1)') opcion
        end do
    end subroutine menu
end program ejercicio15
#include <stdio.h>
#define MAXELEM 3

void cargarSistema(double A[][MAXELEM], unsigned int *N, double B1[MAXELEM]);
void invertir(double A[][MAXELEM], unsigned int N, double inv[][MAXELEM]);
double normaM(double A[][MAXELEM], unsigned int N, unsigned int M);
double Cond(double A[][MAXELEM], unsigned int N);
void restar(double x1[], double x2[], unsigned int N, double xr[]);
double error(double x1[MAXELEM], double x2[MAXELEM], unsigned int N);

int main()
{
    double A[MAXELEM][MAXELEM], B1[MAXELEM], B2[] = {30, 42, 22}, condicion, x1[] = {1, 3, 2}, x2[] = {1, 4, 0};
    unsigned int N;

    cargarSistema(A, &N, B1);

    // Inciso a
    condicion = Cond(A, N);
    printf("a) Numero de la condicion de la matriz: %lf\n", condicion);

    // Inciso b
    printf("b) Error relativo: %lf\n", error(x1, x2, N));

    // Inciso c
    printf("c) Cota teorica del valor relativo: %lf\n", condicion * error(B1, B2, N));

    return 0;
}

void cargarSistema(double A[][MAXELEM], unsigned int *N, double B1[MAXELEM])
{
}
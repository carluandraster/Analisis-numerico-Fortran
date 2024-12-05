#include <stdio.h>
#define MAXELEM 5
#define MAXCAR 10

void cargarMatriz(double A[][MAXELEM], unsigned int *N, char archivo[MAXCAR]);
void generarMatrizId(double id[][MAXELEM], unsigned int N);
void invertir(double A[][MAXELEM], unsigned int N, double inv[][MAXELEM]);
void mostrarMatriz(double A[][MAXELEM], unsigned int N);

int main()
{
    double A[MAXELEM][MAXELEM], inv[MAXELEM][MAXELEM];
    unsigned int N;

    // Inciso a
    cargarMatriz(A, &N, "matriz1.txt");
    invertir(A, N, inv);
    printf("Inciso a: \n");
    mostrarMatriz(inv, N);

    // Inciso b
    cargarMatriz(A, &N, "matriz2.txt");
    invertir(A, N, inv);
    printf("Inciso b: \n");
    mostrarMatriz(inv, N);
    return 0;
}

void cargarMatriz(double A[][MAXELEM], unsigned int *N, char archivo[MAXCAR])
{
    FILE *arch;
    unsigned int i, j;

    arch = fopen(archivo, "rt");
    fscanf(arch, "%u", N);
    for (i = 0; i < *N; i++)
        for (j = 0; j < *N; j++)
            fscanf(arch, "%lf", &A[i][j]);
    fclose(arch);
}

void generarMatrizId(double id[][MAXELEM], unsigned int N)
{
    unsigned int i, j;
    for (i = 0; i < N; i++)
        for (j = 0; j < N; j++)
            id[i][j] = i == j;
}

void invertir(double A[][MAXELEM], unsigned int N, double inv[][MAXELEM])
{
    unsigned int t, fila, i;
    double diag, factor;

    generarMatrizId(inv, N);
    for (t = 0; t < N; t++)
    {
        // Normalizar fila t
        diag = A[t][t];
        for (i = 0; i < N; i++)
        {
            A[t][i] /= diag;
            inv[t][i] /= diag;
        }

        // Barrido
        for (fila = 0; fila < N; fila++)
        {
            if (fila != t)
            {
                factor = A[fila][t];
                for (i = 0; i < N; i++)
                {
                    inv[fila][i] -= factor * inv[t][i];
                    A[fila][i] -= factor * A[t][i];
                }
            }
        }
    }
}

void mostrarMatriz(double A[][MAXELEM], unsigned int N)
{
    unsigned int i, j;
    for (i = 0; i < N; i++)
    {
        for (j = 0; j < N; j++)
            printf("%f ", A[i][j]);
        printf("\n");
    }
}
#include <stdio.h>
#include <assert.h>

int main ()
{
    int row, totalnrow, dot, symbol;
    assert (scanf ("%d", &totalnrow) == 1);

        for (row = 1; row <= totalnrow; row++)
        {
            for (symbol = 1; symbol <= row; symbol++)
                printf("*");
            for (dot = 1; dot <= ((2 * totalnrow) - (2 * row)); dot++)
                printf(".");
            for (symbol = 1; symbol <= row; symbol++)
                printf("*");
            printf("\n");
        }

        for (row=totalnrow-1; row >=1; row--)
        {
            for (symbol=1; symbol <= row; symbol++ )
                printf ("*");
            for (dot=((2*totalnrow)-(2*row)); dot >=1 ; dot--)
                printf (".");
            for (symbol=1; symbol <= row; symbol++)
                printf ("*");
            printf ("\n");
        }
    return 0;
}
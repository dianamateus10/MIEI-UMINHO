#include <stdio.h>
#include <assert.h>

void triangulo ()
{
    int row, totalnrow, symbol, space;
    assert (scanf ("%d", &totalnrow) == 1);

    for (row=1; row <= totalnrow; row++)
    {
        for (space=1; space <= (totalnrow-row); space++)
            putchar (' ');
        for (symbol=1; symbol <= ((2*row)-1); symbol++)
            putchar ('#');
        putchar ('\n');
    }
}

int main ()
{
    int n;
    triangulo (n);
    return 0;
}
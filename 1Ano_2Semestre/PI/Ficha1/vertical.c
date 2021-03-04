#include <stdio.h>
#include <assert.h>

void triangulo ()
{
    int row, totalnrow, symbol, spaces;
    assert (scanf ("%d", &totalnrow) == 1);

    for (row=1; row <= totalnrow; row++)
    {
        for (symbol=1; symbol <= row; symbol++)
            printf ("#");
        for (spaces=1; spaces <= (totalnrow-row); spaces++)
            printf (" ");
        printf ("\n");
    }

    for (row=((2*totalnrow)-1); row > totalnrow ; row--)
    {
        for (symbol=(row-totalnrow); symbol >= 1; symbol--)
            printf ("#");
        for (spaces=1; spaces <= (totalnrow-row); spaces++)
            printf (" ");
        printf ("\n");
    }
}

int main ()
{
    int n;
    triangulo (n);
    return 0;
}

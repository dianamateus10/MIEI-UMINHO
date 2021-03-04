#include <stdio.h>

int circle (int r)
{
    int x, y, cardinais = 0;
    for (y=r; y >= (-r); y--)
    {
        for (x=(-r); x <= r; x++)
        {
            if (x*x+y*y <= r*r)
            {
                putchar ('#');
                cardinais++;
            }
            else putchar (' ');
        }
        putchar ('\n');
    }
    return cardinais;
}

int main ()
{
    int raio, cardinais;
    printf ("Escreva um raio\n");
    scanf ("%d", &raio);

    cardinais = circle (raio);

    printf ("Foram impressos %d cardinais\n", cardinais);
    return 0;
}


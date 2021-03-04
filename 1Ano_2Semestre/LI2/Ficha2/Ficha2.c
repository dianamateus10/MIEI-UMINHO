#include <stdio.h>
#include <math.h>
#include <assert.h>

const char *tipo (int x, int y, int z)
{
    if ((x*x)==((y*y)+(z*z))||(y*y)==((x*x)+(z*z))||(z*z)==((y*y)+(x*x)))
        return "RETANGULO";
    else if ((x==y) && (y==z))
        return "EQUILATERO";
    else if (x!=y && x!=z && y!=z)
        return "ESCALENO";
    else
        return "ISOSCELES";
}

int main ()
{
    int x, y, z, perimeter;
    float area, t;

    assert(scanf("%d %d %d", &x, &y, &z) == 3);

    perimeter = x + y + z;
    t = perimeter/(float) 2;
    area = sqrt(t*(t-x)*(t-y)*(t-z));

    if ((x+y<=z)||(x+z<=y)||(y+z<=x))
        printf ("INVALIDO\n");
    else
        printf ("%s %d %0.2f\n", tipo(x,y,z), perimeter, area);
    return 0;
}

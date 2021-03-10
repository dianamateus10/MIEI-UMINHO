#include <stdio.h>
#include <assert.h>

int main () {
    int list[100], matrix[10][10]={{0}};
    int counter=-1, number, row, column, tens, units, index, t;

    // stores the numbers in an array until the -1 is given
    do {
        counter++;
        assert (scanf("%d", &number) == 1);
        list[counter] = number;

    } while (list[counter] != -1);


    for (index=0; index < counter; index++)
    {
        tens = list[index]/10;
        units = list[index]%10;
        matrix[tens][units]++;
    }


    for (row=0; row < 10; row++)
    {
        printf ("%d|", row);
        for (column=0; column < 10; column++)
        {
            if (matrix[row][column] == 1)
            {
                printf ("%d", column);
            }
            else if (matrix[row][column] > 1)
            {
                    for (t=0; t < matrix[row][column]; t++)
                    {
                        printf ("%d", column);
                    }
            }
        }
        printf ("\n");
    }

    return 0;
}
